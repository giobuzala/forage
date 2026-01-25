#' **Generate a thematic code list using the OpenAI API**
#'
#' @description
#' This function analyzes open-ended survey responses and automatically generates a set of thematic codes with descriptions.
#'
#' @param data A data frame containing the survey data, or a path to a `.xlsx` or `.xls` file.
#' @param x The open-ended variable to analyze.
#' @param n Integer; number of themes to return. If `NULL` (default), the model determines an appropriate number of themes based on the responses.
#' @param sample Optional integer specifying the number of responses to sample for analysis. If `NULL`, all valid responses are used.
#' @param model Character string; the OpenAI model to use. Defaults to `gpt-4o`.
#' @param instructions Optional string; additional instructions for coding.
#'
#' @details
#' The output is a tibble with three columns:
#' - `Code`: A unique numeric code for each theme (standard codes 97–99 are added automatically).
#' - `Bin`: Short label for the theme, written in sentence case.
#' - `Description`: A one-sentence summary describing the theme's content.
#'
#' Standard codes are included automatically:
#' - `97` = `Other`
#' - `98` = `None`
#' - `99` = `Don't know`
#' 
#' Use this function to create a `theme_list` for input into `code_gpt()`.
#'
#' **Note:** It’s best to review and refine the generated codes before using them in `code_gpt()`.
#'
#' @return
#' A table containing the generated thematic code list and their description. Standard codes (`Other`, `None`, `Don't know`) are included automatically.
#' 
#' @export

theme_gpt <- function(data, x, n = NULL, sample = NULL, model = "gpt-4o", instructions = NULL) {
  # Check required packages ----
  
  required_pkgs <- c("tibble", "dplyr", "readxl", "purrr", "stringr", "httr2")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop("These packages are required but not installed: ",
         paste(missing_pkgs, collapse = ", "), call. = FALSE)
  }
  
  # Set up ----
  
  `%>%` <- dplyr::`%>%`
  
  # Check API key
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (is.null(api_key) || api_key == "") {
    stop(
      "OpenAI API key is not set.\n\n",
      "Please generate one at https://platform.openai.com/ and set it using either of the following methods:\n\n",
      "1. Temporarily (for this session):\n\n",
      "   Sys.setenv(OPENAI_API_KEY='your_api_key')\n\n",
      "2. Permanently:\n\n",
      "   Add the following line to your .Renviron file:\n",
      "   OPENAI_API_KEY='your_api_key'\n\n",
      "   You can locate your .Renviron file with:\n",
      "   file.path(Sys.getenv('HOME'), '.Renviron')\n",
      call. = FALSE
    )
  }
  
  # Load data (data frame or file path)
  read_input_data <- function(data) {
    if (is.data.frame(data)) return(data)
    if (is.character(data) && length(data) == 1) {
      ext <- tolower(tools::file_ext(data))
      if (ext %in% c("xlsx", "xls")) return(readxl::read_excel(data))
      stop("Unsupported file type: .", ext, ". Please upload a .xlsx or .xls file.", call. = FALSE)
    }
    stop("`data` must be a data frame or a file path.", call. = FALSE)
  }
  
  data <- read_input_data(data)
  
  # Validate that x exists in dataset
  if (!(x %in% names(data))) stop("Variable ", x, " was not found in the dataset.", call. = FALSE)
  
  # Identify question label
  q_label <- attr(data[[x]], "label", exact = TRUE)
  if (is.null(q_label)) q_label <- x
  
  # Extract valid responses
  responses <- data[[x]]
  responses <- responses[!is.na(responses) & stringr::str_trim(responses) != ""]
  if (length(responses) == 0) stop("No valid responses found in variable ", x, ".", call. = FALSE)
  
  # Determine sample size
  if (is.null(sample) || sample >= length(responses)) {
    sample_resps <- responses
    sample_n <- length(responses)
  } else {
    sample_resps <- sample(responses, sample)
    sample_n <- sample
  }
  
  sample_text <- paste0(seq_along(sample_resps), ". ", sample_resps, collapse = "\n")
  
  # Theme count instruction
  if (is.null(n)) {
    n_text <- "an appropriate set of"
  } else {
    n_text <- paste(n)
  }
  
  # OpenAI API request ----
  
  req <- httr2::request("https://api.openai.com/v1/chat/completions") %>%
    httr2::req_headers(
      "Authorization" = paste("Bearer", api_key),
      "Content-Type" = "application/json"
    ) %>%
    httr2::req_body_json(list(
      model = model,
      temperature = 0,
      seed = 123,
      messages = list(
        list(
          role = "system",
          content = paste(
            "You are a survey researcher designing thematic code frames for qualitative survey data.",
            "You will be shown real open-ended responses. Identify recurring ideas and group them into clear, distinct themes.",
            "Each theme should have a short, descriptive name in *sentence case* (e.g., 'Situational or environmental constraints').",
            "Each description must begin with 'This theme...' and be one concise sentence describing what the theme includes.",
            "Return the output in plain CSV-style text with two columns: Bin, Description.",
            "Do not number or bullet the themes."
          )
        ),
        list(
          role = "user",
          content = paste0(
            "Here are ", sample_n, " example responses to the survey question: '", q_label, "'.\n\n",
            sample_text,
            "\n\nBased on these, create ", n_text,
            " thematic codes with one-sentence descriptions.\nFormat as:\nBin, Description",
            if (!is.null(instructions)) paste0("\n\nAdditional instructions: ", instructions) else ""
          )
        )
      )
    )) %>%
    httr2::req_perform()
  
  result <- httr2::resp_body_json(req)
  text <- result$choices[[1]]$message$content
  
  # Output ----
  
  # Parse GPT output
  lines <- stringr::str_split(text, "\n")[[1]] %>% stringr::str_trim()
  lines <- lines[lines != ""]
  
  df <- purrr::map_dfr(lines, function(line) {
    parts <- strsplit(line, ",", fixed = TRUE)[[1]]
    if (length(parts) >= 2) {
      tibble::tibble(
        Bin = stringr::str_trim(parts[1]),
        Description = stringr::str_trim(paste(parts[-1], collapse = ","))
      )
    } else {
      tibble::tibble(Bin = parts[1], Description = NA_character_)
    }
  })
  
  # Add codes
  df <- df %>%
    dplyr::filter(!(Bin %in% c("Bin", "Description") | Description %in% c("Bin", "Description"))) %>% # Drop accidental header-like row
    dplyr::mutate(Code = dplyr::row_number()) %>%
    dplyr::select(Code, Bin, Description)
  
  # Prepend standard codes
  standard <- tibble::tibble(
    Code = c(97, 98, 99),
    Bin = c("Other", "None", "Don't know"),
    Description = c(
      "Response does not fit into any existing categories or represents a unique situation not captured by other codes.",
      "Response is irrelevant, nonsensical, or provides no meaningful information (e.g., gibberish, off-topic, or empty text).",
      "Respondent expresses uncertainty, confusion, or lack of an opinion or knowledge about the topic."
    )
  )
  
  result <- dplyr::bind_rows(standard, df) %>%
    dplyr::mutate(Code = as.integer(Code)) %>%
    dplyr::relocate(Code, Bin, Description)
  
  invisible(result)
}
