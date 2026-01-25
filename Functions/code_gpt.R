#' **Code open-ended survey responses using the OpenAI API**
#'
#' @description
#' This function submits open-ended survey responses to the OpenAI API and returns a dataset with the original responses, assigned codes, and bins from a provided code list.
#' 
#' @param data A data frame containing the survey data, or a path to a `.xlsx` or `.xls` file.
#' @param x The open-ended variable to be coded.
#' @param theme_list A data frame with at least two columns: `Code` and `Bin`.
#' @param id_var The respondent ID variable.
#' @param n Optional integer; number of responses to code. Defaults to all rows. Useful for testing coding quality without coding every response.
#' @param batch_size Integer; number of responses per API call. Defaults to `100`.
#' @param model Character string; the OpenAI model to use. Defaults to `gpt-4o`.
#' @param instructions Optional string; additional instructions for coding.
#'
#' @return
#' A table with respondent IDs, responses, codes, and bins.
#'
#' @export

code_gpt <- function(data, x, theme_list, id_var, n = NULL, batch_size = 100, model = "gpt-4o", instructions = NULL) {
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
  
  # Validate that x and id_var exist in dataset
  if (!(x %in% names(data))) stop("Variable ", x, " was not found in the dataset.", call. = FALSE)
  if (!(id_var %in% names(data))) stop("ID variable ", id_var, " was not found in the dataset.", call. = FALSE)
  
  # Identify question label
  q_label <- attr(data[[x]], "label", exact = TRUE)
  if (is.null(q_label)) q_label <- x
  
  # Build code list
  code_text <- paste0(
    "Available codes:\n",
    paste(
      purrr::pmap_chr(theme_list, function(Code, Bin, Description, ...) {
        if (!is.null(Description) && !is.na(Description) && trimws(Description) != "") {
          paste0(Code, ": ", Bin, " — ", Description)
        } else {
          paste0(Code, ": ", Bin)
        }
      }),
      collapse = "\n"
    )
  )
  
  # Limit to first n responses
  if (is.null(n)) {n <- nrow(data)}
  responses <- head(data[[x]], n)
  ids <- head(data[[id_var]], n)
  
  # Identify valid responses
  valid_idx <- which(!is.na(responses) & stringr::str_trim(responses) != "")
  valid_responses <- responses[valid_idx]
  
  # OpenAI API request ----
  
  batches <- split(seq_along(valid_responses), ceiling(seq_along(valid_responses) / batch_size))
  
  batch_results <- purrr::map(batches, function(idx) {
    these_resps <- valid_responses[idx]
    
    resp_text <- paste0(seq_along(these_resps), ". ", these_resps, collapse = "\n")
    instr_text <- if (!is.null(instructions)) paste0("\n\nAdditional instructions: ", instructions) else ""
    
    req <- httr2::request("https://api.openai.com/v1/chat/completions") %>%
      httr2::req_headers(
        "Authorization" = paste("Bearer", api_key),
        "Content-Type" = "application/json"
      ) %>%
      httr2::req_body_json(
        list(
          model = model,
          temperature = 0,
          seed = 123,
          messages = list(
            list(
              role = "system",
              content = paste(
                "You are a survey researcher coding open-ended responses based on the detailed code descriptions.",
                "Follow these rules strictly:",
                "1. Do not skip any rows — each response must receive at least one code.",
                "2. Only use numeric codes from the provided list; no text, symbols, or letters.",
                "3. If the same code number appears more than once for a response, list it only once.",
                "4. If multiple codes apply, separate them with commas.",
                "If a response is written in a language other than English, translate it into English first."
              )
            ),
            list(
              role = "user",
              content = paste(
                "Question label: ", q_label, "\n",
                code_text,
                "\n\nResponses:\n", resp_text,
                "\n\nReturn one line per response in the same order, in the format:\n1. codes\n2. codes\n3. codes\n...",
                instr_text
              )
            )
          )
        )
      ) %>%
      httr2::req_perform()
    
    result <- httr2::resp_body_json(req)
    raw_result <- result$choices[[1]]$message$content
    
    codes <- stringr::str_split(raw_result, "\n")[[1]]
    codes <- stringr::str_trim(codes)
    codes <- stringr::str_remove(codes, "^[0-9]+\\.\\s*")
    codes
  })
  
  coded_valid <- unlist(batch_results)
  
  # Insert NA back for invalid rows
  coded_raw <- rep(NA_character_, length(responses))
  coded_raw[valid_idx] <- coded_valid
  
  # Output ----
  
  # Add bins
  coded_bins <- purrr::map_chr(coded_raw, function(c) {
    if (is.na(c) || c == "") return(NA_character_)
    codes_vec <- stringr::str_split(c, ",")[[1]] %>% stringr::str_trim()
    bins_vec <- theme_list$Bin[match(as.numeric(codes_vec), theme_list$Code)]
    bins_vec <- bins_vec[!is.na(bins_vec)]
    paste(bins_vec, collapse = "; ")
  })
  
  # Create table
  result <- tibble::tibble(
    !!id_var := ids,
    !!q_label := responses,
    `Code(s)` = coded_raw,
    `Bin(s)` = coded_bins
  )
  
  # Clean up
  result <- result %>%
    dplyr::mutate(
      `Code(s)` = purrr::map_chr(`Code(s)`, function(x) {
        if (is.na(x) || trimws(x) == "") return(NA_character_)
        codes <- stringr::str_extract_all(x, "\\d+")[[1]] # Extract all numeric codes
        codes <- suppressWarnings(as.numeric(codes)) # Convert to numeric, remove NAs
        codes <- codes[!is.na(codes)]
        codes <- unique(codes) # Deduplicate
        codes <- sort(codes) # Sort
        paste(codes, collapse = ", ")
      })
    )
  
  invisible(result)
}
