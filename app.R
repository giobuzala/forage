library(shiny)

load_env <- function() {
  if (file.exists(".env")) {
    if (!requireNamespace("dotenv", quietly = TRUE)) {
      stop("Package 'dotenv' is required to load .env. Please install it with install.packages('dotenv').", call. = FALSE)
    }
    dotenv::load_dot_env(file = ".env")
  }
}

load_env()

source("Functions/code_gpt.R")
source("Functions/theme_gpt.R")

read_input_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "csv") return(readr::read_csv(path, show_col_types = FALSE))
  if (ext %in% c("xlsx", "xls")) return(readxl::read_excel(path))
  stop("Unsupported file type: .", ext, ". Please upload a .csv, .xlsx, or .xls file.", call. = FALSE)
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif; color: #1f2933; }
      .container { max-width: 720px; margin: 40px auto; }
      .subtle { color: #6b7280; font-size: 0.95rem; }
      .spacer { height: 8px; }
      .btn-primary { background-color: #111827; border-color: #111827; }
      .btn-primary:hover { background-color: #0b1220; }
      .panel { border: 1px solid #e5e7eb; padding: 16px; border-radius: 10px; margin-top: 12px; }
    "))
  ),
  div(
    class = "container",
    h2("forage"),
    p(class = "subtle", "Upload a survey file. Optionally upload a theme list or let the app generate themes."),
    p(class = "subtle", "Make sure your OpenAI API key is set in your R environment."),
    div(
      class = "panel",
      fileInput("data_file", "Survey data (.csv, .xlsx)", accept = c(".csv", ".xlsx", ".xls")),
      uiOutput("column_selectors")
    ),
    div(
      class = "panel",
      fileInput("theme_file", "Theme list (.csv, .xlsx) — optional", accept = c(".csv", ".xlsx", ".xls")),
      numericInput("n_themes", "Number of themes to generate", value = 10, min = 3),
      numericInput("sample_size", "Sample size for theme generation (optional)", value = 0, min = 0),
      downloadButton("theme_template", "Download theme template"),
      div(class = "spacer"),
      tags$small(class = "subtle", "Theme list requires columns: Code, Bin, Description (optional).")
    ),
    div(
      class = "panel",
      actionButton("run", "Code responses", class = "btn-primary"),
      div(class = "spacer"),
      tags$small(class = "subtle", "Output is Excel (.xlsx). If themes are generated, they are included on a second sheet."),
      downloadButton("download", "Download coded file")
    ),
    div(class = "panel", verbatimTextOutput("status"))
  )
)

server <- function(input, output, session) {
  coded_data <- reactiveVal(NULL)
  theme_used <- reactiveVal(NULL)
  status_text <- reactiveVal("Waiting for files.")
  
  data_df <- reactive({
    req(input$data_file)
    read_input_file(input$data_file$datapath)
  })
  
  theme_df <- reactive({
    if (is.null(input$theme_file)) return(NULL)
    read_input_file(input$theme_file$datapath)
  })
  
  output$column_selectors <- renderUI({
    req(data_df())
    cols <- names(data_df())
    tagList(
      selectInput("id_col", "ID column", choices = cols),
      selectInput("response_col", "Open-ended response column", choices = cols)
    )
  })
  
  observeEvent(input$run, {
    req(data_df(), input$id_col, input$response_col)
    status_text("Working...")
    result <- tryCatch(
      {
        theme_list <- theme_df()
        if (is.null(theme_list)) {
          sample_n <- if (is.null(input$sample_size) || input$sample_size <= 0) NULL else input$sample_size
          theme_list <- theme_gpt( # nolint
            data = data_df(),
            x = input$response_col,
            n = input$n_themes,
            sample = sample_n
          )
        } else {
          if (!all(c("Code", "Bin") %in% names(theme_list))) {
            stop("Theme list must include columns named 'Code' and 'Bin'.", call. = FALSE)
          }
        }
        theme_used(theme_list)
        code_gpt( # nolint
          data = data_df(),
          x = input$response_col,
          id_var = input$id_col,
          theme_list = theme_list
        )
      },
      error = function(e) {
        status_text(e$message)
        NULL
      }
    )
    if (!is.null(result)) {
      coded_data(result)
      status_text("Done. Download your coded file below.")
    }
  })
  
  output$status <- renderText({
    status_text()
  })
  
  output$theme_template <- downloadHandler(
    filename = function() "theme_template.csv",
    content = function(file) {
      template <- tibble::tibble(
        Code = c(1, 2, 3, 97, 98, 99),
        Bin = c("Theme A", "Theme B", "Theme C", "Other", "None", "Don't know"),
        Description = c(
          "This theme captures ...",
          "This theme captures ...",
          "This theme captures ...",
          "Response does not fit into any existing categories or represents a unique situation not captured by other codes.",
          "Response is irrelevant, nonsensical, or provides no meaningful information (e.g., gibberish, off-topic, or empty text).",
          "Respondent expresses uncertainty, confusion, or lack of an opinion or knowledge about the topic."
        )
      )
      readr::write_csv(template, file)
    }
  )
  
  output$download <- downloadHandler(
    filename = function() "coded_responses.xlsx",
    content = function(file) {
      req(coded_data())
      themes <- theme_used()
      if (is.null(themes)) {
        writexl::write_xlsx(list("Coded responses" = coded_data()), file)
      } else {
        writexl::write_xlsx(
          list(
            "Coded responses" = coded_data(),
            "Theme list" = themes
          ),
          file
        )
      }
    }
  )
}

shinyApp(ui, server)
