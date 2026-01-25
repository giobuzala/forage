# Libraries ----

library(shiny)
library(shinyjs)
library(dplyr)
library(openxlsx)


# Environment ----

# Loads API keys or other secrets from .env if present
load_env <- function() {
  if (file.exists(".env")) {
    if (!requireNamespace("dotenv", quietly = TRUE)) {
      stop(
        "Package 'dotenv' is required to load .env. Please install it with install.packages('dotenv').",
        call. = FALSE
      )
    }
    dotenv::load_dot_env(file = ".env")
  }
}

load_env()

# Load functions
source("Functions/code_gpt.R")
source("Functions/theme_gpt.R")


# Helpers ----

# Reads uploaded Excel files
read_input_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls")) {
    return(readxl::read_excel(path))
  }
  stop(
    "Unsupported file type: .", ext,
    ". Please upload a .xlsx or .xls file.",
    call. = FALSE
  )
}

# Parses optional number-of-themes input
parse_n_themes <- function(x) {
  if (is.null(x)) return(NULL)
  x <- trimws(x)
  if (x == "") return(NULL)
  n <- suppressWarnings(as.integer(x))
  if (is.na(n) || n <= 0) return(NULL)
  n
}


# UI/UX ----

ui <- fluidPage(
  
  tags$head(
    
  # favicon ----
    
  tags$link(rel = "icon", href = "favicon.ico"),
  tags$link(rel = "shortcut icon", href = "favicon.ico"),
    
  # CSS styling ----
    
  tags$style(HTML("

      /* =====================================================
         BASE DOCUMENT STYLES
         ===================================================== */

      /* Establish base font size so rem units scale predictably */
      html {
        font-size: 16px;
      }

      /* Global body typography and color */
      body {
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
        color: #1f2933;
        font-size: 1rem;
      }

      /* =====================================================
         MAIN PAGE CONTAINER
         ===================================================== */

      /* Centers app and limits line length for readability */
      .container {
        max-width: 760px;
        margin: 56px auto;
      }

      /* =====================================================
         HEADINGS & TITLES
         ===================================================== */

      /* App title */
      h2 {
        font-size: 3rem;
        font-weight: 700;
        letter-spacing: -0.02em;
        margin-bottom: 14px;
      }

      /* Section headings (Step 1 / 2 / 3 titles) */
      h4 {
        font-size: 1.55rem;
        font-weight: 600;
        margin-bottom: 20px;
      }

      /* Bold step number preceding section titles */
      .step-num {
        font-size: 1.55rem;
        font-weight: 600;
        margin-right: 5px;
      }

      /* =====================================================
         TEXT / COPY STYLES
         ===================================================== */

      /* Main tagline under app title */
      .lead {
        font-size: 1.25rem;
        color: #4b5563;
        margin-bottom: 24px;
      }

      /* Helper / instructional copy */
      .subtle {
        font-size: 0.95rem;
        color: #6b7280;
        line-height: 1.6;
      }

      /* =====================================================
         PANEL CONTAINERS
         ===================================================== */

      /* Shared styling for all step panels */
      .panel {
        border: 1px solid #6b7280;
        padding: 18px 26px;
        border-radius: 14px;
        margin-top: 16px;
        background: #ffffff;
      }

      /* Adds vertical spacing between stacked inputs (ID + response selects, etc.) */
      .panel .shiny-input-container + .shiny-input-container {
        margin-top: 14px;
      }

      /* Adds breathing room after the final input in a group
         (e.g., below Open-ended response column selector) */
      .panel .shiny-input-container:last-of-type {
        margin-bottom: 18px;
      }

      /* Simple vertical spacer utility */
      .spacer {
        height: 15px;
      }

      /* Normalizes spacing added by Bootstrap input groups */
      .panel .input-group {
        margin-bottom: 16px;
      }

      /* Completely hides Shiny's file upload progress bar
         (prevents layout jumping after file selection) */
      .shiny-file-input-progress {
        display: none !important;
        height: 0 !important;
        margin: 0 !important;
        padding: 0 !important;
      }

      /* =====================================================
         FORM ELEMENTS
         ===================================================== */

      /* Input labels */
      label {
        display: block;
        font-size: 1.05rem;
        font-weight: 500;
        margin-top: 20px;
        margin-bottom: 10px;
      }

      /* Text inputs & dropdowns */
      .shiny-input-container input,
      .shiny-input-container select {
        font-size: 0.95rem;
        padding: 10px 14px;
        height: auto;
      }

      /* File input text sizing */
      input[type='file'] {
        font-size: 1rem;
      }

      /* Removes Bootstrap's default bottom margin under form groups
         (spacing is handled manually above) */
      .panel .form-group {
        margin-bottom: 0;
      }

      /* =====================================================
         BUTTONS
         ===================================================== */

      /* Base button styling */
      .btn {
        font-size: 0.95rem;
        font-weight: 600;
        padding: 10px 22px;
      }

      /* Primary action buttons */
      .btn-primary,
      .btn-primary:hover,
      .btn-primary:focus,
      .btn-primary:active {
        background-color: #1f2933;
        border-color: #1f2933;
        outline: none !important;
        box-shadow: none !important;
      }
      
      .btn-primary:focus-visible {
        outline: none !important;
        box-shadow: none !important;
      }

      /* Download buttons */
      .btn-download {
        background-color: #065f46;
        border-color: #065f46;
        color: #ffffff;
      }

      /* Disabled button appearance */
      .btn:disabled,
      .btn.disabled {
        background-color: #e5e7eb;
        border-color: #e5e7eb;
        color: #9ca3af;
        cursor: not-allowed;
        opacity: 1;
      }

      /* Prevent hover/focus effects on disabled buttons */
      .btn:disabled:hover,
      .btn.disabled:hover,
      .btn:disabled:focus,
      .btn.disabled:focus {
        background-color: #e5e7eb;
        border-color: #e5e7eb;
        color: #9ca3af;
        box-shadow: none;
      }

      /* =====================================================
         STATUS / NOTICE BANNER
         ===================================================== */

      /* Status indicator shown under workflow overview */
      .notice {
        font-size: 0.95rem;
        font-weight: 500;
        background: #f9fafb;
        border: 1px solid #4b5563;
        padding: 12px 18px;
        border-radius: 14px;
        margin-top: 20px;
        display: inline-block;
      }

      /* =====================================================
         INSTRUCTION LISTS
         ===================================================== */

      /* Ordered list spacing inside panels */
      .panel ol {
        margin: 4px 0;
        padding-left: 22px;
      }
      
      /* =====================================================
         INFORMATIONAL PANELS (Instructions & Disclaimer)
         ===================================================== */

      /* Base styling for non-interactive informational panels */
      .panel-info {
        background: #f9fafb;
        border-color: #d1d5db;
      }

      /* Headings in info panels */
      .panel-info h5 {
        font-size: 1.2rem;
        font-weight: 600;
        margin-bottom: 8px;
      }

      /* Text in info panels */
      .panel-info p,
      .panel-info li {
        font-size: 0.95rem;
        color: #4b5563;
        line-height: 1.6;
        margin-bottom: 8px;
      }

      /* Tighter top margin for lists */
      .panel-info ol {
      margin-top: 8px;
      }
      
      /* =====================================================
         CREDITS / FOOTER
         ===================================================== */

      /* Container for author / attribution text */
      .credits {
        text-align: center;
        font-size: 0.9rem;
        color: #4b5563;
        margin-top: 32px;
      }

      /* Links inside the credits section */
      .credits a {
        color: #4b5563;
        font-weight: 500;
        text-decoration: none;
      }

      /* Hover state for credit links */
      .credits a:hover {
        text-decoration: underline;
      }

    "))
  ),
  
  # Main content container ----
  
  div(
    class = "container",
    h2("forage"),
    p(class = "lead", "AI-assisted open-ended coding agent"),
    
    # Instructions
    
    div(
      class = "panel panel-info",
      h5("How it works"),
      tags$ol(
        tags$li(strong("Upload your survey file:"), " Select the file containing your open-ended survey responses."),
        tags$li(
          strong("Generate a theme list:"),
          " The app will analyze responses and generate themes with clear labels and descriptions.
          You can download the list for review or proceed to the next step using it as generated."
        ),
        tags$li(
          strong("Code responses and download results:"),
          " If you are not using the theme list generated in the previous step, upload your own theme list.
          Each response will be assigned one or more themes, and you can download the coded file."
        )
      )
    ),
    
    # Step 1
    
    div(
      class = "panel",
      h4(tags$span("Step 1.", class = "step-num"), "Upload survey data"),
      tags$small(class = "subtle", "Upload an Excel file that contains a respondent ID column and an open-ended response column."),
      fileInput("data_file", "Survey file", accept = c(".xlsx", ".xls")),
      uiOutput("column_selectors")
    ),
    
    # Step 2
    
    div(
      class = "panel",
      h4(tags$span("Step 2.", class = "step-num"), "Generate themes"),
      p(class = "subtle", "Generate a theme list from your open-ended responses."),
      textInput("n_themes", "Number of themes", value = ""),
      div(class = "spacer"),
      tags$small(class = "subtle", "Leave blank to let the app decide the number of themes."),
      div(class = "spacer"),
      actionButton("run_themes", "Generate theme list", class = "btn-primary"),
      div(class = "spacer"),
      uiOutput("download_themes_ui")
    ),
    
    # Step 3
    
    div(
      class = "panel",
      h4(tags$span("Step 3.", class = "step-num"), "Code responses"),
      p(
        class = "subtle",
        "Use the generated themes or upload an existing theme list.
        As a best practice, review generated themes before coding."
      ),
      p(class = "subtle", "Results are returned as an Excel file with coded responses and a theme list."),
      fileInput("theme_file", "Theme list", accept = c(".xlsx", ".xls")),
      tags$small(class = "subtle", "Leave empty to use the generated theme list."),
      div(class = "spacer"),
      actionButton("run", "Code responses", class = "btn-primary"),
      div(class = "spacer"),
      uiOutput("download_coded_ui")
    ),
    
    # Disclaimer
    
    div(
      class = "panel panel-info",
      h5("About AI-assisted coding"),
      p(
        "This tool uses AI to assist with generating themes and coding open-ended survey responses. ",
        "The resulting themes and codes are based on probabilistic interpretations of text and ",
        "may differ from how a human researcher would code the same responses."
      ),
      p(
        tags$strong("Best practice: "),
        "Review generated themes before coding, validate results against your research objectives, ",
        "and apply human judgment where nuance or ambiguity exists."
      ),
      p(
        "This tool is designed to accelerate qualitative analysis, ",
        tags$strong("not to replace expert review or methodological oversight.")
      )
    ),
    
    # Credits
    
    div(
      class = "credits",
      tags$p(
        "Created by ",
        tags$strong("Giorgi Buzaladze"),
        " · ",
        tags$a(href = "https://giobuzala.com/", target = "_blank", "Website"),
        " · ",
        tags$a(href = "https://github.com/giobuzala", target = "_blank", "GitHub"),
        " · ",
        tags$a(href = "https://www.linkedin.com/in/giorgibuzaladze/", target = "_blank", "LinkedIn")
      )
    )
  )
)


# Server ----

server <- function(input, output, session) {
  # Reactive state holders ----

  # Stores the final coded survey data
  coded_data <- reactiveVal(NULL)
  
  # Stores the theme list generated by GPT
  generated_themes <- reactiveVal(NULL)
  
  # Stores the theme list actually used for coding (uploaded theme list OR generated themes)
  theme_used <- reactiveVal(NULL)
  
  # Stores the text shown in the status banner at the top of the app
  status_text <- reactiveVal("Ready to upload survey data.")
  
  # Helper function to update the status banner
  set_status <- function(msg) status_text(msg)
  
  # Data input reactives ----

  # Reads the uploaded survey file
  # Re-runs automatically whenever a new file is uploaded
  data_df <- reactive({
    req(input$data_file) # Block until a file exists
    read_input_file(input$data_file$datapath)
  })
  
  # Reads the uploaded theme list file (optional)
  # Returns NULL if no theme file is uploaded
  theme_df <- reactive({
    if (is.null(input$theme_file)) return(NULL)
    read_input_file(input$theme_file$datapath)
  })
  
  # UI outputs ----

  # Dynamically populate ID / response column selectors based on the uploaded survey file
  output$column_selectors <- renderUI({
    req(data_df())
    cols <- names(data_df())
    
    tagList(
      selectInput("id_col", "ID column", choices = cols),
      selectInput("response_col", "Open-ended response column", choices = cols)
    )
  })
  
  # Render the status banner shown near the top of the app
  # This is the ONLY place status_text() is displayed
  output$status_banner <- renderUI({
    div(class = "notice", tags$strong("Status: "), status_text())
  })
  
  # Button enable/disable + global status logic ----

  # This observer runs whenever any referenced reactive changes
  # It acts like a simple state machine for the app
  observe({
    
    # Enable "Generate themes" once survey data exists
    toggleState("run_themes", !is.null(input$data_file))
    
    # Enable "Code responses" once either:
    # - Generated themes exist, OR
    # - An uploaded theme list exists
    toggleState("run", !is.null(generated_themes()) || !is.null(theme_df()))
    
    # Status priority is IMPORTANT here!
    # The first matching condition wins.
    
    if (is.null(input$data_file)) {
      # Nothing uploaded yet
      set_status("Ready to upload survey data.")
      
    } else if (!is.null(coded_data())) {
      # Coding has successfully completed
      set_status("Coding complete. Download your results below.")
      
    } else if (!is.null(generated_themes())) {
      # Themes have been generated but coding not yet run
      set_status("Themes generated. Download to review themes or proceed to coding.")
      
    } else {
      # Survey uploaded, but no themes generated or uploaded yet
      set_status("Ready to generate themes or code responses.")
    }
  })
  
  # Step 2 — Theme generation ----

  observeEvent(input$run_themes, {
    req(data_df(), input$response_col)
    
    # Immediate feedback before long-running operation
    set_status("Generating themes…")
    
    # Show Shiny's modal progress indicator while GPT runs
    withProgress(
      message = "Analyzing responses:",
      detail = "Generating theme list...",
      {
        # Run GPT theme generation with error handling
        result <- tryCatch(
          theme_gpt(
            data = data_df(),
            x = input$response_col,
            n = parse_n_themes(input$n_themes),
            sample = NULL
          ),
          error = function(e) {
            # If GPT errors, update status and stop quietly
            set_status(paste("Error:", e$message))
            NULL
          }
        )
        
        # Only update state if generation succeeded
        if (!is.null(result)) {
          generated_themes(result)
          set_status("Themes generated. Download to review themes or proceed to coding.")
        }
      }
    )
  })
  
  # Step 3 — Coding responses ----

  observeEvent(input$run, {
    req(data_df(), input$id_col, input$response_col)
    
    # Immediate feedback before coding starts
    set_status("Coding responses…")
    
    withProgress(
      message = "Coding responses:",
      detail = "Assigning themes...",
      {
        result <- tryCatch(
          {
            # Determine which theme list to use
            # Priority:
            # 1. Uploaded theme list
            # 2. Generated themes
            theme_list <- if (!is.null(theme_df())) {
              theme_df()
            } else if (!is.null(generated_themes())) {
              generated_themes()
            } else {
              stop(
                "Invariant violation: Coding triggered without a theme list.",
                call. = FALSE
              )
            }
            
            # Validate required columns in theme list
            if (!all(c("Code", "Bin") %in% names(theme_list))) {
              stop(
                HTML(
                  paste(
                    "<strong>Theme list format is invalid.</strong><br><br>",
                    "Your uploaded theme list must contain the following columns:<br><br>",
                    "• <strong>Code</strong> – a unique <u>numeric</u> code for each theme (e.g., 1, 2, 3)<br>",
                    "• <strong>Bin</strong> – short descriptive label for the theme (e.g., \"Trust\", \"Price\", \"Service\")<br>",
                    "• <strong>Description</strong> <em>(optional, but recommended)</em> – a brief explanation ",
                    "of what the theme represents<br><br>",
                    "<strong>Your file currently contains the following columns:</strong><br>",
                    paste(names(theme_list), collapse = ", "),
                    "<br><br>",
                    "Please rename your columns and try again."
                  )
                ),
                call. = FALSE
              )
            }
            
            # Store the theme list actually used for coding
            theme_used(theme_list)
            
            # Run the GPT-based coding function
            code_gpt(
              data = data_df(),
              x = input$response_col,
              id_var = input$id_col,
              theme_list = theme_list
            )
          },
          
          # Error handler for coding
          error = function(e) {
            # Show a visible error notification (HTML-rendered)
            showNotification(
              ui = HTML(e$message),
              type = "error",
              duration = NULL
            )
            
            # Update status banner to error state
            set_status("Error: Theme list format is invalid.")
            NULL
          }
        )
        
        # If coding succeeded, store results and update status
        if (!is.null(result)) {
          coded_data(result)
          set_status("Coding complete. Download your results below.")
        }
      }
    )
  })
  
  # Download handlers ----

  # Show theme list download button only after themes exist
  output$download_themes_ui <- renderUI({
    req(generated_themes())
    downloadButton("download_themes", "Download theme list", class = "btn-download")
  })
  
  output$download_themes <- downloadHandler(
    
    # File name
    filename = function() "Theme List.xlsx",
    
    # File content
    content = function(file) {
      # Input validation and data ----
      
      req(generated_themes())
      themes <- generated_themes()
      
      # Workbook ----
      
      wb <- openxlsx::createWorkbook()
      
      # Styles
      header_style <- openxlsx::createStyle(textDecoration = "bold", wrapText = TRUE, valign = "center")
      
      even_row_style <- openxlsx::createStyle(fgFill = "#F2F2F2", wrapText = TRUE, border = "TopBottomLeftRight", borderColour = "#E0E0E0", valign = "center")
      
      odd_row_style <- openxlsx::createStyle(fgFill = "#FFFFFF", wrapText = TRUE, border = "TopBottomLeftRight", borderColour = "#E0E0E0", valign = "center")
      
      # Pad theme list to 30 rows
      max_rows <- 30
      n_pad <- max(0, max_rows - nrow(themes))
      
      codes_sheet <- dplyr::bind_rows(
        themes,
        tibble::tibble(
          Code = rep(NA_integer_, n_pad),
          Bin = rep(NA_character_, n_pad),
          Description = rep(NA_character_, n_pad)
        )
      )
      
      # Theme List sheet ----
      
      openxlsx::addWorksheet(wb, "Theme List")
      openxlsx::writeData(wb, "Theme List", codes_sheet, withFilter = FALSE)
      
      # Header formatting
      openxlsx::addStyle(wb, "Theme List", header_style, rows = 1, cols = 1:ncol(codes_sheet), gridExpand = TRUE)
      
      # Zebra striping
      for (i in seq_len(nrow(codes_sheet))) {
        style <- if (i %% 2 == 0) even_row_style else odd_row_style
        
        openxlsx::addStyle(wb, "Theme List", style, rows = i + 1, cols = 1:ncol(codes_sheet), gridExpand = TRUE, stack = TRUE)
      }
      
      # Column widths
      openxlsx::setColWidths(wb, "Theme List", cols = 2, widths = 50)
      openxlsx::setColWidths(wb, "Theme List", cols = 3, widths = 80)
      
      # Save workbook ----
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # Show coded data download button only after coding succeeds
  output$download_coded_ui <- renderUI({
    req(coded_data())
    downloadButton("download", "Download coded file", class = "btn-download")
  })
  
  output$download <- downloadHandler(
    
    # File name
    filename = function() "Coded Responses.xlsx",
    
    # File content
    content = function(file) {
      # Workbook ----
      
      wb <- openxlsx::createWorkbook()
      
      # Styles
      header_style <- openxlsx::createStyle(textDecoration = "bold", wrapText = TRUE, valign = "center")
      
      even_row_style <- openxlsx::createStyle(fgFill = "#F2F2F2", wrapText = TRUE, border = "TopBottomLeftRight", borderColour = "#E0E0E0")
      
      odd_row_style <- openxlsx::createStyle(fgFill = "#FFFFFF", wrapText = TRUE, border = "TopBottomLeftRight", borderColour = "#E0E0E0")
      
      vert_center_style <- openxlsx::createStyle(valign = "center")
      
      # Coded Responses sheet ----
      
      coded <- coded_data()
        # %>% dplyr::mutate(`Bin(s)` = NA_character_)
      
      openxlsx::addWorksheet(wb, "Coded Responses")
      openxlsx::writeData(wb, sheet = "Coded Responses", x = coded, withFilter = TRUE)
      
      # Header formatting
      openxlsx::addStyle(wb, "Coded Responses", header_style, rows = 1, cols = 1:ncol(coded), gridExpand = TRUE)
      
      # Alternating rows and vertical centering
      for (i in seq_len(nrow(coded))) {
        style <- if (i %% 2 == 0) even_row_style else odd_row_style
        
        openxlsx::addStyle(wb, "Coded Responses", style, rows = i + 1, cols = 1:ncol(coded), gridExpand = TRUE)
        
        openxlsx::addStyle(wb, "Coded Responses", vert_center_style, rows = i + 1, cols = 1:ncol(coded), gridExpand = TRUE, stack = TRUE)
      }
      
      openxlsx::freezePane(wb, "Coded Responses", firstRow = TRUE)
      
      # # Bin(s) lookup
      code_col <- which(names(coded) == "Code(s)")
      bin_col  <- which(names(coded) == "Bin(s)")
      # 
      # openxlsx::writeData(
      #   wb,
      #   "Coded Responses",
      #   x = paste0(
      #     '=IF(', openxlsx::int2col(code_col), '2="", "", ',
      #     'TEXTJOIN("; ", , MAP(TEXTSPLIT(',
      #     openxlsx::int2col(code_col), '2, ","), LAMBDA(code,',
      #     ' IF(TRIM(code)="", "", TRIM(IFERROR(',
      #     ' XLOOKUP(TRIM(code)+0, ',
      #     '\'Theme List\'!$A$2:$A$100, ',
      #     '\'Theme List\'!$B$2:$B$100), ',
      #     '"CODE " & TRIM(code) & " DOES NOT EXIST")))))))'
      #   ),
      #   startCol = bin_col,
      #   startRow = 2
      # )
      # 
      # openxlsx::conditionalFormatting(
      #   wb,
      #   "Coded Responses",
      #   cols = bin_col,
      #   rows = 2:(nrow(coded) + 1),
      #   rule = "DOES NOT EXIST",
      #   type = "contains",
      #   style = openxlsx::createStyle(fontColour = "#FF0000")
      # )
      
      # Column widths
      openxlsx::setColWidths(wb, "Coded Responses", cols = 1, widths = 14)
      openxlsx::setColWidths(wb, "Coded Responses", cols = 2, widths = 100)
      openxlsx::setColWidths(wb, "Coded Responses", cols = code_col, widths = 20)
      openxlsx::setColWidths(wb, "Coded Responses", cols = bin_col,  widths = 100)
      
      # Theme List sheet ----
      
      themes <- theme_used() %>%
        dplyr::mutate(
          Count = NA_integer_,
          Percentage = NA_real_
        )
      
      openxlsx::addWorksheet(wb, "Theme List")
      openxlsx::writeData(wb, "Theme List", themes, withFilter = FALSE)
      
      # Header formatting
      openxlsx::addStyle(wb, "Theme List", header_style, rows = 1, cols = 1:ncol(themes), gridExpand = TRUE)
      
      # Alternating rows and vertical centering
      for (i in seq_len(nrow(themes))) {
        style <- if (i %% 2 == 0) even_row_style else odd_row_style
        
        openxlsx::addStyle(wb, "Theme List", style, rows = i + 1, cols = 1:ncol(themes), gridExpand = TRUE)
        
        openxlsx::addStyle(wb, "Theme List", vert_center_style, rows = i + 1, cols = 1:ncol(themes), gridExpand = TRUE, stack = TRUE)
      }
      
      # Column widths
      openxlsx::setColWidths(wb, "Theme List", cols = 2, widths = 50)
      openxlsx::setColWidths(wb, "Theme List", cols = 3, widths = 80)
      openxlsx::setColWidths(wb, "Theme List", cols = 4:5, widths = 14)
      
      # Count and percentage formulas
      count_formula <- paste0(
        'IF($A', 2:(nrow(themes) + 1), '="", "", ',
        'SUMPRODUCT(--(ISNUMBER(SEARCH("," & $A', 2:(nrow(themes) + 1),
        ' & ",", "," & SUBSTITUTE(\'Coded Responses\'!',
        openxlsx::int2col(code_col),
        '$2:$',
        openxlsx::int2col(code_col),
        '$3000, " ", "") & ",")))))'
      )
      
      perc_formula <- paste0(
        'IF($A', 2:(nrow(themes) + 1), '="", "", ',
        'SUMPRODUCT(--ISNUMBER(SEARCH("," & $A', 2:(nrow(themes) + 1),
        ' & ",", "," & SUBSTITUTE(\'Coded Responses\'!',
        openxlsx::int2col(code_col),
        '$2:$',
        openxlsx::int2col(code_col),
        '$3000, " ", "") & ","))) / ',
        'MAX(1, COUNTA(\'Coded Responses\'!',
        openxlsx::int2col(code_col),
        '$2:$',
        openxlsx::int2col(code_col),
        '$3000)))'
      )
      
      openxlsx::writeFormula(wb, "Theme List", count_formula, startCol = 4, startRow = 2)
      openxlsx::writeFormula(wb, "Theme List", perc_formula,  startCol = 5, startRow = 2)
      
      openxlsx::addStyle(wb, "Theme List", openxlsx::createStyle(numFmt = "0%"), rows = 2:(nrow(themes) + 1), cols = 5, gridExpand = TRUE, stack = TRUE)
      
      # Save workbook ----
      
      openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
    }
  )
}

shinyApp(ui, server)
