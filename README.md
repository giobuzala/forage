# [forage](https://giobuzala.shinyapps.io/forage/)

An interactive Shiny app for AI-assisted coding of open-ended survey responses.

## Overview

forage is an interactive Shiny application for coding open-ended survey responses into structured qualitative themes using OpenAI’s language models.

It is designed for survey researchers and analysts who want a faster, more consistent, and more transparent way to generate code frames and apply them to large volumes of verbatim responses without losing human control over the process.

The workflow is explicitly human-in-the-loop: generated theme lists can be reviewed, edited, or replaced before coding begins.

## Hosted app

The live version of forage is available on *shinyapps.io*:

```
https://giobuzala.shinyapps.io/forage/
```

## How it works

1. Upload a survey Excel file (`.xlsx` or `.xls`) that contains:
   - a **respondent ID** column
   - an **open-ended response** column

2. **Theme generation** creates a theme list:
   - **Code** - a unique numeric code for each theme (e.g., 1, 2, 3)
   - **Bin** - short descriptive label for the theme (e.g., "Trust", "Price", "Service")
   - **Description** - a brief explanation of what the theme represents

3. **Coding responses** assigns all applicable codes from the theme list to each response. The output is an Excel file with two sheets:
   - `Coded Responses`
   - `Theme List`

This structure keeps the coding transparent and auditable, making it easy to review results, share with collaborators, or feed into downstream analysis workflows.

## Methodology

This tool uses AI to assist with generating themes and coding open-ended survey responses. The resulting themes and codes are based on probabilistic interpretations of text and may differ from how a human researcher would code the same responses.

As a best practice, generated themes should be reviewed before coding, with results validated against research objectives and human judgment applied where nuance or ambiguity exists.

This tool is designed to accelerate qualitative analysis, not to replace expert review or methodological oversight.

## Files

- `app.R` Shiny UI with two steps: generate themes, then code responses.
- `Functions/theme_gpt.R` Generates a theme list from open-ended responses.
- `Functions/code_gpt.R` Codes responses using a provided theme list.

## To run locally

1. Install packages:

```r
install.packages(c(
  "dotenv", "dplyr", "httr2", "openxlsx", "purrr", "readxl", "shiny", "shinyjs", "stringr"
))
```

2. Set your OpenAI API key using one of the following methods:

```r
Sys.setenv(OPENAI_API_KEY="your_api_key")
```

Or create a local `.Renviron` file in the project root:

```r
OPENAI_API_KEY="your_api_key"
```

3. Start the app:

```r
shiny::runApp("app.R")
```

## License

MIT
