library(shiny)
library(readxl)
library(dplyr)
library(purrr)
library(stringr)
library(clipr)
library(shinyalert)
source("R/helpers.R")


ui <- fluidPage(
  titlePanel("Excel Data to dplyr Code Converter"),
  useShinyalert(),
  tags$div(
    style = "background-color: #FFA500; color: black; padding: 10px; text-align: center; font-weight: bold; font-size: 16px; margin-bottom: 15px;",
    "Warning: Only upload the variable selection Excel sheets here. DO NOT upload participant data here!"
  ),
  sidebarLayout(
    sidebarPanel(
      p("Upload an Excel file to generate the dplyr select code. Click 'Convert to Code' to see the generated code and 'Copy Code to Clipboard' to copy the code."),
      fileInput("file", "Choose Excel File", accept = c(".xlsx")),
      textInput("datasetName", "Enter a valid name for the dataset (it will be prefixed with 'dataset_id_'):", value = ""),
      actionButton("convert", "Convert to Code"),
      actionButton("clear", "Clear"),
      hr(),
      actionButton("copyCode", "Copy Code to Clipboard")
    ),
    mainPanel(
      tabsetPanel(
        id = "mainTabs",
        tabPanel("Data Preview", tableOutput("dataPreview")),
        tabPanel("Code Preview", verbatimTextOutput("codePreview"))
      )
    )
  )
)

server <- function(input, output, session) {
  codeText <- reactiveVal("")
  selectionData <- reactiveVal(NULL)
  
  observeEvent(input$convert, {
    req(input$file)
    
    dataset_name <- input$datasetName
    if (!validate_name(dataset_name)) {
      shinyalert(
        title = "Invalid Dataset Name",
        text = "The dataset name must be a valid R object name. It should start with a letter and only contain letters, numbers, periods, or underscores.",
        type = "error"
      )
      return()
    }
    
    file_path <- input$file$datapath
    sheet_names <- excel_sheets(file_path)
    sheet_names <- sheet_names[!startsWith(sheet_names, "_")]
    
    # Map of timepoints per sheet from headers
    timepoints_map <- map(sheet_names, ~ get_timepoints_for_sheet(file_path, .x)) |>
      set_names(sheet_names)
    
    # Read all sheets
    selection_data <- map_df(
      sheet_names,
      ~ process_sheet(.x, file_path, timepoints = timepoints_map[[.x]])
    ) |>
      mutate(
        across(where(is.logical), ~ replace_na(.x, FALSE)),
        across(where(is.character), ~ replace_na(.x, ""))
      )
    
    selectionData(selection_data)
    
    # Generate code (with robust fallbacks)
    select_code <- generate_dplyr_code(selection_data, dataset_name, timepoints_map)
    codeText(select_code)
    
    output$codePreview <- renderText(select_code)
    output$dataPreview  <- renderTable(selection_data)
    updateTabsetPanel(session, "mainTabs", selected = "Code Preview")
    
    if (grepl("\\(No variables matched your selection", select_code, fixed = TRUE)) {
      shinyalert(
        title = "No matches found",
        text = "I didn't find any variables that met the sheet/timepoint rules (including Father/OBS). Check the headers or your ticks.",
        type = "warning"
      )
    }
  })
  
  observeEvent(input$clear, {
    updateTabsetPanel(session, "mainTabs", selected = "Data Preview")
    output$dataPreview <- renderTable(NULL)
    output$codePreview <- renderText(NULL)
    codeText("")
    selectionData(NULL)
  })
  
  observeEvent(input$copyCode, {
    if (isTruthy(codeText())) {
      write_clip(codeText(), allow_non_interactive = TRUE)
      shinyalert(
        title = "Copied",
        text = "The generated dplyr code has been copied to your clipboard.",
        type = "success"
      )
    } else {
      shinyalert(
        title = "Warning",
        text = "Please select a file first and click on \"Convert to Code\"",
        type = "warning"
      )
    }
    updateTabsetPanel(session, "mainTabs", selected = "Code Preview")
  })
}

shinyApp(ui = ui, server = server)