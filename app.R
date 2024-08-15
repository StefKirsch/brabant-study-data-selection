library(shiny)
library(readxl)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(forcats)
library(clipr)
#library(shinyjs)
library(shinyalert)

# Function to read and process a single sheet
process_sheet <- function(sheet_name, file_path) {
  # Read the first two rows to determine the column names
  header_df <- read_excel(
    file_path, 
    sheet = sheet_name, 
    n_max = 2,
    col_names = FALSE,
    .name_repair = "unique_quiet"
  )
  
  # Read the data. We do this now, because we need the dummy column names
  df <- file_path |>
    read_excel(
      sheet = sheet_name,
      skip = 0, 
      col_names = FALSE,
      .name_repair = "unique_quiet"
    ) |> 
    slice(-1:-2)
  
  auto_names <- colnames(df)
  
  col_names <- ifelse(
    is.na(header_df[2, ]),
    header_df[1, ],
    header_df[2, ]
  ) |> 
    unlist()
  
  col_names <- str_remove(col_names, ":")
  auto_names <- set_names(auto_names, col_names)
  auto_names <- auto_names[1:detect_index(col_names, is.na) - 1]
  
  df <- df |>
    rename(
      any_of(auto_names)
    ) |> 
    select(names(auto_names)) |> 
    mutate(
      Category = sheet_name,
      .before = Subscale
    ) |> 
    mutate(across(
      matches("T[0-9]"),
      as.logical
    )) |> 
    fill(Subscale, .direction = "down")
  
  return(df)
}

# Function to generate dplyr code
generate_dplyr_code <- function(tibble) {
  code <- "select(\n"
  current_category <- ""
  
  tibble_grouped <- tibble |>
    mutate(
      Category = as_factor(Category),
      Subscale = as_factor(Subscale)
    ) |> 
    group_by(Category, Subscale) |>
    group_split()
  
  for (group in tibble_grouped) {
    category <- unique(group$Category)
    subscale <- unique(group$Subscale)
    time_points <- c()
    
    for (row in seq_len(nrow(group))) {
      variable <- paste0("starts_with(\"", group$Variable[row], "_\")")
      
      for (time_point in c("T1", "T2", "T3")) {
        if (group[[row, time_point]]) {
          time_points <- c(
            time_points, 
            paste0("    ", variable, " & contains(\"_", time_point, "\")")
          )
        }
      }
    }
    
    if (length(time_points) > 0) {
      if (category != current_category) {
        code <- paste0(code, "    # ", "Category: ", category, "\n")
        current_category <- category
      }
      code <- paste0(code, "      # ", "Subscale: ", subscale, "\n")
      columns <- paste(time_points, collapse = ",\n    ")
      code <- paste0(code, "    ", columns, ",\n")
    }
  }
  
  code <- paste0(substr(code, 1, nchar(code) - 2), "\n  )")
  
  return(code)
}

# Define the UI
ui <- fluidPage(
  #useShinyjs(),
  titlePanel("Excel Data to dplyr Code Converter"),
  sidebarLayout(
    sidebarPanel(
      p("Upload an Excel file to generate the dplyr select code. Click 'Convert to Code' to see the generated code and 'Copy Code to Clipboard' to copy the code."),
      fileInput("file", "Choose Excel File", accept = c(".xlsx")),
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

# Define the server
server <- function(input, output, session) {
  codeText <- reactiveVal("")
  selectionData <- reactiveVal(NULL)
  
  # Input file file picker
  observeEvent(input$file, {
    updateTabsetPanel(session, "mainTabs", selected = "Data Preview")
  })
  
  # Convert to Code button
  observeEvent(input$convert, {
    req(input$file)
    
    file_path <- input$file$datapath
    sheet_names <- excel_sheets(file_path)
    sheet_names <- sheet_names[!startsWith(sheet_names, "_")]
    
    selection_data <- map_df(
      sheet_names, 
      process_sheet, 
      file_path = file_path
    ) |> 
      mutate(across(
        everything(),
        ~ replace_na(.x, FALSE)
      ))
    
    selectionData(selection_data)
    
    select_code <- generate_dplyr_code(selection_data)
    full_code <- paste0("data_brabant |>\n", select_code)
    codeText(full_code)
    
    output$codePreview <- renderText({
      full_code
    })
    
    output$dataPreview <- renderTable({
      selection_data
    })
    
    updateTabsetPanel(session, "mainTabs", selected = "Data Preview")
  })
  
  # clear button
  observeEvent(input$clear, {
    updateTabsetPanel(session, "mainTabs", selected = "Data Preview")
    output$dataPreview <- renderTable(NULL)
    output$codePreview <- renderText(NULL)
    reset("file")
    codeText("")
    selectionData(NULL)
  })
  
  # Copy Code to Clipboard Button
  observeEvent(input$copyCode, {
    #req(codeText())
    if (isTruthy(codeText())) {
      write_clip(codeText(), allow_non_interactive = TRUE)
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

# Helper function to reset file input
reset <- function(id) {
  #shinyjs::reset(id)
}

shinyApp(ui = ui, server = server)
