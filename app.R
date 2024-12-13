library(shiny)
library(readxl)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(forcats)
library(clipr)
library(shinyalert) # Workflow warnings and errors

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
  # Make sure that the name of all helper sheets that don't have actual data start with an underscore "_"
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
  
  # remove colons from column names
  col_names <- str_remove(col_names, ":")
  auto_names <- set_names(auto_names, col_names)
  
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
      matches("^\\d"), # start with number
      as.logical
    )) |> 
    fill(Subscale, .direction = "down") |> 
    fill(Explanation, .direction = "down") # Ensure Explanation column is filled down
  
  return(df)
}


# Function to validate R object names
validate_name <- function(name) {
  # Check if the name is valid according to R naming rules
  grepl("^[a-zA-Z][a-zA-Z0-9._]*$", name)
}


generate_dplyr_code <- function(tibble, dataset_name) {
  full_dataset_name <- paste0("dataset_id_", dataset_name) # Ensure dataset name has the required prefix
  code <- paste0(full_dataset_name, " <- data_brabant |>\nselect(\n")
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
    explanation <- unique(group$Explanation) # Get explanations for the subscale
    time_points <- c()
    
    for (row in seq_len(nrow(group))) {
      
      first_time_point_in_var <- TRUE
      
      variable <- paste0("starts_with(\"", group$Variable[row], "_\")")
      
      for (time_point in c("28", "8wPP", "6mPP", "1yPP")) {
        if (group[[row, time_point]]) {
          
          # Add explanation as a comment
          if (first_time_point_in_var) {
            time_points <- c( 
              time_points,
              paste0(
                "    # ", 
                group$Explanation[row],
                "," # Add an extra comma so we can remove trailing commas
              )
            )
            first_time_point_in_var <- FALSE
          }
          
          time_points <- c(
            time_points,
            paste0("    ", variable, " & (ends_with(\"_", time_point, "\") | ends_with(\"_", time_point, "_r\"))")
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
  
  # Remove the trailing comma, add newline and close the select statement
  code <- paste0(
    substr(
      code, 
      start = 1, 
      stop = nchar(code) - 2
    ),
    "\n ) |> \n # favor recoded columns if non-recoded column is present \n prefer_recoded_columns()\n\n"
  )
  
  # Add code to write and preview the dataset
  code <- paste0(
    code,
    full_dataset_name, " |> write_sav(\"RDdata/", full_dataset_name, ".sav\")\n\n",
    "# preview\n",
    full_dataset_name
  )
  
  # Remove the double commas after the explanation
  code <- str_replace_all(code, ",,", "")
  
  return(code)
}


ui <- fluidPage(
  titlePanel("Excel Data to dplyr Code Converter"),
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
    
    selection_data <- map_df(
      sheet_names, 
      process_sheet, 
      file_path = file_path
    ) |> 
      mutate(across(
        everything(),
        ~ replace_na(.x, replace = FALSE)
      ))
    
    selectionData(selection_data)
    
    select_code <- generate_dplyr_code(selection_data, dataset_name)
    codeText(select_code)
    
    output$codePreview <- renderText({
      select_code
    })
    
    output$dataPreview <- renderTable({
      selection_data
    })
    
    updateTabsetPanel(session, "mainTabs", selected = "Code Preview")
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
