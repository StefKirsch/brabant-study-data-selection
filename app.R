# library(shiny)
# library(readxl)
# library(dplyr)
# library(purrr)
# library(tidyr)
# library(stringr)
# library(forcats)
# library(clipr)
# library(shinyalert) # Workflow warnings and errors
# 
# # Function to read and process a single sheet
# process_sheet <- function(sheet_name, file_path) {
#   # Read the first two rows to determine the column names
#   header_df <- read_excel(
#     file_path, 
#     sheet = sheet_name, 
#     n_max = 2,
#     col_names = FALSE,
#     .name_repair = "unique_quiet"
#   )
#   
#   # Read the data. We do this now, because we need the dummy column names
#   # Make sure that the name of all helper sheets that don't have actual data start with an underscore "_"
#   df <- file_path |>
#     read_excel(
#       sheet = sheet_name,
#       skip = 0, 
#       col_names = FALSE,
#       .name_repair = "unique_quiet"
#     ) |> 
#     slice(-1:-2)
#   
#   auto_names <- colnames(df)
#   
#   col_names <- ifelse(
#     is.na(header_df[2, ]),
#     header_df[1, ],
#     header_df[2, ]
#   ) |> 
#     unlist()
#   
#   # remove colons from column names
#   col_names <- str_remove(col_names, ":")
#   auto_names <- set_names(auto_names, col_names)
#   
#   df <- df |>
#     rename(
#       any_of(auto_names)
#     ) |> 
#     select(names(auto_names)) |> 
#     mutate(
#       Category = sheet_name,
#       .before = Subscale
#     ) |> 
#     mutate(across(
#       matches("^\\d"), # start with number
#       as.logical
#     )) |> 
#     fill(Subscale, .direction = "down") |> 
#     fill(Explanation, .direction = "down") # Ensure Explanation column is filled down
#   
#   return(df)
# }
# 
# 
# # Function to validate R object names
# validate_name <- function(name) {
#   # Check if the name is valid according to R naming rules
#   grepl("^[a-zA-Z][a-zA-Z0-9._]*$", name)
# }
# 
# 
# generate_dplyr_code <- function(tibble, dataset_name) {
#   full_dataset_name <- paste0("dataset_id_", dataset_name) # Ensure dataset name has the required prefix
#   code <- paste0(full_dataset_name, " <- data_brabant |>\nselect(\n")
#   current_category <- ""
#   
#   tibble_grouped <- tibble |>
#     mutate(
#       Category = as_factor(Category),
#       Subscale = as_factor(Subscale)
#     ) |> 
#     group_by(Category, Subscale) |>
#     group_split()
#   
#   for (group in tibble_grouped) {
#     category <- unique(group$Category)
#     subscale <- unique(group$Subscale)
#     explanation <- unique(group$Explanation) # Get explanations for the subscale
#     time_points <- c()
#     
#     for (row in seq_len(nrow(group))) {
#       
#       first_time_point_in_var <- TRUE
#       
#       variable <- paste0("starts_with(\"", group$Variable[row], "_\")")
#       
#       for (time_point in c("28", "8wPP", "6mPP", "1yPP")) {
#         if (group[[row, time_point]]) {
#           
#           # Add explanation as a comment
#           if (first_time_point_in_var) {
#             time_points <- c( 
#               time_points,
#               paste0(
#                 "    # ", 
#                 group$Explanation[row],
#                 "," # Add an extra comma so we can remove trailing commas
#               )
#             )
#             first_time_point_in_var <- FALSE
#           }
#           
#           time_points <- c(
#             time_points,
#             paste0("    ", variable, " & (ends_with(\"_", time_point, "\") | ends_with(\"_", time_point, "_r\"))")
#           )
#         }
#       }
#     }
#     
#     if (length(time_points) > 0) {
#       if (category != current_category) {
#         code <- paste0(code, "    # ", "Category: ", category, "\n")
#         current_category <- category
#       }
#       code <- paste0(code, "      # ", "Subscale: ", subscale, "\n")
#       
#       columns <- paste(time_points, collapse = ",\n    ")
#       code <- paste0(code, "    ", columns, ",\n")
#     }
#   }
#   
#   # Remove the trailing comma, add newline and close the select statement
#   code <- paste0(
#     substr(
#       code, 
#       start = 1, 
#       stop = nchar(code) - 2
#     ),
#     "\n ) |> \n # favor recoded columns if non-recoded column is present \n prefer_recoded_columns()\n\n"
#   )
#   
#   # Add code to write and preview the dataset
#   code <- paste0(
#     code,
#     full_dataset_name, " |> write_sav(\"RDdata/", full_dataset_name, ".sav\")\n\n",
#     "# preview\n",
#     full_dataset_name
#   )
#   
#   # Remove the double commas after the explanation
#   code <- str_replace_all(code, ",,", "")
#   
#   return(code)
# }
# 
# 
# ui <- fluidPage(
#   titlePanel("Excel Data to dplyr Code Converter"),
#   tags$div(
#     style = "background-color: #FFA500; color: black; padding: 10px; text-align: center; font-weight: bold; font-size: 16px; margin-bottom: 15px;",
#     "Warning: Only upload the variable selection Excel sheets here. DO NOT upload participant data here!"
#   ),
#   sidebarLayout(
#     sidebarPanel(
#       p("Upload an Excel file to generate the dplyr select code. Click 'Convert to Code' to see the generated code and 'Copy Code to Clipboard' to copy the code."),
#       fileInput("file", "Choose Excel File", accept = c(".xlsx")),
#       textInput("datasetName", "Enter a valid name for the dataset (it will be prefixed with 'dataset_id_'):", value = ""),
#       actionButton("convert", "Convert to Code"),
#       actionButton("clear", "Clear"),
#       hr(),
#       actionButton("copyCode", "Copy Code to Clipboard")
#     ),
#     mainPanel(
#       tabsetPanel(
#         id = "mainTabs",
#         tabPanel("Data Preview", tableOutput("dataPreview")),
#         tabPanel("Code Preview", verbatimTextOutput("codePreview"))
#       )
#     )
#   )
# )
# 
# 
# 
# server <- function(input, output, session) {
#   codeText <- reactiveVal("")
#   selectionData <- reactiveVal(NULL)
#   
#   observeEvent(input$convert, {
#     req(input$file)
#     
#     dataset_name <- input$datasetName
#     if (!validate_name(dataset_name)) {
#       shinyalert(
#         title = "Invalid Dataset Name",
#         text = "The dataset name must be a valid R object name. It should start with a letter and only contain letters, numbers, periods, or underscores.",
#         type = "error"
#       )
#       return()
#     }
#     
#     file_path <- input$file$datapath
#     sheet_names <- excel_sheets(file_path)
#     sheet_names <- sheet_names[!startsWith(sheet_names, "_")]
#     
#     selection_data <- map_df(
#       sheet_names, 
#       process_sheet, 
#       file_path = file_path
#     ) |> 
#       mutate(across(
#         everything(),
#         ~ replace_na(.x, replace = FALSE)
#       ))
#     
#     selectionData(selection_data)
#     
#     select_code <- generate_dplyr_code(selection_data, dataset_name)
#     codeText(select_code)
#     
#     output$codePreview <- renderText({
#       select_code
#     })
#     
#     output$dataPreview <- renderTable({
#       selection_data
#     })
#     
#     updateTabsetPanel(session, "mainTabs", selected = "Code Preview")
#   })
#   
#   observeEvent(input$clear, {
#     updateTabsetPanel(session, "mainTabs", selected = "Data Preview")
#     output$dataPreview <- renderTable(NULL)
#     output$codePreview <- renderText(NULL)
#     codeText("")
#     selectionData(NULL)
#   })
#   
#   observeEvent(input$copyCode, {
#     if (isTruthy(codeText())) {
#       write_clip(codeText(), allow_non_interactive = TRUE)
#     } else {
#       shinyalert(
#         title = "Warning",
#         text = "Please select a file first and click on \"Convert to Code\"",
#         type = "warning"
#       )
#     }
#     updateTabsetPanel(session, "mainTabs", selected = "Code Preview")
#   })
# }
# 
# 
# shinyApp(ui = ui, server = server)
###########################################################
# library(shiny)
# library(readxl)
# library(dplyr)
# library(purrr)
# library(tidyr)
# library(stringr)
# library(forcats)
# library(clipr)
# library(shinyalert)
# # library(haven) # uncomment if you'll run write_sav()
# 
# # ---------- Helpers ----------
# 
# truthy_vals <- c("1","true","t","yes","y","x","✔","check","checked")
# falsy_vals  <- c("0","false","f","no","n","")
# as_bool_cell <- function(x) {
#   v <- tolower(trimws(as.character(x)))
#   ifelse(
#     is.na(v) | v %in% falsy_vals, FALSE,
#     ifelse(v %in% truthy_vals, TRUE,
#            suppressWarnings(!is.na(as.logical(v))) & as.logical(v))
#   )
# }
# 
# # Extract time points from the first two header rows.
# # Special case: "Obstetric data" => "OBS"
# get_timepoints_for_sheet <- function(file_path, sheet_name) {
#   if (str_trim(sheet_name) == "Obstetric data") return("OBS")
# 
#   hdr <- read_excel(
#     file_path,
#     sheet = sheet_name,
#     n_max = 2,
#     col_names = FALSE,
#     .name_repair = "unique_quiet",
#     col_types = "text"
#   )
# 
#   header_vals <- hdr[1, ] |> unlist(use.names = FALSE) |> as.character()
#   is_tp_col <- !is.na(header_vals) &
#     str_detect(header_vals, regex("^\\s*Time\\s*point\\s*$", ignore_case = TRUE))
#   if (!any(is_tp_col)) return(character(0))
# 
#   tps <- hdr[2, is_tp_col, drop = TRUE] |>
#     unlist(use.names = FALSE) |>
#     as.character() |>
#     str_trim()
#   tps <- tps[!is.na(tps) & tps != ""]
#   unique(tps)
# }
# 
# # Read and normalize one sheet
# process_sheet <- function(sheet_name, file_path, timepoints) {
#   header_df <- read_excel(
#     file_path, sheet = sheet_name, n_max = 2,
#     col_names = FALSE, .name_repair = "unique_quiet",
#     col_types = "text"
#   )
# 
#   df <- read_excel(
#     file_path, sheet = sheet_name, skip = 0,
#     col_names = FALSE, .name_repair = "unique_quiet",
#     col_types = "text"
#   ) |>
#     slice(-1:-2)
# 
#   auto_names <- colnames(df)
#   col_names <- ifelse(is.na(header_df[2, ]), header_df[1, ], header_df[2, ]) |>
#     unlist() |> as.character()
#   col_names <- str_remove(col_names, ":")
#   auto_names <- set_names(auto_names, col_names)
# 
#   df <- df |>
#     rename(any_of(auto_names)) |>
#     select(names(auto_names))
# 
#   # Ensure core columns exist
#   needed <- c("Variable", "Subscale", "Explanation")
#   for (nm in needed) if (!nm %in% names(df)) df[[nm]] <- NA_character_
# 
#   # Category positioning
#   df$Category <- sheet_name
#   if ("Subscale" %in% names(df)) df <- relocate(df, Category, .before = Subscale) else df <- relocate(df, Category, .before = 1)
# 
#   # Convert timepoint columns (if provided) to logicals
#   if (length(timepoints) > 0) {
#     tp_cols_present <- intersect(names(df), timepoints)
#     if (length(tp_cols_present) > 0) {
#       df <- df |> mutate(across(all_of(tp_cols_present), as_bool_cell))
#     }
#   }
# 
#   # Also convert any columns that *look* like timepoint flags but weren't captured
#   # (e.g., header detection failed). Heuristic: short labels or common tokens.
#   common_tp <- c("28","8wpp","6mpp","1ypp","obs")
#   maybe_tp <- setdiff(names(df), c("Category","Variable","Subscale","Explanation"))
#   maybe_tp <- maybe_tp[
#     nchar(maybe_tp) <= 6 | tolower(maybe_tp) %in% common_tp
#   ]
#   if (length(maybe_tp) > 0) {
#     # Only coerce columns that are not already logical
#     to_try <- setdiff(maybe_tp, names(df)[sapply(df, is.logical)])
#     if (length(to_try) > 0) {
#       df <- df |> mutate(across(all_of(to_try), as_bool_cell))
#     }
#   }
# 
#   # Fill down
#   if ("Subscale" %in% names(df)) df <- df |> fill(Subscale, .direction = "down")
#   if ("Explanation" %in% names(df)) df <- df |> fill(Explanation, .direction = "down")
# 
#   df
# }
# 
# # Validate R object names
# validate_name <- function(name) grepl("^[a-zA-Z][a-zA-Z0-9._]*$", name)
# 
# # Build dplyr code
# generate_dplyr_code <- function(tibble, dataset_name, timepoints_map) {
#   full_dataset_name <- paste0("dataset_id_", dataset_name)
#   code <- paste0(full_dataset_name, " <- data_brabant |>\nselect(\n")
#   current_category <- ""
#   added_any <- FALSE
# 
#   tibble_grouped <- tibble |>
#     mutate(Category = as_factor(Category), Subscale = as_factor(Subscale)) |>
#     group_by(Category, Subscale) |>
#     group_split()
# 
#   for (group in tibble_grouped) {
#     category <- as.character(unique(group$Category))
#     subscale <- as.character(unique(group$Subscale))
# 
#     # Timepoints from header
#     group_tp <- timepoints_map[[category]]
#     # OBS rule
#     if (identical(str_trim(category), "Obstetric data")) group_tp <- "OBS"
#     # Fallback: infer from logical columns in this group
#     if (length(group_tp) == 0 || all(!group_tp %in% names(group))) {
#       logical_tp <- names(group)[sapply(group, is.logical)]
#       logical_tp <- setdiff(logical_tp, c("Category","Variable","Subscale","Explanation"))
#       # Last fallback: look for common tokens even if not logical
#       if (length(logical_tp) == 0) {
#         candidates <- setdiff(names(group), c("Category","Variable","Subscale","Explanation"))
#         candidates <- candidates[
#           nchar(candidates) <= 6 | tolower(candidates) %in% c("28","8wpp","6mpp","1ypp","obs")
#         ]
#         logical_tp <- candidates
#       }
#       group_tp <- unique(logical_tp)
#     }
#     if (length(group_tp) == 0) next
# 
#     is_father <- str_detect(category, "\\s*-\\s*Father\\s*$")
#     time_points <- c()
# 
#     if (nrow(group) > 0) {
#       for (row in seq_len(nrow(group))) {
#         varname <- group$Variable[row]
#         if (is.na(varname) || !nzchar(varname)) next
#         first_time_point_in_var <- TRUE
#         variable <- paste0("starts_with(\"", varname, "_\")")
# 
#         for (tp in group_tp) {
#           # Convert cell to boolean robustly even if column isn't logical
#           val <- if (tp %in% names(group)) as_bool_cell(group[[row, tp]]) else FALSE
#           if (isTRUE(val)) {
#             suffix_core <- if (is_father) paste0("_F_", tp) else paste0("_", tp)
#             selector <- paste0(
#               variable, " & (ends_with(\"", suffix_core, "\") | ends_with(\"", suffix_core, "_r\"))"
#             )
#             if (first_time_point_in_var) {
#               expl <- group$Explanation[row]
#               if (!is.na(expl) && nzchar(expl)) {
#                 time_points <- c(time_points, paste0("    # ", expl, ","))
#               }
#               first_time_point_in_var <- FALSE
#             }
#             time_points <- c(time_points, paste0("    ", selector))
#           }
#         }
#       }
#     }
# 
#     if (length(time_points) > 0) {
#       if (!identical(category, current_category)) {
#         code <- paste0(code, "    # Category: ", category, "\n")
#         current_category <- category
#       }
#       code <- paste0(code, "      # Subscale: ", subscale, "\n")
#       columns <- paste(time_points, collapse = ",\n    ")
#       code <- paste0(code, "    ", columns, ",\n")
#       added_any <- TRUE
#     }
#   }
# 
#   # Close select() safely
#   if (added_any) {
#     if (stringr::str_ends(code, ",\n")) code <- substr(code, 1, nchar(code) - 2)
#     code <- paste0(code, "\n) |>\n# favor recoded columns if non-recoded column is present\nprefer_recoded_columns()\n\n")
#   } else {
#     code <- paste0(
#       code,
#       "    # (No variables matched your selection across sheets/timepoints)\n",
#       ")\n|>\n# favor recoded columns if non-recoded column is present\nprefer_recoded_columns()\n\n"
#     )
#   }
# 
#   # Write + preview
#   code <- paste0(
#     code,
#     full_dataset_name, " |> write_sav(\"RDdata/", full_dataset_name, ".sav\")\n\n",
#     "# preview\n",
#     full_dataset_name
#   )
# 
#   code <- stringr::str_replace_all(code, ",,", "")
#   code
# }
# 
# # ---------- UI ----------
# 
# ui <- fluidPage(
#   titlePanel("Excel Data to dplyr Code Converter"),
#   useShinyalert(),
#   tags$div(
#     style = "background-color: #FFA500; color: black; padding: 10px; text-align: center; font-weight: bold; font-size: 16px; margin-bottom: 15px;",
#     "Warning: Only upload the variable selection Excel sheets here. DO NOT upload participant data here!"
#   ),
#   sidebarLayout(
#     sidebarPanel(
#       p("Upload an Excel file to generate the dplyr select code. Click 'Convert to Code' to see the generated code and 'Copy Code to Clipboard' to copy the code."),
#       fileInput("file", "Choose Excel File", accept = c(".xlsx")),
#       textInput("datasetName", "Enter a valid name for the dataset (it will be prefixed with 'dataset_id_'):", value = ""),
#       actionButton("convert", "Convert to Code"),
#       actionButton("clear", "Clear"),
#       hr(),
#       actionButton("copyCode", "Copy Code to Clipboard")
#     ),
#     mainPanel(
#       tabsetPanel(
#         id = "mainTabs",
#         tabPanel("Data Preview", tableOutput("dataPreview")),
#         tabPanel("Code Preview", verbatimTextOutput("codePreview"))
#       )
#     )
#   )
# )
# 
# # ---------- Server ----------
# 
# server <- function(input, output, session) {
#   codeText <- reactiveVal("")
#   selectionData <- reactiveVal(NULL)
# 
#   observeEvent(input$convert, {
#     req(input$file)
# 
#     dataset_name <- input$datasetName
#     if (!validate_name(dataset_name)) {
#       shinyalert(
#         title = "Invalid Dataset Name",
#         text = "The dataset name must be a valid R object name. It should start with a letter and only contain letters, numbers, periods, or underscores.",
#         type = "error"
#       )
#       return()
#     }
# 
#     file_path <- input$file$datapath
#     sheet_names <- excel_sheets(file_path)
#     sheet_names <- sheet_names[!startsWith(sheet_names, "_")]
# 
#     # Map of timepoints per sheet from headers
#     timepoints_map <- map(sheet_names, ~ get_timepoints_for_sheet(file_path, .x)) |>
#       set_names(sheet_names)
# 
#     # Read all sheets
#     selection_data <- map_df(
#       sheet_names,
#       ~ process_sheet(.x, file_path, timepoints = timepoints_map[[.x]])
#     ) |>
#       mutate(
#         across(where(is.logical), ~ replace_na(.x, FALSE)),
#         across(where(is.character), ~ replace_na(.x, ""))
#       )
# 
#     selectionData(selection_data)
# 
#     # Generate code (with robust fallbacks)
#     select_code <- generate_dplyr_code(selection_data, dataset_name, timepoints_map)
#     codeText(select_code)
# 
#     output$codePreview <- renderText(select_code)
#     output$dataPreview  <- renderTable(selection_data)
#     updateTabsetPanel(session, "mainTabs", selected = "Code Preview")
# 
#     if (grepl("\\(No variables matched your selection", select_code, fixed = TRUE)) {
#       shinyalert(
#         title = "No matches found",
#         text = "I didn't find any variables that met the sheet/timepoint rules (including Father/OBS). Check the headers or your ticks; I now also try a fallback detection, so if you're still seeing this, the headers may not match expected layout.",
#         type = "warning"
#       )
#     }
#   })
# 
#   observeEvent(input$clear, {
#     updateTabsetPanel(session, "mainTabs", selected = "Data Preview")
#     output$dataPreview <- renderTable(NULL)
#     output$codePreview <- renderText(NULL)
#     codeText("")
#     selectionData(NULL)
#   })
# 
#   observeEvent(input$copyCode, {
#     if (isTruthy(codeText())) {
#       write_clip(codeText(), allow_non_interactive = TRUE)
#       shinyalert(
#         title = "Copied",
#         text = "The generated dplyr code has been copied to your clipboard.",
#         type = "success"
#       )
#     } else {
#       shinyalert(
#         title = "Warning",
#         text = "Please select a file first and click on \"Convert to Code\"",
#         type = "warning"
#       )
#     }
#     updateTabsetPanel(session, "mainTabs", selected = "Code Preview")
#   })
# }
# 
# shinyApp(ui = ui, server = server)
# 

library(shiny)
library(readxl)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(forcats)
library(clipr)
library(shinyalert)
# library(haven) # uncomment if you'll run write_sav()

# ---------- Helpers ----------

truthy_vals <- c("1","true","t","yes","y","x","✔","check","checked")
falsy_vals  <- c("0","false","f","no","n","")

as_bool_cell <- function(x) {
  v <- tolower(trimws(as.character(x)))
  ifelse(
    is.na(v) | v %in% falsy_vals, FALSE,
    ifelse(v %in% truthy_vals, TRUE,
           suppressWarnings(!is.na(as.logical(v))) & as.logical(v))
  )
}

# Extract time points from the first two header rows.
# Special case: "Obstetric data" => "OBS"
get_timepoints_for_sheet <- function(file_path, sheet_name) {
  if (str_trim(sheet_name) == "Obstetric data") return("OBS")
  
  hdr <- read_excel(
    file_path,
    sheet = sheet_name,
    n_max = 2,
    col_names = FALSE,
    .name_repair = "unique_quiet",
    col_types = "text"
  )
  
  header_vals <- hdr[1, ] |> unlist(use.names = FALSE) |> as.character()
  is_tp_col <- !is.na(header_vals) &
    str_detect(header_vals, regex("^\\s*Time\\s*point\\s*$", ignore_case = TRUE))
  if (!any(is_tp_col)) return(character(0))
  
  tps <- hdr[2, is_tp_col, drop = TRUE] |>
    unlist(use.names = FALSE) |>
    as.character() |>
    str_trim()
  tps <- tps[!is.na(tps) & tps != ""]
  unique(tps)
}

# Read and normalize one sheet
process_sheet <- function(sheet_name, file_path, timepoints) {
  header_df <- read_excel(
    file_path, sheet = sheet_name, n_max = 2,
    col_names = FALSE, .name_repair = "unique_quiet",
    col_types = "text"
  )
  
  df <- read_excel(
    file_path, sheet = sheet_name, skip = 0,
    col_names = FALSE, .name_repair = "unique_quiet",
    col_types = "text"
  ) |>
    slice(-1:-2)
  
  auto_names <- colnames(df)
  col_names <- ifelse(is.na(header_df[2, ]), header_df[1, ], header_df[2, ]) |>
    unlist() |> as.character()
  col_names <- str_remove(col_names, ":")
  auto_names <- set_names(auto_names, col_names)
  
  df <- df |>
    rename(any_of(auto_names)) |>
    select(names(auto_names))
  
  # Ensure core columns exist
  needed <- c("Variable", "Subscale", "Explanation")
  for (nm in needed) if (!nm %in% names(df)) df[[nm]] <- NA_character_
  
  # Category positioning
  df$Category <- sheet_name
  if ("Subscale" %in% names(df)) df <- relocate(df, Category, .before = Subscale) else df <- relocate(df, Category, .before = 1)
  
  # Convert timepoint columns (if provided) to logicals
  if (length(timepoints) > 0) {
    tp_cols_present <- intersect(names(df), timepoints)
    if (length(tp_cols_present) > 0) {
      df <- df |> mutate(across(all_of(tp_cols_present), as_bool_cell))
    }
  }
  
  # Also convert any columns that *look* like timepoint flags but weren't captured
  common_tp <- c("28","8wpp","6mpp","1ypp","obs")
  maybe_tp <- setdiff(names(df), c("Category","Variable","Subscale","Explanation"))
  maybe_tp <- maybe_tp[
    nchar(maybe_tp) <= 6 | tolower(maybe_tp) %in% common_tp
  ]
  if (length(maybe_tp) > 0) {
    # Only coerce columns that are not already logical
    to_try <- setdiff(maybe_tp, names(df)[sapply(df, is.logical)])
    if (length(to_try) > 0) {
      df <- df |> mutate(across(all_of(to_try), as_bool_cell))
    }
  }
  
  # Fill down
  if ("Subscale" %in% names(df)) df <- df |> fill(Subscale, .direction = "down")
  if ("Explanation" %in% names(df)) df <- df |> fill(Explanation, .direction = "down")
  
  df
}

# Validate R object names
validate_name <- function(name) grepl("^[a-zA-Z][a-zA-Z0-9._]*$", name)

# Build dplyr code
generate_dplyr_code <- function(tibble, dataset_name, timepoints_map) {
  full_dataset_name <- paste0("dataset_id_", dataset_name)
  code <- paste0(full_dataset_name, " <- data_brabant |>\nselect(\n")
  current_category <- ""
  added_any <- FALSE
  
  tibble_grouped <- tibble |>
    mutate(Category = as_factor(Category), Subscale = as_factor(Subscale)) |>
    group_by(Category, Subscale) |>
    group_split()
  
  for (group in tibble_grouped) {
    category <- as.character(unique(group$Category))
    subscale <- as.character(unique(group$Subscale))
    
    # Timepoints from header
    group_tp <- timepoints_map[[category]]
    # OBS rule
    if (identical(str_trim(category), "Obstetric data")) group_tp <- "OBS"
    # Fallback: infer from logical columns in this group
    if (length(group_tp) == 0 || all(!group_tp %in% names(group))) {
      logical_tp <- names(group)[sapply(group, is.logical)]
      logical_tp <- setdiff(logical_tp, c("Category","Variable","Subscale","Explanation"))
      # Last fallback: look for common tokens even if not logical
      if (length(logical_tp) == 0) {
        candidates <- setdiff(names(group), c("Category","Variable","Subscale","Explanation"))
        candidates <- candidates[
          nchar(candidates) <= 6 | tolower(candidates) %in% c("28","8wpp","6mpp","1ypp","obs")
        ]
        logical_tp <- candidates
      }
      group_tp <- unique(logical_tp)
    }
    if (length(group_tp) == 0) next
    
    is_father <- str_detect(category, "\\s*-\\s*Father\\s*$")
    time_points <- c()
    
    if (nrow(group) > 0) {
      for (row in seq_len(nrow(group))) {
        varname <- group$Variable[row]
        if (is.na(varname) || !nzchar(varname)) next
        first_time_point_in_var <- TRUE
        variable <- paste0("starts_with(\"", varname, "_\")")
        
        for (tp in group_tp) {
          # Convert cell to boolean robustly even if column isn't logical
          val <- if (tp %in% names(group)) as_bool_cell(group[[row, tp]]) else FALSE
          if (isTRUE(val)) {
            if (is_father) {
              # Father sheets: select _F_{tp} (and _F_{tp}_r)
              suffix_core <- paste0("_F_", tp)
              selector <- paste0(
                variable, " & (ends_with(\"", suffix_core, "\") | ends_with(\"", suffix_core, "_r\"))"
              )
            } else {
              # Non-father sheets: strictly _{tp} (or _{tp}_r), and explicitly EXCLUDE any _F_{tp}
              selector <- paste0(
                variable,
                " & (ends_with(\"_", tp, "\") | ends_with(\"_", tp, "_r\"))",
                " & !matches(\"_F_", tp, "(_r)?$\")"
              )
            }
            
            if (first_time_point_in_var) {
              expl <- group$Explanation[row]
              if (!is.na(expl) && nzchar(expl)) {
                time_points <- c(time_points, paste0("    # ", expl, ","))
              }
              first_time_point_in_var <- FALSE
            }
            time_points <- c(time_points, paste0("    ", selector))
          }
        }
      }
    }
    
    if (length(time_points) > 0) {
      if (!identical(category, current_category)) {
        code <- paste0(code, "    # Category: ", category, "\n")
        current_category <- category
      }
      code <- paste0(code, "      # Subscale: ", subscale, "\n")
      columns <- paste(time_points, collapse = ",\n    ")
      code <- paste0(code, "    ", columns, ",\n")
      added_any <- TRUE
    }
  }
  
  # Close select() safely
  if (added_any) {
    if (stringr::str_ends(code, ",\n")) code <- substr(code, 1, nchar(code) - 2)
    code <- paste0(code, "\n) |>\n# favor recoded columns if non-recoded column is present\nprefer_recoded_columns()\n\n")
  } else {
    code <- paste0(
      code,
      "    # (No variables matched your selection across sheets/timepoints)\n",
      ")\n|>\n# favor recoded columns if non-recoded column is present\nprefer_recoded_columns()\n\n"
    )
  }
  
  # Write + preview
  code <- paste0(
    code,
    full_dataset_name, " |> write_sav(\"RDdata/", full_dataset_name, ".sav\")\n\n",
    "# preview\n",
    full_dataset_name
  )
  
  code <- stringr::str_replace_all(code, ",,", "")
  code
}

# ---------- UI ----------

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

# ---------- Server ----------

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
