library(purrr)
library(stringr)
library(tidyr)
library(forcats)

truthy_vals <- c("1", "true", "t", "yes", "y", "x", "✔", "check", "checked")
falsy_vals <- c("0", "false", "f", "no", "n", "")

as_bool_cell <- function(x) {
  v <- tolower(trimws(as.character(x)))
  ifelse(
    is.na(v) | v %in% falsy_vals, FALSE,
    ifelse(
      v %in% truthy_vals,
      TRUE,
      suppressWarnings(!is.na(as.logical(v))) & as.logical(v)
    )
  )
}

# Extract time points from the first two header rows.
# Special case: "Obstetric data" => "OBS"
get_timepoints_for_sheet <- function(file_path, sheet_name) {
  if (str_trim(sheet_name) == "Obstetric data") {
    return("OBS")
  }

  hdr <- read_excel(
    file_path,
    sheet = sheet_name,
    n_max = 2,
    col_names = FALSE,
    .name_repair = "unique_quiet",
    col_types = "text"
  )

  header_vals <- hdr[1, ] |>
    unlist(use.names = FALSE) |>
    as.character()
  is_tp_col <- !is.na(header_vals) &
    str_detect(
      header_vals,
      regex("^\\s*Time\\s*point\\s*$", ignore_case = TRUE)
    )
  if (!any(is_tp_col)) {
    return(character(0))
  }

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
    file_path,
    sheet = sheet_name, n_max = 2,
    col_names = FALSE, .name_repair = "unique_quiet",
    col_types = "text"
  )

  df <- read_excel(
    file_path,
    sheet = sheet_name, skip = 0,
    col_names = FALSE, .name_repair = "unique_quiet",
    col_types = "text"
  ) |>
    slice(-1:-2)

  auto_names <- colnames(df)
  col_names <- ifelse(
    is.na(header_df[2, ]),
    header_df[1, ],
    header_df[2, ]
  ) |>
    unlist() |>
    as.character()
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
      df <- df |> mutate(across(
        all_of(tp_cols_present),
        as_bool_cell
      ))
    }
  }

  # Also convert any columns that *look* like timepoint flags but weren't captured
  common_tp <- c("28", "8wpp", "6mpp", "1ypp", "obs")
  maybe_tp <- setdiff(names(df), c("Category", "Variable", "Subscale", "Explanation"))
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
  if ("Subscale" %in% names(df)) {
    df <- df |>
      fill(Subscale, .direction = "down")
  }
  if ("Explanation" %in% names(df)) {
    df <- df |>
      fill(Explanation, .direction = "down")
  }

  df
}

# Validate R object names
validate_name <- function(name) grepl("^[a-zA-Z][a-zA-Z0-9._]*$", name)

# Build dplyr code
generate_dplyr_code <- function(tibble, dataset_name, timepoints_map) {
  full_dataset_name <- paste0("dataset_id_", dataset_name)
  # Prepend an all-caps comment message
  code <- paste0(
    "# NOTE: DATA FROM THE MOTHER AFTER 2 YEARS POSTPARTUM IS NOT COMPLETE YET (DATA COLLECTION IS STILL ONGOING)\n\n",
    full_dataset_name, " <- data_brabant |>\nselect(\n"
  )
  current_category <- ""
  added_any <- FALSE

  tibble_grouped <- tibble |>
    mutate(
      Category = as_factor(Category),
      Subscale = as_factor(Subscale)
    ) |>
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
      logical_tp <- setdiff(logical_tp, c("Category", "Variable", "Subscale", "Explanation"))
      if (length(logical_tp) == 0) {
        candidates <- setdiff(names(group), c("Category", "Variable", "Subscale", "Explanation"))
        candidates <- candidates[
          nchar(candidates) <= 6 | tolower(candidates) %in% c("28", "8wpp", "6mpp", "1ypp", "obs")
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

        for (tp in group_tp) {
          val <- if (tp %in% names(group)) as_bool_cell(group[[row, tp]]) else FALSE
          if (isTRUE(val)) {
            if (is_father) {
              # Start with <var>_ and end with _F_<tp> or _F_<tp>_r
              selector <- paste0(
                "matches(\"^",
                varname,
                "_.*(?<=_)",
                "F_",
                tp,
                "(?:_r)?$\", perl = TRUE)"
              )
            } else {
              # Start with <var>_ and end with _<tp> or _<tp>_r, but NOT _F_<tp>
              selector <- paste0(
                "matches(\"^",
                varname,
                "_.*(?<!_F_)(?<=_)",
                tp,
                "(?:_r)?$\", perl = TRUE)"
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

  code <- paste0(
    code,
    full_dataset_name, " |> write_sav(\"RDdata/", full_dataset_name, ".sav\")\n\n",
    "# preview\n",
    full_dataset_name
  )

  stringr::str_replace_all(code, ",,", "")
}
