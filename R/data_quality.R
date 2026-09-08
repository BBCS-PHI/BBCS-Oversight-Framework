# DQ functions------------------------------------------------------------------
## Function to check row counts ------------------------------------------------

check_row_counts <- function(df, reference_data) {
  # Inputs:
  #   input_data     - data frame whose row count will be checked
  #   reference_data - data frame to compare against
  #
  # Output:
  #   Prints a message indicating whether the row counts match (with counts).
  
  input_rows <- nrow(df)
  reference_rows <- nrow(reference_data)
  
  if (input_rows == reference_rows) {
    message("\u2705 PASS: Row counts match: ", input_rows, " rows")
  } else {
    message("\u26A0\uFE0F WARNING: Row counts do NOT match. ",
            "Input: ", input_rows, " rows | Reference: ", reference_rows, " rows",
            "\n\nThis may be expected for metrics where multiple input rows are collapsed into a single calculated output row. ")
  }
}

## Function to check non-populated columns -------------------------------------

# ignoring numerator and denominator as these can be empty columns for pre-calculated indicators

check_missing_rows <- function(df, metadata, cols = NULL,
                                    ignore = c("numerator", "denominator",
                                               "lower_ci95", "upper_ci95"),
                                    show_n = 10
) {
  # Inputs:
  #   df     - data frame whose rows will be checked
  #   metadata - metadata to filter indicators to be checked
  #
  # Output:
  #   A dataframe with columns containing missing rows
  
  current_ids <- metadata |>
    dplyr::filter(status_code == 1 & precalculated == "No") |> # only want to check non pre-calculated indicators
    dplyr::distinct(indicator_id) |>
    dplyr::pull(indicator_id)
  
  # columns to check
  cols_to_check <-
    if (is.null(cols)) setdiff(names(df), ignore) else intersect(cols, names(df))
  
  miss_df <- df |>
    filter(denominator != 0 & !is.na(denominator)) |>
    dplyr::filter(indicator_id %in% current_ids) |>
    dplyr::filter(if_any(all_of(cols_to_check), ~ is.na(.x)))
  
  
  if (nrow(miss_df) == 0) {
    message("\u2705 PASS: No rows with missing values in the checked columns.")
  } else {
    missing_summary <- miss_df |>
      dplyr::select(
        indicator_id,
        dplyr::all_of(setdiff(cols_to_check, "indicator_id"))
      ) |>
      dplyr::mutate(
        dplyr::across(
          -indicator_id,
          ~ is.na(.x)
        )
      ) |>
      tidyr::pivot_longer(
        cols = -indicator_id,
        names_to = "missing_column",
        values_to = "is_missing"
      ) |>
      dplyr::filter(is_missing) |>
      dplyr::count(
        indicator_id,
        missing_column,
        name = "missing_rows"
      ) |>
      dplyr::arrange(
        indicator_id,
        missing_column
      )
      
    message("\u26A0\uFE0F WARNING: Found rows with missing values in the checked columns: ", nrow(miss_df))
    print(missing_summary)
    
  }
  
  return(miss_df)
  
}

## Function to check unique age group code for dasr indicator ------------------

check_age_group_code <- function(df) {
  #  Inputs:
  #   df - data frame containing DASR records; must include
  #        `indicator_id`, `age_group_code`, and `value_type_code`
  #
  # Output:
  #   Prints a message indicating whether each DASR (`value_type_code == 4`)
  #   indicator_id has exactly one unique `age_group_code` (OK) or if any
  #   indicator_id has more than one (problem).
  
  unique_age_count <- df |>
    filter(value_type_code == 4) |>
    distinct(indicator_id, age_group_code) |>
    group_by(indicator_id) |>
    summarise(count = n()) |>
    arrange(indicator_id) |>
    filter(count > 1)
  
  if(nrow(unique_age_count) == 0){
    message("\u2705 PASS: One unique age group code per dasr indicator ID")
  } else{
    message("\u26A0\uFE0F WARNING: More than 1 age group code per dasr indicator ID")
  }
}

## Function to check all time period types are populated -----------------------
check_time_period_type <- function(df) {
  #  Inputs:
  #   df - data frame whose rows will be checked
  #
  # Output:
  #   Prints a message indicating whether time period column is populated or not
  missing_rows <- df |>
    filter(is.na(time_period_type)) |>
    distinct(indicator_id, start_date, end_date)
  
  if (nrow(missing_rows) == 0) {
    message("\u2705 PASS: time_period_type is populated for all rows.")
  } else {
    message("\u26A0\uFE0F WARNING: Some indicators are missing time_period_type. Details below:")
    print(missing_rows)
  }
}


## Function to check value columns are populated for active indicators ---------

check_active_indicator_values <- function(df, metadata) {
  
  active_ids <- metadata |> 
    dplyr::filter(status_code == 1) |> # Active indicators
    dplyr::distinct(indicator_id) |> 
    dplyr::pull(indicator_id)
  
  # Missing values that are not expected
  failures <- df |>
    dplyr::filter(
      indicator_id %in% active_ids,
      is.na(indicator_value),
      !(
        (value_type_code == 9 & (is.na(denominator) | denominator == 0)) | # Percentage change
          (value_type_code == 2 & (is.na(denominator) | denominator == 0)) | # Percentage
          (value_type_code == 10 & (is.na(denominator) | denominator == 0))
      )
    )
  
  if (nrow(failures) == 0) {
    
    message("\u2705 PASS: All active indicators have a populated indicator_value, excluding expected missing percentage and percentage-change values.")
    
    return(invisible(NULL)) 
    
  } else {
    
    failure_summary <- failures |> 
      dplyr::count(
        indicator_id,
        value_type_code,
        name = "missing_rows"
      ) |> 
      dplyr::arrange(
        indicator_id,
        value_type_code
      )
    
    failed_ids <- failure_summary |> 
      dplyr::distinct(indicator_id) |> 
      dplyr::pull(indicator_id)
    
    message("\u26A0\uFE0F WARNING: Found active indicators withunexpected ",
            "missing indicator_value for indicator ID(s): ", paste(failed_ids, collapse = ", "))
    
    print(failure_summary)
    
    return(failures)
  }
}

# Function to check missing confidence intervals -------------------------------
check_missing_confidence_intervals <- function(df) {
  
  warnings <- df |>
    dplyr::filter(
      !is.na(indicator_value),
      is.na(lower_ci95) | is.na(upper_ci95)
    )
  
  if (nrow(warnings) == 0) {
    
    message(
      "\u2705 PASS: No missing confidence intervals."
    )
    
  } else {
    
    failure_summary <- warnings |>
      dplyr::count(
        indicator_id,
        value_type_code,
        name = "missing_rows"
      ) |>
      dplyr::arrange(
        indicator_id,
        value_type_code
      )
    
    failed_ids <- failure_summary |>
      dplyr::distinct(indicator_id) |>
      dplyr::pull(indicator_id)
    
    message(
      "\u26A0\uFE0F WARNING: Some rows have missing confidence intervals. ",
      "This may be acceptable for value types where confidence intervals ",
      "are not expected. \n Indicator ID(s): ",
      paste(sort(failed_ids), collapse = ", ")
    )
    
    print(failure_summary)
  }
  
  return(warnings)
}

## Function to check combination id is populated -------------------------------
check_combination_splits <- function(df) {
  #  Inputs:
  #   df - data frame whose rows will be checked
  #
  # Output:
  #   Prints a message indicating whether `combination_id` column is populated or not
  missing_rows <- df |>
    filter(is.na(combination_id))
  
  if (nrow(missing_rows) == 0) {
    message("\u2705 PASS: combination_id is populated for all rows.")
  } else {
    message("\u26A0\uFE0F WARNING: Some indicators are missing combination_id. Details below:")
    print(missing_rows)
  }
}

## Function to check duplicates ------------------------------------------------

check_duplicates <- function(df, key_cols=c(
  "indicator_id",
  "time_period_type",
  "aggregation_id",
  "start_date",
  "end_date",
  "imd_code",
  "ethnicity_code"
), indicator_filter = NULL) {
  
  result <- df %>%
    
    # Optional indicator filter
    {
      if (!is.null(indicator_filter)) {
        filter(., indicator_id %in% indicator_filter)
      } else {
        .
      }
    } |>
    
    # Group by key columns
    group_by(across(all_of(key_cols))) |>
    
    # Count rows
    summarise(row_count = n(), .groups = "drop") |>
    
    # Keep only duplicates
    filter(row_count > 1) |>
    
    arrange(indicator_id)
  
  if (nrow(result) > 0) {
    message(
      "\u274C FAIL: Duplicate records found: ",
      nrow(result),
      " duplicated key combination(s)."
    )
  } else {
    message("\u2705 PASS: No Duplicate records found for the specified key columns")
  }
  
  return(result)
}

## Function to check source codes ----------------------------------------------

check_source_code <- function(
    df,
    indicator_col = "indicator_id",
    source_code_col = "source_code"){
  
  # Count distinct source codes per indicator
  results <- df |>
    group_by(.data[[indicator_col]]) |>
    summarise(
      n_source_codes = n_distinct(.data[[source_code_col]]),
      source_codes = paste(unique(.data[[source_code_col]]), collapse = ", "),
      .groups = "drop"
    ) |>
    filter(n_source_codes > 1)
  
  if(nrow(results) > 0){
    message("\u26A0\uFE0F WARNING: Some indicators have more than one source code.")
    return(results)
  }else{
    message("\u2705 PASS: Every indicator has exactly one source code.")
    return(NULL)
  }
}


# Function to check valid percentages ------------------------------------------
check_percentages <- function(df) {
  
  invalid_rows <- df |>
    dplyr::filter(
      value_type_code == 2,
      !is.na(indicator_value),
      indicator_value > 100
    )
  
  if (nrow(invalid_rows) > 0) {
    
    failed_indicators <- unique(invalid_rows$indicator_id)
    
    message(
      "\u26A0\uFE0F WARNING: Found percentage values greater than 100 for ",
      "indicator ID(s): ",
      paste(failed_indicators, collapse = ", "),
      ".\n This may be valid for metrics comparing actual performance against ",
      "planned or target values, where the actual numerator exceeds the ",
      "planned denominator. Please review these indicators to confirm that ",
      "values above 100% are expected."
    )
    
  } else {
    
    message(
      "\u2705 PASS: No percentage values greater than 100 were found."
    )
  }
  
  return(invalid_rows)
}

## Run all DQ checks ------------------------------------------------------------

run_all_dq_checks <- function(df, reference_data, metadata, cols_to_check = NULL, show_n = 10) {
  message("\n==== Running Data Quality Checks ====\n\n")
  
  message("1) Row counts\n")
  check_row_counts(df, reference_data)
  cat("\n")
  
  message("2) Rows with missing required columns\n")
  check_missing_rows(df, metadata = metadata)
  cat("\n")
  
  message("3) Unique age_group_code for DASR indicators\n")
  check_age_group_code(df)
  cat("\n")
  
  message("4) time_period_type populated\n")
  check_time_period_type(df)
  cat("\n")
  
  message("5) Active indicator values populated\n")
  check_active_indicator_values(df, metadata)
  cat("\n")
  
  message("6) combination_id populated\n")
  check_combination_splits(df)
  cat("\n")
  
  message("7) Identify duplicates\n")
  check_duplicates(df)
  cat("\n")
  
  message("8) Check number of unique source codes \n")
  check_source_code(df)
  cat("\n")
  
  message("9) Check invalid percentage values \n")
  check_percentages(df)
  cat("\n")
  
  message("10) Check missing confidence intervals \n")
  check_missing_confidence_intervals(df)
  cat("\n")
  
  message("\n==== DQ checks completed ====\n")
}






