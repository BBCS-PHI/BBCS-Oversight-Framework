# Function to calculate different value types ----------------------------------

# Purpose:
#   Calculates and standardises indicator values (Percentage, Crude/Ratio, DASR)
#   and returns a unified schema ready for row-binding across methods.
#
# Inputs:
#   data         - dataframe with (at minimum) the following columns:
#                  indicator_id, start_date, end_date, numerator, denominator,
#                  indicator_value (may be NA), value_type_code, imd_code,
#                  aggregation_id, sex_code, ethnicity_code, creation_date,
#                  source_code
#   metadata     - OPTIONAL dataframe used to enrich `data`; must contain:
#                  <metadata_key>, age, period_type, value_multiplier, status_code
#   metadata_key - character scalar giving the join key name in `metadata`
#                  (default: "indicator_id")
#
# Output:
#   A dataframe in the standardised schema `TIDY_COLS`, containing:
#     - Rows not requiring calculation (kept as-is, standardised)
#     - Rows that required calculation but were ineligible (standardised)
#     - Newly calculated results for:
#         * Percentage (via calc_percentage)
#         * Crude rate / Ratio (via calc_crude_or_ratio)
#         * DASR (via calc_dasr)
#   All outputs have consistent columns (e.g., indicator_value, lower_ci95,
#   upper_ci95) and cleaned datatypes.
#
# Eligibility rules for calculation:
#   - indicator_value is NA
#   - denominator is present and > 0
#   - value_multiplier is present (non-NA)
#   - status_code == 1
#   - value_type_code is present (non-NA)

calculate_values <- function(data, metadata, age_lookup, metadata_key = "indicator_id"){
  
  # -------- Validate inputs / bring in value_multiplier ---------------------------
  message("\u25B6 Cleaning data types and validating inputs...")
  df <- data |>
    mutate(time_period_type = get_duration_label(as.Date(start_date), as.Date(end_date))) |>
    create_combination_id() |>
    clean_data_types()
  
  # metadata is REQUIRED
  if (is.null(metadata)) {
    stop("`metadata` is required and must not be NULL")
  }
  
  # Basic checks
  if (!metadata_key %in% names(metadata)) {
    stop(sprintf("\u274C metadata must contain the key column '%s'", metadata_key))
  }
  if (!"value_multiplier" %in% names(metadata)) {
    stop("\u274C metadata must contain a 'value_multiplier' column")
  }
  if (!"status_code" %in% names(metadata)) {
    stop("\u274C metadata must contain a 'status_code' column")
  }
  
  # Select required columns from metadata
  metadata_cols = c(metadata_key, "age", "period_type", "value_multiplier", "status_code", "population_type", "precalculated")
  metadata2 <- metadata |> select(all_of(metadata_cols))
  
  # Bring metadata onto staging
  message("\u25B6 Bringing metadata onto staging table...")
  df <- df |>
    left_join(metadata2, by = setNames(metadata_key, metadata_key))
  
  # Exclude incomplete data
  # Comment this out as we don't want to exclude incomplete data
  # message("\u25B6 Excluding incomplete data..")
  # df <- exclude_incomplete_data(df = df, period_type_col = "period_type",
  #                                   start_date_col = "start_date", end_date_col = "end_date",
  #                                   time_period_col = "time_period_type",
  #                                   current_date = Sys.Date(), enforce_duration = TRUE, tolerance_days = 5L)
  
  
  # Create 3 and 5 year pooled data
  message("\u25B6 Creating 3 and 5 year pooled data...")
  # pooled_df <- create_pooled_with_ranges(df, ks = c(3,5), POOL_KEYS = POOL_KEYS) |>
  #   left_join(metadata2, by = setNames(metadata_key, metadata_key))
  pooled_df <- create_pooled_with_ranges(df, ks = c(3,5), POOL_KEYS = POOL_KEYS)
  
  # Combine pooled data with yearly data
  message("\u25B6 Combining yearly and pooled data...")
  all_df <- bind_rows(pooled_df, df) |>
    select(
      -ends_with(".x"),
      -ends_with(".y")
    )
  
  
  # --- Partition rows -------------------------------------------------------
  
  message("\u25B6 Determining rows requiring calculations...")
  # needs_calc <- is.na(all_df$indicator_value) & (!is.na(all_df$denominator) & all_df$denominator != 0) # this will bypass count data as denominator will always be zero/null
  needs_calc <- !is.na(all_df$precalculated) &
    all_df$precalculated == "No"
  
  # Only process where status_code == 1 or 2 AND value_multiplier is present
  eligible_for_processing <- needs_calc &
    !is.na(all_df$value_multiplier) &
    all_df$status_code %in% c(1,2) &
    !is.na(all_df$value_type_code)
  
  # To be calculated
  df_calc <- all_df[eligible_for_processing, ] |>
    mutate(numerator = if_else(is.na(numerator), 0, numerator))
  
  # Rows that we keep as-is (no calc needed)
  df_keep <- all_df[!needs_calc, ] |>
    # mutate(denominator = if_else(is.na(denominator), 0, denominator)) |>
    left_join(get_age_lookup(metadata = metadata2, age_lookup = age_lookup), by = "indicator_id") |>
    mutate(age_group_code = age_code) |>
    select(all_of(TIDY_COLS))
  
  
  # Print unique indicator ids for which rows are kept as-is 
  print(paste0("Unique indicator ids for which rows are kept as-is (no calc required): ",
               unique(df_keep$indicator_id)))
  
  # Rows that needed calc but were NOT eligible (will be appended to the output later)
  df_skip <- all_df[needs_calc & !eligible_for_processing, ] |>
    # mutate(denominator = if_else(is.na(denominator), 0, denominator)) |>
    left_join(get_age_lookup(metadata = metadata2, age_lookup = age_lookup), by = "indicator_id") |>
    mutate(age_group_code = age_code) |>
    select(all_of(TIDY_COLS))
  
  
  # -------- Percentage ------------------------------------------------------
  message("\u25B6 Calculating percentage...")
  temp1 <- df_calc |> calc_percentage() |>
    tidy_output()
  
  # -------- Crude rate & Ratio ----------------------------------------------
  message("\u25B6 Calculating crude rate and ratio...")
  temp2 <- df_calc |> calc_crude_or_ratio() |>
    tidy_output()
  
  # -------- Directly age standardised rate (DASR) ---------------------------
  message("\u25B6 Calculating DASR...")
  temp3 <- df_calc |> calc_dasr(metadata = metadata2, age_lookup = age_lookup) |>
    tidy_output()
  
  # -------- Count -----------------------------------------------------------
  message("\u25B6 Calculating count...")
  temp4 <- df_calc |> calc_count() |>
    tidy_output()
  
  # -------- Percentage change -------------------------------------------------
  message("\u25B6 Calculating count...")
  temp5 <- df_calc |> calc_percentage_change() |>
    tidy_output()
  
  # -------- Difference --------------------------------------------------------
  message("\u25B6 Calculating difference...")
  temp6 <- df_calc |> calc_difference() |>
    tidy_output()
  
  # -------- SII ---------------------------------------------------------------
  message("\u25B6 Calculating SII...")
  temp7 <- df_calc |> calculate_sii() |>
    tidy_output()

  
  # -------- Combine all outputs ---------------------------------------------
  message("\u25B6 Combining all outputs...")
  output <- bind_rows(
    df_keep,          # Rows not calculated
    df_skip,          # Rows that needed calc but were ineligible (status/value_multiplier)
    temp1,            # Percentage
    temp2,            # Crude rate & Ratio
    temp3,            # DASR
    temp4,            # Count
    temp5,            # Percentage change
    temp6,            # Difference
    temp7             # SII
  ) |> clean_data_types()
  
  # -------- Reporting of output ---------------------------------------------
  message("\u25B6 Reporting output total rows...")
  print(paste("Total rows for df_keep:", nrow(df_keep)))
  print(paste("Total rows for df_skip:", nrow(df_skip)))
  print(paste("Total rows for temp1 (perc):", nrow(temp1)))
  print(paste("Total rows for temp2 (crude & ratio):", nrow(temp2)))
  print(paste("Total rows for temp3 (DASR):", nrow(temp3)))
  print(paste("Total rows for temp4 (Count):", nrow(temp4)))
  print(paste("Total rows for temp5 (Percentage change):", nrow(temp5)))
  print(paste("Total rows for temp6 (Difference):", nrow(temp6)))
  print(paste("Total rows for temp7 (SII):", nrow(temp7)))
  
  # -------- Reporting of skipped items --------------------------------------
  message("\u25B6 Reporting skipped items...")
  if (any(needs_calc & !eligible_for_processing)) {
    reasons_df <- all_df[needs_calc & !eligible_for_processing, ] |>
      transmute(
        indicator_id = indicator_id,
        status_code = status_code,
        reason_missing_value_multiplier = is.na(value_multiplier),
        reason_status_not_1 = is.na(status_code) | status_code != 1,
        reason_missing_value_type = is.na(value_type_code)
      ) |>
      group_by(indicator_id, status_code) |>
      summarise(
        reasons = paste0(
          c(
            if (any(reason_missing_value_multiplier)) "missing value_multiplier" else NULL,
            if (any(reason_status_not_1)) paste0("status_code = ", unique(status_code)) else NULL,
            if (any(reason_missing_value_type)) "missing value_type_code" else NULL
          ),
          collapse = "; "
        ),
        .groups = "drop"
      )
    
    message("\u26A0\uFE0F  Some indicators were not processed:")
    apply(
      reasons_df,
      1,
      function(r) message("  ID ", r[["indicator_id"]], ": ", r[["reasons"]])
    )
  }
  
  message("\u2705 Process completed!")
  # Return a list of all dfs for tracking
  return(list(combined_calc_dfs = output,
              df_keep = df_keep,
              df_skip = df_skip,
              perc = temp1,
              crude_ratio = temp2,
              dasr = temp3,
              count = temp4,
              perc_change = temp5,
              difference = temp6,
              sii = temp7))
}
