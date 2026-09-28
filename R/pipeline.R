# remotes::install_github(
#   "BBCS-PHI/metricengineR",
#   upgrade = "never"
# )
# packageVersion("metricengineR")

calculate_values <- function(
    data, 
    metadata, 
    age_metadata
    ){
  
  cli::cli_h1("Metric calculation pipeline")
  
  # -------- Validate inputs / bring in value_multiplier -----------------------
  cli::cli_alert_info(
    "Preparing data and validating inputs."
  )
  
  if(!is.data.frame(data)){
    stop(
      "`data` must be a data frame.",
      call. = FALSE
    )
  }
  
  if(!is.data.frame(metadata)){
    stop(
      "`metadata` must be a data frame.",
      call. = FALSE
    )
  }
  
  if(!is.data.frame(age_metadata)){
    stop(
      "`age_metadata` must be a data frame.",
      call. = FALSE
    )
  }
  
  
  # Validate required metadata columns
  
  metadata_cols <- c(
    "indicator_id",
    "age",
    "period_type",
    "value_multiplier",
    "status_code",
    "population_type",
    "precalculated"
  )
  
  missing_metadata_cols <- setdiff(
    metadata_cols,
    names(metadata)
  )
  
  if(length(missing_metadata_cols) > 0L){
    stop(
      paste0(
        "Missing required metadata columns: ",
        paste(missing_metadata_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  # Check metadata contains one row per indicator
  
  duplicate_metadata <- metadata |>
    dplyr::count(
      .data[["indicator_id"]],
      name = "row_count"
    ) |>
    dplyr::filter(
      .data$row_count > 1L
    )
  
  if(nrow(duplicate_metadata) > 0L){
    stop(
      "`metadata` must contain one row per indicator.",
      call. = FALSE
    )
  }
  
  # Prepare data
  
  df <- data |>
    dplyr::mutate(
      time_period_type = 
        metricengineR::derive_time_period_type(
          as.Date(.data$start_date), 
          as.Date(.data$end_date)
          )
      ) |>
    metricengineR::create_inequality_combination_id() |> 
    metricengineR::clean_data_types()
 

  # Select required metadata
  
  metadata2 <- metadata |> 
    dplyr::select(
      dplyr::all_of(
        metadata_cols
      )
    )
 
  # Join metadata with the staging data
  
  cli::cli_alert_info(
    "Joining metadata to staging data."
  )
  
  df <- df |>
    dplyr::left_join(
      metadata2, 
      by = "indicator_id"
      )
  
  # --- Determine processing groups --------------------------------------------
  
  cli::cli_alert_info(
    "Determining rows requiring calculations."
  )
 
  needs_calc <- !is.na(df$precalculated) &
   df$precalculated == "No"
  
  keep_as_is <- !is.na(df$precalculated) &
    df$precalculated == "Yes"
  
  # Only process where status_code == 1 or 2 AND value_multiplier is present
  eligible_for_processing <- needs_calc &
    !is.na(df$value_multiplier) &
    df$status_code %in% c(1L, 2L) &
    !is.na(df$value_type_code)
  
  skipped <- !keep_as_is &
    !eligible_for_processing
  
  # Rows requiring calculations
  
  df_calc <- df[eligible_for_processing, , drop = FALSE]

  # Pre-calculated rows kept as supplied
  
  df_keep <- df[keep_as_is, , drop = FALSE] |>
    dplyr::left_join(
      age_metadata |> 
        dplyr::select(
          "indicator_id",
          "single_age_code"
        ), 
      by = "indicator_id"
      ) |>
    dplyr::mutate(
      age_group_code = single_age_code
      ) |> # need to join back to age lookup to get the right age code especially for DASR indicators where the column has age splits instead of a single age group code
    metricengineR::tidy_output()
  
  if(nrow(df_keep) > 0L){
    
    kept_ids <- paste(
      unique(df_keep$indicator_id),
      collapse = ", "
    )
    
    cli::cli_alert_info(
      "Indicators kept as-is: {kept_ids}"
    )
    
  } else {
    
    cli::cli_alert_info(
      "No indicators were kept as-is."
    )
  }
  
  # Rows requiring calculation but not eligible
  
  df_skip <- df[skipped, , drop = FALSE] |>
    dplyr::left_join(
      age_metadata |> 
        dplyr::select(
          "indicator_id",
          "single_age_code"
        ), 
      by = "indicator_id"
      ) |>
    dplyr::mutate(
      age_group_code = single_age_code
      ) |>
    metricengineR::tidy_output()
  
  if(nrow(df_skip) == 0L){
    
    cli::cli_alert_success(
      "No indicators were skipped."
    )
    
  } else {
    
    skipped_ids <- paste(
      unique(df_skip$indicator_id),
      collapse = ", "
    )
    
    cli::cli_alert_warning(
      "Indicators skipped from calculation: {skipped_ids}"
    )
  }
  
  cli::cli_h2("Performing Calculations")
  
  # -------- Percentage --------------------------------------------------------
  cli::cli_h2("Percentage")
  
  percentage <- df_calc |> 
    metricengineR::calculate_percentage(
      confidence_intervals_required = FALSE
      ) |> 
    metricengineR::tidy_output()
  
  # -------- Crude rate  -------------------------------------------------------
  cli::cli_h2("Crude Rate")
  
  crude_rate <- df_calc |> 
    metricengineR::calculate_crude_rate(
      confidence_intervals_required = FALSE
    ) |> 
    metricengineR::tidy_output()
  
  # -------- Ratio  ------------------------------------------------------------
  cli::cli_h2("Ratio")
  
  ratio <- df_calc |> 
    metricengineR::calculate_ratio() |> 
    metricengineR::tidy_output()
  
  # -------- Directly age standardised rate (DASR) ---------------------------
  cli::cli_h2("DASR")
  
  dasr <- df_calc |>
    metricengineR::calculate_dasr(
      age_metadata = age_metadata |> 
        dplyr::select(
          "indicator_id",
          "single_age_code"
        ),
      confidence_intervals_required = FALSE
      ) |> 
    metricengineR::tidy_output()
  
  # -------- Count -----------------------------------------------------------
  cli::cli_h2("Count")
  
  count <- df_calc |> 
    metricengineR::calculate_count() |> 
    metricengineR::tidy_output()
  
  # -------- Percentage change -------------------------------------------------
  cli::cli_h2("Percentage Change")
 
  percentage_change <- df_calc |> 
    metricengineR::calculate_percentage_change() |> 
    metricengineR::tidy_output()
  
  # -------- Difference --------------------------------------------------------
  cli::cli_h2("Difference")
  
  difference <- df_calc |> 
    metricengineR::calculate_difference() |> 
    metricengineR::tidy_output()
  
  # -------- SII ---------------------------------------------------------------
  cli::cli_h2("Slope of Inequality Index (SII)")
  
  sii <- df_calc |> 
    metricengineR::calculate_sii() |> 
    metricengineR::tidy_output()
  
  # -------- Combine all outputs ---------------------------------------------
  
  cli::cli_alert_info(
    "Combining calculated outputs."
  )
  
  output <- dplyr::bind_rows(
    df_keep,          
    df_skip,          
    percentage,            
    crude_rate,             
    ratio,            
    dasr,            
    count,           
    percentage_change,            
    difference,            
    sii             
  ) |> 
    metricengineR::clean_data_types()
  
  # -------- Reporting of output ---------------------------------------------
  
  cli::cli_h2("Output summary")
  
  cli::cli_dl(
    c(
      "Rows kept as-is" = nrow(df_keep),
      "Rows excluded for investigation" = nrow(df_skip),
      "Percentage" = nrow(percentage),
      "Crude rate" = nrow(crude_rate),
      "Ratio" = nrow(ratio),
      "DASR" = nrow(dasr),
      "Count" = nrow(count),
      "Percentage change" = nrow(percentage_change),
      "Difference" = nrow(difference),
      "SII" = nrow(sii),
      "Total valid output" = nrow(output)
    )
  )
  
  # -------- Reporting of skipped items --------------------------------------
  
  if(any(skipped)){
    
    cli::cli_h2("Skipped indicators")
    
    reasons_df <- df[
      skipped,
      ,
      drop = FALSE
    ] |>
      dplyr::transmute(
        indicator_id = .data$indicator_id,
        status_code = .data$status_code,
        
        reason_missing_precalculated =
          is.na(.data$precalculated),
        
        reason_invalid_precalculated =
          !is.na(.data$precalculated) &
          !.data$precalculated %in% c("Yes", "No"),
        
        reason_missing_value_multiplier =
          is.na(.data$value_multiplier),
        
        reason_invalid_status =
          is.na(.data$status_code) |
          !.data$status_code %in% c(1L, 2L),
        
        reason_missing_value_type =
          is.na(.data$value_type_code)
      ) |>
      dplyr::group_by(
        .data$indicator_id,
        .data$status_code
      ) |>
      dplyr::summarise(
        reasons = paste(
          c(
            if(any(.data$reason_missing_precalculated))
              "missing precalculated" else NULL,
            
            
            if(any(.data$reason_invalid_precalculated))
              "invalid precalculated value" else NULL,
            
            if(any(.data$reason_missing_value_multiplier))
              "missing value_multiplier" else NULL,
            
            if(any(.data$reason_invalid_status))
              "invalid or missing status_code" else NULL,
            
            if(any(.data$reason_missing_value_type))
              "missing value_type_code" else NULL
          ),
          collapse = "; "
        ),
        .groups = "drop"
      )
    
    cli::cli_alert_warning(
      "{nrow(reasons_df)} indicator(s) were not processed."
    )
    
    for(i in seq_len(nrow(reasons_df))){
      
      cli::cli_bullets(
        c(
          "*" = paste0(
            "Indicator ",
            reasons_df$indicator_id[i],
            ": ",
            reasons_df$reasons[i]
          )
        )
      )
    }
  }
  
  
  # Complete
  
  cli::cli_alert_success(
    "Metric calculation pipeline completed successfully."
  )
  
  
  # Return outputs
  
  list(
    combined_calc_dfs = output,
    df_keep = df_keep,
    df_skip = df_skip,
    percentage = percentage,
    crude_rate = crude_rate,
    ratio = ratio,
    dasr = dasr,
    count = count,
    percentage_change = percentage_change,
    difference = difference,
    sii = sii
  )
}

run_all_dq_checks <- function(df, reference_data, metadata, cols_to_check = NULL, show_n = 10) {
  
  cli::cli_h1("Running Data Quality Checks")
  
  # Row counts ------------------------------------------------------
  
  message("1) Row counts\n")
  
  metricengineR::check_row_counts(
    df = df, 
    reference_data = reference_data
    )

  cat("\n")
  
  # Missing required columns ----------------------------------------
  
  message("2) Rows with missing required columns\n")
  
  metricengineR::check_missing_values(
    df = df,
    cols =  c("indicator_id", "start_date", "end_date", "time_period_type",
                    "combination_id", "source_code")
  )

  cat("\n")
  
  # Unique age group code for DASR indicators -----------------------
  
  message("3) Unique age_group_code for DASR indicators\n")
  
  metricengineR::check_dasr_age_group_code(
    df
  )

  cat("\n")
  
  # Active indicator values are populated ---------------------------
  
  message("4) Active indicator values are populated\n")
  
  metricengineR::check_active_indicator_values(
    df = df,
    metadata = metadata
  )
  
  cat("\n")
  
  # Duplicates ------------------------------------------------------
  
  message("5) Identify duplicates\n")
  
  metricengineR::check_duplicates(
    df = df,
    key_cols = c("indicator_id", "start_date", "end_date", "aggregation_id",
                 "age_group_code", "sex_code", "ethnicity_code", "imd_code", "value_type_code",
                 "source_code")
  )
  
  cat("\n")
  
  # Unique source code ----------------------------------------------
  
  message("6) Check number of unique source codes \n")
  
  metricengineR::check_source_code(
    df = df,
    indicator_col = "indicator_id",
    source_code_col = "source_code"
  )

  cat("\n")
  
  # Invalid percentages ---------------------------------------------
  
  message("7) Check invalid percentage values \n")
  
  metricengineR::check_invalid_percentages(
    df = df
  )
  
  cat("\n")
  
  # Missing confidence intervals -------------------------------------
  
  message("8) Check missing confidence intervals \n")
  
  metricengineR::check_missing_confidence_intervals(
    df = df
  )

  cat("\n")
  
  cli::cli_alert_info("DQ checks completed.")

}

