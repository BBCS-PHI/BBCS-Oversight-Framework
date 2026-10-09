# Load packages
library(DBI)
library(odbc)
library(metricengineR)
library(tidyverse)
library(cli)

# Start timer
run_start <- Sys.time()

#1. Set parameters -------------------------------------------------------------
ids <- c("All")

# Source function file
source("R/Phase 1/load_excel_data.R") # Load manual Excel metrics data into SQL
source("R/pipeline.R")        # Run calculations pipeline

#2. Create database connection -------------------------------------------------
conn <- DBI::dbConnect(
  odbc::odbc(),
  Driver = "SQL Server",
  Server = "MLCSU-BI-SQL",
  Database = "Cluster_BBCS",
  Trusted_Connection = "True"
)

#3. Function to run all processes ----------------------------------------------

run_all <- function(conn, indicator_ids = "All", schema_name, table_name) {
  
  #-------------------- Phase 1 ------------------------------------------------
  # Load data into Phase 1 SQL staging table 
  
  cli::cli_h1("Phase 1")
  
  cli::cli_alert_info("Running data extraction")
  
  DBI::dbExecute(
    conn,
    "EXEC [Cluster_BBCS].[BBCS].[Oversight_Framework_Staging_Data_Insert_All]"
  )
  
  cli::cli_alert_success("Process completed.")
  
  #-------------------- Phase 2 ------------------------------------------------
  # Convert Phase 1 SQL staging table into Phase 2 SQL table 
  
  cli::cli_h1("Phase 2")
  
  cli::cli_alert_info("Converting Phase 1 SQL Staging table into Phase 2 SQL table")

  DBI::dbExecute(
    conn,
    "EXEC [Cluster_BBCS].[BBCS].[Oversight_Framework_Phase_2_Process]"
  )
  
  cli::cli_alert_success("Process completed.")
  
  #--------------------- Phase 3 -----------------------------------------------
  # Update age metadata table
  
  cli::cli_h1("Phase 3")
  
  cli::cli_alert_info("Updating Age metadata reference table")

  DBI::dbExecute(
    conn,
    "EXEC [Cluster_BBCS].[BBCS].[Oversight_Framework_Phase_3_Metadata_Age_Process]"
  )
  
  cli::cli_alert_success("Process completed.")
  
  # Create Phase 3 final input table e.g., combine all data into one
  
  cli::cli_alert_info("Combining latest data from multiple sources into one Phase 3 final input table")

  DBI::dbExecute(
    conn,
    "EXEC [Cluster_BBCS].[BBCS].[Oversight_Framework_Phase_3_Final_Input_Process]"
  )
  
  cli::cli_alert_success("Process completed.")
  
  #------------------------- Metadata ------------------------------------------
  
  cli::cli_h1("Load Metadata")
  
  metadata <- DBI::dbGetQuery(
    conn,
    "
  SELECT *
  FROM [Cluster_BBCS].[BBCS].[Oversight_Framework_Reference_Metadata]
  "
  )
  
  # Save metadata in the environment
  assign("metadata", metadata, envir = .GlobalEnv)
  
  age_metadata <- DBI::dbGetQuery(
    conn,
    "SELECT *
     FROM [Cluster_BBCS].[BBCS].[Metric_Engine_Reference_Age_Metadata]"
  )
  
  # Save age metadata in the environment
  assign("age_metadata", age_metadata, envir = .GlobalEnv)
  
  cli::cli_alert_success("Process completed.")
  
  #-------------------- Load staging data --------------------------------------
  
  cli::cli_h1("Data Extraction")
  
  #  Normalize indicator_ids
  ids <- metricengineR::normalise_indicator_ids(indicator_ids)
  
  if (is.null(ids) || length(ids) == 0) {
    message("Extracting ALL indicators from staging table ...")
  } else {
    message(sprintf("Extracting %d indicator(s) from staging table ...", length(ids)))
  }
  
  # Get indicators from the staging table
  staging_data <- metricengineR::get_indicators_from_sql(
    conn = conn,
    schema_name = schema_name,
    table_name = table_name,
    indicator_ids = indicator_ids,
  )
  
  # Save staging data in the environment
  assign("staging_data", staging_data, envir = .GlobalEnv)
  
  #-------------------- Run ETL ------------------------------------------------
  
  cli::cli_h1("Data Processing")
  
  result <- calculate_values(
    data = staging_data,
    metadata = metadata,
    age_metadata = age_metadata
  )
  
  cli::cli_alert_success("Process completed.")
  
  #--------------------- Store result ------------------------------------------
  
  cli::cli_h1("Store result")
  
  result <- list(
    result = result,
    staging_data = staging_data,
    metadata = metadata,
    indicator_ids = ids
  )
  
  cli::cli_alert_success("Process completed.")
  
  return(result)
  
}

#4. Execute and capture output -------------------------------------------------

output <- run_all(conn = conn,
                  indicator_ids =  ids,
                  schema_name = "BBCS",
                  table_name = "Oversight_Framework_Fact_Final_Input_Data")


#5. Run all DQ checks ----------------------------------------------------------

run_all_dq_checks(df = output$result$combined_calc_dfs,
                  reference_data = output$staging_data,
                  metadata = output$metadata)


#6. Standardize output ---------------------------------------------------------
result <- output$result$combined_calc_dfs |>
  dplyr::filter(time_period_type %in% c("1 year", "Monthly", "Quarterly")) |> 
  dplyr::mutate(insertion_date_time = Sys.time()) |>
  dplyr::mutate(
    indicator_id     = as.integer(indicator_id),
    start_date       = as.Date(start_date),
    end_date         = as.Date(end_date),
    numerator        = as.numeric(numerator),
    denominator      = as.numeric(denominator),
    indicator_value  = as.numeric(indicator_value),
    lower_ci95       = as.numeric(lower_ci95),
    upper_ci95       = as.numeric(upper_ci95),
    imd_code         = as.integer(imd_code),
    aggregation_id   = as.integer(aggregation_id),
    age_group_code   = as.integer(age_group_code),
    sex_code         = as.integer(sex_code),
    ethnicity_code   = as.integer(ethnicity_code),
    creation_date    = as.POSIXct(creation_date),
    value_type_code  = as.integer(value_type_code),
    source_code      = as.integer(source_code),
    time_period_type = as.character(time_period_type),
    combination_id   = as.integer(combination_id)
  ) |>  
  # NULL out CIs for all Oversight Framework metrics
  dplyr::mutate(
    lower_ci95 = NA_real_,
    upper_ci95 = NA_real_
  )

float_cols <- c("numerator", "denominator", "indicator_value", "lower_ci95", "upper_ci95")

for (col in float_cols) {
  result[[col]] <- as.numeric(result[[col]])
  result[[col]][!is.finite(result[[col]])] <- NA_real_
}


# 7) Write output into database ------------------------------------------------

cli::cli_h1("Final Data Output SQL Insertion")

metricengineR::replace_indicator_data_in_sql(
  conn,
  database_name = "Cluster_BBCS",
  schema_name   = "BBCS",
  table_name    = "Oversight_Framework_Fact_Final_Output_Data",
  data     = result,
  indicator_ids = ids,
  id_column = "indicator_id"
)


# 8) Output table updates ------------------------------------------------------

# Add additional columns for SPC value, SPC chart eligibility, latest data point flag

cli::cli_h1("Final Data Output Updates")

DBI::dbExecute(
  conn,
  "EXEC [Cluster_BBCS].[BBCS].[Oversight_Framework_Final_Output_Table_Updates]"
)

cli::cli_alert_success("Process completed.")

# 9) Create SPC charts ---------------------------------------------------------

cli::cli_h1("SPC Charts ")

DBI::dbExecute(
  conn,
  "EXEC [Cluster_BBCS].[BBCS].[Oversight_Framework_SPC_SCV]"
)

# End timer

run_end <- Sys.time()

total_mins <- round(as.numeric(difftime(run_end, run_start, units = "mins")), 2)

cli::cli_alert_success("Process completed.")

cli::cli_alert_info(" Total run time: {total_mins} min")

# 10) Close database connection ----------------------------------------------------
DBI::dbDisconnect(conn)
