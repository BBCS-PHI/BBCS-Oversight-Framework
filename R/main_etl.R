# Load packages
library(tidyverse)
library(lubridate)
library(readxl)
library(DBI)
library(odbc)
library(PHEindicatormethods)
library(tibble)

# Start timer
run_start <- Sys.time()

# Set parameters
ids <- c("All")

# Source all function files
source("R/excel_data_load.R")
source("R/transformations.R")
source("R/time_periods.R")
source("R/calculations.R")
source("R/database.R")
source("R/data_quality.R")
source("R/pipeline.R")

# Create database connection
conn <- DBI::dbConnect(
  odbc::odbc(),
  Driver = "SQL Server",
  Server = "MLCSU-BI-SQL",
  Database = "Cluster_BBCS",
  Trusted_Connection = "True"
)


run_all <- function(conn, indicator_ids = "All", table_name) {
  
  # Convert Phase 1 SQL staging table into Phase 2 SQL table 
  message("Converting Phase 1 SQL Staging table into Phase 2 SQL table...")
  DBI::dbExecute(
    conn,
    "EXEC [Cluster_BBCS].[BBCS].[Oversight_Framework_Phase_2_Process]"
  )
  
  # Update age metadata table
  message("Updating Age metadata reference table ...")
  DBI::dbExecute(
    conn,
    "EXEC [Cluster_BBCS].[BBCS].[Oversight_Framework_Phase_3_Metadata_Age_Process]"
  )
  
  # Create Phase 3 final input table e.g., combine all data into one
  message("Combining latest data from multiple sources into one Phase 3 final input table ...")
  DBI::dbExecute(
    conn,
    "EXEC [Cluster_BBCS].[BBCS].[Oversight_Framework_Phase_3_Final_Input_Process]"
  )
  
  # Retrieve refreshed reference data
  metadata <- DBI::dbGetQuery(
    conn,
    "
  SELECT *
  FROM [Cluster_BBCS].[BBCS].[Oversight_Framework_Reference_Metadata]
  "
  )
  
  age_lookup <- get_age_lookup_from_sql(conn)
  
  #  Normalize indicator_ids
  ids <- normalize_indicator_ids(indicator_ids)
  
  # Pull fresh staging data
  if (is.null(ids) || length(ids) == 0) {
    message("Extracting ALL indicators from staging table ...")
  } else {
    message(sprintf("Extracting %d indicator(s) from staging table ...", length(ids)))
  }
  
  # Get indicators from the staging table
  staging_data <- get_indicators_from_sql(
    conn         = conn,
    table_name   = table_name,
    indicator_ids = ids
  ) 
  
  assign("staging_data", staging_data, envir = .GlobalEnv)
  
  # Run ETL
  message("Processing indicator data ...")
  result <- calculate_values(
    data = staging_data,
    metadata = metadata,
    age_lookup = age_lookup,
    metadata_key = "indicator_id"
  )
  
  list(
    result = result,
    staging_data = staging_data,
    metadata = metadata,
    indicator_ids = ids
  )
}

# 4) Execute and capture output -------------------------------------------------

output <- run_all(conn = conn,
                  indicator_ids =  ids,
                  table_name = "[Cluster_BBCS].[BBCS].[Oversight_Framework_Fact_Final_Input_Data]")

#5. Run all DQ checks ----------------------------------------------------------
run_all_dq_checks(df = output$result$combined_calc_dfs,
                  reference_data = output$staging_data,
                  metadata = output$metadata)

# 5) Add insertion time stamp and standardise schema ---------------------------
result <- output$result$combined_calc_dfs |>
  filter(time_period_type %in% c("1 year", "Monthly", "Quarterly")) |> 
  mutate(insertion_date_time = Sys.time()) |>
  mutate(
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
  ) |>  # NULL out CIs for all Oversight Framework metrics
  mutate(
    lower_ci95 = NA_real_,
    upper_ci95 = NA_real_
  )

float_cols <- c("numerator", "denominator", "indicator_value", "lower_ci95", "upper_ci95")

for (col in float_cols) {
  result[[col]] <- as.numeric(result[[col]])
  result[[col]][!is.finite(result[[col]])] <- NA_real_
}


# 7) Write output into database ------------------------------------------------
insert_data_into_sql_table(
  conn,
  database = "Cluster_BBCS",
  schema   = "BBCS",
  table    = "Oversight_Framework_Fact_Final_Output_Data",
  data     = result,
  indicator_ids = ids,
  id_column = "indicator_id"
)

# Add additional columns for SPC value, SPC chart eligibility, latest data point flag
message("Updating final output table ...")
DBI::dbExecute(
  conn,
  "EXEC [Cluster_BBCS].[BBCS].[Oversight_Framework_Final_Output_Table_Updates]"
)

# Create SPC charts 
message("Creating SPC charts...")
DBI::dbExecute(
  conn,
  "EXEC [Cluster_BBCS].[BBCS].[Oversight_Framework_SPC_SCV]"
)

# End timer
run_end <- Sys.time()
total_mins <- as.numeric(difftime(run_end, run_start, units = "mins"))
message(sprintf(" Total run time: %.2f min", total_mins))

# Close database connection
DBI::dbDisconnect(conn)
