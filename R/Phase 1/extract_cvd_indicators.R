library(metricengineR)
library(cvdprevent)
library(tidyverse)
library(janitor)
library(odbc)
library(DBI)

# Database connection ----------------------------------------------------------

conn <- DBI::dbConnect(
  odbc::odbc(),
  Driver = "SQL Server",
  Server = "MLCSU-BI-SQL",
  Database = "Cluster_BBCS",
  Trusted_Connection = "True"
)

#1. CVD indicator list ---------------------------------------------------------

# Get valid combinations of time period and system level
available_combinations <- cvdprevent::cvd_time_period_system_levels() |>
  dplyr::select(
    TimePeriodID,
    TimePeriodName,
    SystemLevelID,
    SystemLevelName
  ) |>
  dplyr::distinct() |> 
  janitor::clean_names()

# Retrieve indicators for every valid combination
all_indicator_availability <- purrr::map2(
  available_combinations$time_period_id,
  available_combinations$system_level_id,
  cvdprevent::cvd_indicator_list
) |>
  dplyr::bind_rows() |> 
  janitor::clean_names()

# Get all available indicators
cvd_indicators <- all_indicator_availability |>
  dplyr::select(
    indicator_id,
    indicator_code,
    indicator_name,
    indicator_short_name
  ) |>
  dplyr::distinct() |>
  dplyr::arrange(indicator_id) |> 
  janitor::clean_names()

# Load data into SQL
DBI::dbWriteTable(
  conn,
  name = DBI::Id(
    schema = "BBCS",
    table = "CVD_Prevent_Indicators_Dim"
  ),
  value = cvd_indicators,
  overwrite = TRUE,
  row.names = FALSE
)

#2. CVD metadata ---------------------------------------------------------------

cvd_metadata <- cvd_indicators$indicator_id |>
  purrr::map(cvdprevent::cvd_indicator_details) |>
  dplyr::bind_rows() |> 
  dplyr::rename_with(
    ~ stringr::str_replace_all(
      .x, "MetaData", "Metadata"
    )
  ) |> 
  janitor::clean_names()

# Check the max length of character columns
# cvd_metadata |>
#   dplyr::summarise(
#     dplyr::across(
#       where(is.character),
#       ~ max(nchar(.x), na.rm = TRUE)
#     )
#   ) |>
#   tidyr::pivot_longer(
#     everything(),
#     names_to = "column",
#     values_to = "max_length"
#   )

# Load data into SQL
DBI::dbWriteTable(
  conn,
  name = DBI::Id(
    schema = "BBCS",
    table = "CVD_Prevent_Metadata_Dim"
  ),
  value = cvd_metadata,
  overwrite = TRUE,
  row.names = FALSE,
  field.types = c(
    indicator_code = "VARCHAR(25)",
    indicator_id = "INT",
    indicator_name = "VARCHAR(500)",
    indicator_order = "INT",
    indicator_short_name = "VARCHAR(250)",
    notification_count = "INT",
    age_standardised = "VARCHAR(2)",
    category_name = "VARCHAR(250)",
    metadata = "VARCHAR(5000)",
    metadata_category_id = "INT",
    metadata_title = "VARCHAR(50)",
    indicator_status = "VARCHAR(2)"
    
  )
)

#3. Priority groups ------------------------------------------------------------

priority_groups <- cvdprevent::cvd_indicator_priority_groups() |> 
  janitor::clean_names() |> 
  dplyr::select(
    -c(
      axis_character,
      format_display_name,
      highest_priority_notification_type,
      indicator_format_id,
      notification_count
    )
  )


# Load data into SQL
DBI::dbWriteTable(
  conn,
  name = DBI::Id(
    schema = "BBCS",
    table = "CVD_Prevent_Priority_Groups_Dim"
  ),
  value = priority_groups,
  overwrite = TRUE,
  row.names = FALSE
)


#4. Extract latest data --------------------------------------------------------

cvd_data <- metricengineR::get_cvd_indicators(
  time_period_id = tail(sort(unique(available_combinations$time_period_id)), 2),
  system_level_id = c(1, 4, 5, 6, 7, 8)
) |> 
  janitor::clean_names()

# Finalise table
cvd_df <- cvd_data$data |> 
  dplyr::left_join(
    available_combinations |> 
      dplyr::select(system_level_id, system_level_name) |> 
      dplyr::distinct(),
    by = "system_level_id"
  ) |> 
  dplyr::transmute(
    area_code,
    area_name,
    area_type = system_level_name,
    category_attribute,
    numerator,
    denominator,
    value,
    lower_ci = lower_confidence_limit,
    upper_ci = upper_confidence_limit,
    indicator_code,
    indicator_name,
    indicator_short_name,
    metric_category_name,
    metric_category_type_name,
    time_period_name,
    value_note,
    factor
  )
  
# Load data into SQL
dbWriteTable(
  conn,
  name = DBI::Id(
    schema = "BBCS",
    table = "CVD_Prevent_Data_Fact"
  ),
  value = cvd_df,
  append = TRUE,
  row.names = FALSE
)


  
#5. Run stored procedure -------------------------------------------------------

# Deduplicate fact table
# Update the remaining tables (ambitions, benchmarking, and time periods formatting)

DBI::dbExecute(
  conn,
  "EXEC [BBCS].[CVD_Prevent_Updates]"
)

#6. Disconnect db connection ----------------------------------------------------

DBI::dbDisconnect(conn)