library(metricengineR)
library(cvdprevent)

#1. Extract CVDPREVENT data ----------------------------------------------------

cvd_data <- metricengineR::get_cvd_indicators(
  time_period_id = c(33, 32),
  system_level_id = c(1, 4, 5, 6, 7, 8)
)

#2. Build fact table -----------------------------------------------------------

cvd_df <- cvd_data$data |> 
  left_join(
    cvdprevent::cvd_time_period_system_levels() |> 
      select(SystemLevelID, SystemLevelName) |> 
      distinct(),
    by = c("system_level_id" = "SystemLevelID")
  ) |> 
  dplyr::transmute(
    area_code,
    area_name,
    area_type = SystemLevelName,
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
  

#3. Load data into SQL ---------------------------------------------------------

conn <- DBI::dbConnect(
  odbc::odbc(),
  Driver = "SQL Server",
  Server = "MLCSU-BI-SQL",
  Database = "Cluster_BBCS",
  Trusted_Connection = "True"
)

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

#4. Deduplicate data -----------------------------------------------------------

sql <- "
  ;WITH Dups AS (
      SELECT *,
          ROW_NUMBER() OVER (
              PARTITION BY
                  area_code,
                  area_name,
                  area_type,
                  category_attribute,
                  numerator,
                  denominator,
                  value,
                  lower_ci,
                  upper_ci,
                  indicator_code,
                  indicator_name,
                  indicator_short_name,
                  metric_category_name,
                  metric_category_type_name,
                  time_period_name,
                  value_note,
                  factor
              ORDER BY (SELECT NULL)
          ) AS rn
      FROM [Cluster_BBCS].[BBCS].[CVD_Prevent_Data_Fact]
  )
  DELETE FROM Dups
  WHERE rn > 1;
  "

DBI::dbExecute(
  conn, sql)
  
#5. Disconnect db connection ----------------------------------------------------

DBI::dbDisconnect(conn)