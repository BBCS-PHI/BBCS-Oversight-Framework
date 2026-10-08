library(readxl)
library(tidyverse)
library(purrr)
library(DBI)
library(odbc)

###################################################################################################################
# Purpose(s):
# To load Excel metrics data (Phase 1) into [Cluster_BBCS].[BBCS].[Oversight_Framework_Fact_SQL_Staging_Data_Excel]
# The data are then inserted into [Cluster_BBCS].[BBCS].[Oversight_Framework_Fact_SQL_Data]
# Excel input file path: //Mlcsu-bi-fs/bsolccg/Reports/02_Routine/BBCS Oversight Framework/SQL scripts/Excel Input
##################################################################################################################


#1.  List all Excel files ------------------------------------------------------

cli::cli_h1("Listing Excel files")

excel_files <- list.files(
  path = "//Mlcsu-bi-fs/bsolccg/Reports/02_Routine/BBCS Oversight Framework/SQL scripts/Excel Input",
  pattern = "\\.(xlsx|xlsm|xls)$",
  full.names = TRUE,
  ignore.case = TRUE
)

# Don't read the Data Input Template
excel_files <- excel_files[basename(excel_files) != "Data Input Template.xlsx"]

#2. Establish SQL connection ---------------------------------------------------

sql_connection <- dbConnect(
  odbc::odbc(),
  Driver   = "SQL Server",
  Server   = "MLCSU-BI-SQL",
  Database = "Cluster_BBCS",
  Trusted_Connection = "Yes"
)

#3. Read all Excel files -------------------------------------------------------

cli::cli_h1("Reading Excel files")

all_data <- excel_files |>
  purrr::map(
    ~ metricengineR::read_excel_file(
      .x,
      sheet_name = "Data Input"
    ) |>
      dplyr::mutate(
        reference_id = as.character(reference_id),
        indicator_id = as.integer(indicator_id),
        start_date = as.Date(start_date),
        end_date = as.Date(end_date),
        numerator = as.numeric(numerator),
        denominator = as.numeric(denominator),
        indicator_value = as.numeric(indicator_value),
        lower_ci95 = as.numeric(lower_ci95),
        upper_ci95 = as.numeric(upper_ci95),
        imd_code = as.integer(imd_code),
        geography_code = as.character(geography_code),
        aggregation_id = as.integer(aggregation_id),
        age_group_code = as.integer(age_group_code),
        sex_code = as.integer(sex_code),
        ethnicity_code = as.integer(ethnicity_code),
        creation_date = as.Date(creation_date),
        value_type_code = as.integer(value_type_code),
        source_code = as.integer(source_code)
      )
  ) |>
  dplyr::bind_rows()

cli::cli_alert_success("Process completed.")


#4. Populate remaining columns -------------------------------------------------

geography_lookup <- DBI::dbGetQuery(
  sql_connection,
  "
  SELECT 
      [aggregation_code],
      [aggregation_id],
      [aggregation_type]
  FROM [Cluster_BBCS].[BBCS].[Metric_Engine_Reference_Geography]
  "
) |> 
  mutate(
    aggregation_code2 = case_when(
      aggregation_type == "Provider" ~ substr(aggregation_code, 1, 3), # Get the first 3 characters for provider codes
      TRUE ~ aggregation_code
    )
  )

metadata <- DBI::dbGetQuery(
  sql_connection,
  "
  SELECT 
      a.[indicator_id],
      a.[reference_id],
      b.[value_type_code],
      a.[source_code]
  FROM [Cluster_BBCS].[BBCS].[Oversight_Framework_Reference_Metadata] a
  INNER JOIN [Cluster_BBCS].[BBCS].[Metric_Engine_Reference_Value_Type] b
      ON a.[value_type] = b.[value_type]
  "
)

df <- all_data |>
  dplyr::mutate(
    reference_id = as.character(reference_id),
    geography_code = case_when(
      
      geography_code %in% c("E38000258", "D2P2L") ~ geography_code,
      TRUE ~ substr(geography_code, 1, 3) # Get the first 3 characters for provider codes
    )
  ) |>
  dplyr::left_join(
    metadata |>
      dplyr::mutate(reference_id = as.character(reference_id)),
    by = "reference_id",
    suffix = c("", "_lookup")
  ) |>
  dplyr::left_join(
    geography_lookup,
    by = c("geography_code" = "aggregation_code2"),
    suffix = c("", "_lookup")
  ) |>
  dplyr::transmute(
    indicator_id = indicator_id_lookup,
    start_date,
    end_date,
    numerator,
    denominator,
    indicator_value,
    lower_ci95,
    upper_ci95,
    imd_code,
    aggregation_id = aggregation_id_lookup,
    age_group_code,
    sex_code,
    ethnicity_code,
    creation_date = Sys.Date(),
    value_type_code = value_type_code_lookup,
    source_code = source_code_lookup
  )



#5. DQ checks ------------------------------------------------------------------

# Check for any missing value across the following columns
df |>
  dplyr::summarise(
    dplyr::across(
      c(
        indicator_id,
        start_date,
        end_date,
        imd_code,
        aggregation_id,
        age_group_code,
        sex_code,
        ethnicity_code,
        creation_date,
        value_type_code,
        source_code
      ),
      ~ any(is.na(.))
    )
  )


# See the actual rows 
df |>
  dplyr::filter(
    dplyr::if_any(
      c(
        indicator_id,
        start_date,
        end_date,
        imd_code,
        aggregation_id,
        age_group_code,
        sex_code,
        ethnicity_code,
        creation_date,
        value_type_code,
        source_code
      ),
      is.na
    )
  )

#6. Loading data into SQL ------------------------------------------------------

# Append data to Oversight_Framework_Fact_SQL_Staging_Data_Excel

cli::cli_h1("Loading Excel data into SQL")

DBI::dbWriteTable(
  conn = sql_connection,
  name = DBI::Id(
    schema = "BBCS",
    table = "Oversight_Framework_Fact_SQL_Staging_Data_Excel"
  ),
  value = df,
  append = TRUE
)

# Insert data [Cluster_BBCS].[BBCS].[Oversight_Framework_Fact_SQL__Data]
dbExecute(sql_connection,
          "  INSERT INTO [Cluster_BBCS].[BBCS].[Oversight_Framework_Fact_SQL_Data] (
       [indicator_id]
      ,[start_date]
      ,[end_date]
      ,[numerator]
      ,[denominator]
      ,[indicator_value]
      ,[lower_ci95]
      ,[upper_ci95]
      ,[imd_code]
      ,[aggregation_id]
      ,[age_group_code]
      ,[sex_code]
      ,[ethnicity_code]
      ,[creation_date]
      ,[value_type_code]
      ,[source_code]
      )
	  (
  SELECT [indicator_id]
      ,[start_date]
      ,[end_date]
      ,[numerator]
      ,[denominator]
      ,[indicator_value]
      ,[lower_ci95]
      ,[upper_ci95]
      ,[imd_code]
      ,[aggregation_id]
      ,[age_group_code]
      ,[sex_code]
      ,[ethnicity_code]
      ,[creation_date]
      ,[value_type_code]
      ,[source_code]
	  FROM [Cluster_BBCS].[BBCS].[Oversight_Framework_Fact_SQL_Staging_Data_Excel]
	  ) "
)

# Remove the Excel staging table from the database
dbExecute(sql_connection,
          "DROP TABLE IF EXISTS [Cluster_BBCS].[BBCS].[Oversight_Framework_Fact_SQL_Staging_Data_Excel]")


cli::cli_alert_success("Process completed.")
