library(DBI)
library(odbc)
library(metricengineR)
library(tibble)

################################################################################
# Purpose(s):
# To download Fingertips data 
# To transform data according to the Oversight Framework table schema
# To load data into [Cluster_BBCS].[BBCS].[Oversight_Framework_Fact_API_Data]"
################################################################################

#1. Establish sql connection ---------------------------------------------------

sql_connection <-
  dbConnect(
    odbc(),
    Driver = "SQL Server",
    Server = "MLCSU-BI-SQL",
    Database = "Cluster_BBCS",
    Trusted_Connection = "True"
  )

#2. Fingertips indicator mapping -----------------------------------------------

ref_table <- tibble::tribble(
  ~fingertips_id, ~reference_id,
  93725,          8.03,
  93726,          8.03,
  92600,          8.04,
  94063,          8.05,
  30311,          8.06,
  40501,          8.01,
  40401,          8.02,
  92196,          22.2,
  20601,          22.51,
  20602,          22.52
)


#2. Extract Fingertips data ----------------------------------------------------

fingertips_data <- metricengineR::get_fingertips_indicators(
  ref_table$fingertips_id
) |> 
  janitor::clean_names()

#3. Get metadata to populate key columns ---------------------------------------

metadata <- dbGetQuery(
  sql_connection, "
SELECT e.indicator_id
, a.reference_id
, b.age_code
, c.sex_code
, CASE WHEN 
  a.imd = 'All (Persons)' THEN 999
  ELSE NULL 
  END as imd_code
, CASE WHEN 
  a.ethnicity_categories = 'All (Persons)' THEN 999
  ELSE NULL 
  END as ethnicity_code
, a.precalculated
, d.value_type_code
, a.source_code
FROM [Cluster_BBCS].[BBCS].[Oversight_Framework_Reference_Metadata] a

LEFT JOIN [Cluster_BBCS].[BBCS].[Metric_Engine_Reference_Age_Group] b
ON a.age = b.age_group_label

LEFT JOIN [Cluster_BBCS].[BBCS].[Metric_Engine_Reference_Sex] c
ON a.sex = c.sex

LEFT JOIN [Cluster_BBCS].[BBCS].[Metric_Engine_Reference_Value_Type] d
ON a.value_type = d.value_type

LEFT JOIN [Cluster_BBCS].[BBCS].[Oversight_Framework_Reference_Indicator_List] e
ON a.reference_id = e.Reference_ID
"
)

# Update metadata to add Fingertips ids
metadata <- metadata %>%
  inner_join(
    ref_table, by = "reference_id"
  )

#4. Build final dataset --------------------------------------------------------

final_df <- fingertips_data |> 
  dplyr::filter(
    area_type == "ICBs",
    area_name %in% c(
      "NHS Birmingham and Solihull Integrated Care Board - QHL",
      "NHS Black Country Integrated Care Board - QUA"
    )
  ) |> 
  dplyr::left_join(
    metadata,
    by = c("indicator_id" = "fingertips_id")
  ) |> 
  dplyr::group_by(
    indicator_id.y,
    reference_id,
    precalculated,
    area_name,
    time_period,
    time_period_range,
    age_code,
    sex_code,
    imd_code,
    ethnicity_code,
    value_type_code,
    source_code
  ) |> 
  dplyr::summarise(
    numerator = dplyr::if_else(
      all(is.na(count)), 
      NA_real_, 
      sum(count, na.rm = TRUE) # To prevent NULL numerators from being assigned as zeros
      ), 
    
    denominator = dplyr::if_else(
      all(is.na(denominator)),
      NA_real_,
      sum(denominator, na.rm = TRUE)
    ),
    
    indicator_value = dplyr::case_when(
      first(precalculated) == "Yes" ~ first(value),
      
      first(precalculated) == "No" &
        all(is.na(count)) ~ first(value), # Numerator is unavailable but Value is available, so use it as the fallback 
      
      TRUE ~ NA_real_
    ),
    
    lower_ci95 = dplyr::case_when(
      first(precalculated) == "Yes" ~ first(lower_ci_95_0_limit),
      
      first(precalculated) == "No" &
        all(is.na(count)) ~ first(lower_ci_95_0_limit),
      
      TRUE ~ NA_real_
    ),
    
    upper_ci95 = dplyr::case_when(
      first(precalculated) == "Yes" ~ first(upper_ci_95_0_limit),
      
      first(precalculated) == "No" &
        all(is.na(count)) ~ first(upper_ci_95_0_limit),
      
      TRUE ~ NA_real_
    ),
    
    .groups = "drop"
  ) |> 
  dplyr::mutate(
    
    # Starting year, e.g.
    # 2024/25   -> 2024
    # 2024      -> 2024
    # 2021 - 23 -> 2021
    
    start_year = as.integer(
      stringr::str_extract(.data$time_period, "^\\d{4}")
    ),
    
    # Number of years:
    # 1y -> 1
    # 3y -> 3
    # 5y -> 5
    
    period_years = as.integer(
      readr::parse_number(.data$time_period_range)
    ),
    
    # Identify whether Fingertips period is financial
    
    is_financial_year = stringr:: str_detect(
      .data$time_period,
      "/"
    ),
    
    start_date = dplyr::case_when(
      
      # Financial year
      # 2024/25 -> 01/04/2024
      
      .data$is_financial_year ~ lubridate::make_date(
        .data$start_year, # year
        4,                # month
        1                 # day
      ),
      
      # Calendar / pooled calendar
      # 2024      -> 01/01/2024
      # 2021 - 23 -> 01/01/2021
      
      TRUE ~ lubridate::make_date(
        .data$start_year, # year
        1,                # month
        1                 # day
      )
    ),
    
    end_date = dplyr::case_when(
      
      # Financial year
      # 2024/25 (start 2024 + 1 year) -> 31/03/2025
      
      .data$is_financial_year ~ lubridate::make_date(
        .data$start_year + .data$period_years , # year
        3,                                      # month
        31                                      # day
      ),
      
      # Calendar / pooled calendar
      # 2024, 1y      -> 31/12/2024
      # 2021 - 23, 3y -> 31/12/2023
      
      TRUE ~ lubridate::make_date(
        .data$start_year + .data$period_years - 1, # year
        12,                                        # month
        31                                         # day
      )
      ),
    
    age_group_code = .data$age_code,
    
    aggregation_id = dplyr::case_when(
      stringr::str_detect(
        .data$area_name,
        "Birmingham and Solihull"
      ) ~ "151",
      
      stringr::str_detect(
        .data$area_name,
        "Black Country"
      ) ~ "163",
      
      TRUE ~ NA_character_
    ),
    
    creation_date = Sys.Date()
    
    ) |> 
  dplyr::transmute(
    indicator_id = indicator_id.y,
    start_date,
    end_date,
    numerator,
    denominator,
    indicator_value,
    lower_ci95,
    upper_ci95,
    imd_code,
    aggregation_id,
    age_group_code,
    sex_code,
    ethnicity_code,
    creation_date,
    value_type_code,
    source_code
  ) %>%
  dplyr::mutate(
    numerator        = as.numeric(.data$numerator),
    denominator      = as.numeric(.data$denominator),
    indicator_value  = as.numeric(.data$indicator_value),
    lower_ci95       = as.numeric(.data$lower_ci95),
    upper_ci95       = as.numeric(.data$upper_ci95),
    age_group_code   = as.integer(.data$age_group_code),
    sex_code         = as.integer(.data$sex_code),
    value_type_code  = as.integer(.data$value_type_code),
    source_code      = as.integer(.data$source_code)
  )


#5. Load data into API staging table -------------------------------------------

dbWriteTable(
  sql_connection,
  name = DBI::Id(
    schema = "BBCS",
    table = "Oversight_Framework_Fact_API_Data"
  ),
  value = final_df,
  append = TRUE,
  row.names = FALSE
)

DBI::dbDisconnect(conn)

