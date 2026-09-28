library(tidyverse)
library(metricengineR)
library(lubridate)
library(writexl)


##########################################################################################################################################################################
# 9.02 (OF0039) Level of growth in community care contacts
# Data source: https://digital.nhs.uk/data-and-information/publications/statistical/community-services-statistics-for-children-young-people-and-adults/june-2026/datasets
# Frequency: Monthly
# Numerator: Sum total of attended care contacts for each month of the period in the current financial year
# Denominator: Sum total of attended care contacts for each month of the equivalent period in the previous financial year
# Purpose(s): 
# To download data from the NHS publication website
# To transform data into the Oversight Framework standardised table schema
# To create an Excel file to be loaded into [Cluster_BBCS].[BBCS].[Oversight_Framework_Fact_SQL_Staging_Data]
#########################################################################################################################################################################

#1. Download CSDS data ----------------------------------------------------------

csds_result <- metricengineR::get_publication_data(
  parent_url = paste0(
    "https://digital.nhs.uk/data-and-information/publications/",
    "statistical/community-services-statistics-for-children-young-people-and-adults"
  ),
  publication_pattern = paste0(
    "community-services-statistics-for-children-young-people-and-adults/",
    "[a-z]+-[0-9]{4}/?$"
  ),
  resource_text = "CSV Data \\(as ZIP\\)",
  download_folder = "data/CSDS_downloads",
  dataset_suffix = "/datasets",
  period_pattern = "[a-z]+-[0-9]{4}/?$",
  match_period = TRUE,
  file_pattern = "\\.zip($|\\?)",
  all_columns_character = TRUE
)

#2. Filter and transform dataset -----------------------------------------------

##2.1. Build numerator dataset--------------------------------------------------

monthly_numerators <- csds_result$data |>
  dplyr::filter(
    DIMENSION == "TotalCareContacts",
    (ORG_LEVEL == "ICB" & ORG_CODE %in% c("QHL", "QUA")) |
      (ORG_LEVEL == "Provider" & ORG_CODE %in% c("RNA", "RRK", "RXK", "RYW","TAJ", "RBK", "RL4"))) |>
  dplyr::transmute(
    reference_id = "9.02",
    start_date = as.Date(REPORTING_PERIOD_START),
    end_date = as.Date(REPORTING_PERIOD_END),
    provider_code = dplyr::if_else(
      ORG_LEVEL == "Provider",
      paste0(ORG_CODE, "00"),
      NA_character_
    ),
    provider_site_code = NA_character_,
    icb = dplyr::case_when(
      ORG_LEVEL == "ICB" & ORG_CODE == "QHL" ~ "E38000258",
      ORG_LEVEL == "ICB" & ORG_CODE == "QUA" ~ "D2P2L",
      TRUE ~ NA_character_
    ),
    age = NA_integer_,
    numerator = as.numeric(MEASURE_VALUE),
    ethnicity_code = NA_integer_,
    imd_quintile = NA_integer_,
    geography_level = ORG_LEVEL,
    geography_code = dplyr::case_when(
      ORG_LEVEL == "ICB" ~ icb,
      ORG_LEVEL == "Provider" ~ provider_code
    )
  )

##2.2. Build denominator dataset -----------------------------------------------

previous_year_data <- monthly_numerators |>
  dplyr::transmute(
    geography_level,
    geography_code,
    start_date = start_date %m+% lubridate::years(1),
    denominator = numerator
  )

##2.3 Build final dataset ------------------------------------------------------

monthly_dataset <- monthly_numerators |>
  dplyr::left_join(
    previous_year_data,
    by = c(
      "geography_level",
      "geography_code",
      "start_date"
    )
  ) |>
  dplyr::select(
    -geography_level,
    -geography_code
  ) |> 
  dplyr::filter(!is.na(denominator)) |> 
  dplyr::select(reference_id, start_date, end_date, provider_code,
         provider_site_code, icb, age, numerator, denominator,
         ethnicity_code, imd_quintile)

#3. Create an Excel file -------------------------------------------------------

df <- monthly_dataset |> 
  dplyr::transmute(
    Reference_ID = as.numeric(reference_id),
    Start_Date = start_date,
    End_Date = end_date,
    Provider_Code = provider_code,
    Provider_Site_Code = provider_site_code,
    ICB = icb,
    Age = age,
    Numerator = numerator,
    Denominator = denominator,
    Ethnicity_Code = ethnicity_code,
    IMD_Quintile = imd_quintile
  )

output_file <- paste0(
  "//Mlcsu-bi-fs/bsolccg/Reports/02_Routine/BBCS Oversight Framework/SQL scripts/Excel Input/",
  "9.02 - Level of growth in community care contacts.xlsx"
)

# Remove existing file

if(file.exists(output_file)){
  
  file.remove(
    output_file
  )
}

# Write updated file

writexl::write_xlsx(
  df,
  path = output_file
)