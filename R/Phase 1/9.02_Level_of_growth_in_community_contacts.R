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
    numerator = as.numeric(MEASURE_VALUE),
    imd_code = 999L,
    geography_code = dplyr::case_when(
      ORG_LEVEL == "Provider" ~ paste0(ORG_CODE, "00"),
      ORG_LEVEL == "ICB" & ORG_CODE == "QHL" ~ "E38000258",
      ORG_LEVEL == "ICB" & ORG_CODE == "QUA" ~ "D2P2L",
      TRUE ~ NA_character_
    ),
    age_group_code = 999L,
    sex_code = 999L,
    ethnicity_code = 999L
  )

##2.2. Build denominator dataset -----------------------------------------------

previous_year_data <- monthly_numerators |>
  dplyr::transmute(
    geography_code,
    
    # Keep the original date so we can see where the denominator came from
    denominator_start_date = start_date,
    denominator_end_date = end_date,
    
    # Shift forward one year so it joins to the current year's row
    start_date = start_date %m+% lubridate::years(1),
    end_date = end_date %m+% lubridate::years(1),
    
    denominator = numerator
  )

##2.3 Build final dataset ------------------------------------------------------

monthly_dataset <- monthly_numerators |>
  dplyr::left_join(
    previous_year_data,
    by = c(
      "geography_code",
      "start_date",
      "end_date"
    )
  ) |>
  dplyr::filter(!is.na(denominator))

#3. Create an Excel file -------------------------------------------------------

df <- monthly_dataset |> 
  dplyr::transmute(
    reference_id,
    indicator_id = NA_integer_,
    start_date,
    end_date,
    numerator,
    denominator,
    indicator_value = NA_real_,
    lower_ci95 = NA_real_,
    upper_ci95 = NA_real_,
    imd_code = 999L,
    geography_code,
    aggregation_id = NA_integer_,
    age_group_code = 999L,
    sex_code = 999L,
    ethnicity_code = 999L,
    creation_date = as.Date(NA),
    value_type_code = NA_integer_,
    source_code = NA_integer_
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
  list("Data Input" = df),
  path = output_file
)
