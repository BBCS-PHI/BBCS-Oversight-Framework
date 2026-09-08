# Purpose(s):
# To store all functions related to extracting data from external resources using API 
# These include functions to extract data from Fingertips and CVD PREVENT

# Fingertips -------------------------------------------------------------------
# # install.packages("remotes")
# install.packages(c(
#   "miniUI",
#   "shiny",
#   "shinycssloaders"
# ))
# remotes::install_github("rOpenSci/fingertipsR",
#                         build_vignettes = TRUE,
#                         dependencies = "suggests")
library(fingertipsR)

get_fingertips_indicators <- function(indicator_ids) {
  # Load required packages
  if (!requireNamespace("httr", quietly = TRUE)) install.packages("httr")
  if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")
  
  library(httr)
  library(jsonlite)
  
  # Ensure input is a character vector
  indicator_ids <- as.character(indicator_ids)
  
  # Base URL for Fingertips API
  base_url <- "https://fingertips.phe.org.uk/api/all_data/csv/by_indicator_id?indicator_ids="
  
  # Initialize empty list to store results
  results_list <- list()
  
  # Loop through each indicator ID
  for (id in indicator_ids) {
    url <- paste0(base_url, id)
    
    # Try to fetch and parse data
    tryCatch({
      response <- GET(url)
      
      if (status_code(response) != 200) {
        stop(paste("HTTP error", status_code(response)))
      }
      
      # Read CSV content directly from response
      content_text <- content(response, "text", encoding = "UTF-8")
      df <- read.csv(text = content_text, stringsAsFactors = FALSE)
      
      results_list[[id]] <- df
      message(paste("Successfully retrieved indicator:", id))
      
    }, error = function(e) {
      message(paste("Error retrieving indicator", id, ":", e$message))
      results_list[[id]] <- NULL
    })
  }
  
  # Combine all successful data frames into one
  combined_df <- do.call(rbind, results_list[!sapply(results_list, is.null)])
  
  return(combined_df)
}

# Example
# ids <- c(93725, 93726,92600, 94063, 30311)
# fingertips_data <- get_fingertips_indicators(ids)

# CVD Prevent ------------------------------------------------------------------
# install.packages("pak")
# pak::pak("cvdprevent")

library(cvdprevent)

# All indicator types such as Outcome and Standard
indicator_types <- cvdprevent::cvd_indicator_types()

# All available time periods for different indicator types
time_periods <- indicator_types |> 
  dplyr::pull(IndicatorTypeID) |> 
  purrr::map_dfr(
    \(id) cvdprevent::cvd_time_period_list(
      indicator_type_id = id
    )
  )

# All available system levels for particular time periods
system_levels <- cvdprevent::cvd_time_period_system_levels()

# Metadata of available time periods and system levels
all_combinations <- system_levels |> 
  dplyr::transmute(
    time_period_id = TimePeriodID,
    system_level_id = SystemLevelID
  ) |> 
  dplyr::distinct()

# Function to get CVD indicators
get_cvd_indicators <- function(time_period_id = NULL, system_level_id = NULL, indicator_id = NULL, combinations = NULL) {
  
  message("Starting CVDPREVENT extraction...")
  
  if(!is.null(combinations)){
    combinations <- combinations |> 
      dplyr::select(
        time_period_id,
        system_level_id
      ) |> 
      dplyr::distinct()
  } else{
    if(is.null(time_period_id) || is.null(system_level_id)){
      stop(
        "Provide either 'combinations' or both ",
        "'time_period_id' and 'system_level_id'." 
      )
    }
  }
  
  combinations <- tidyr::expand_grid(
    time_period_id = time_period_id,
    system_level_id = system_level_id
  )
  
  results <- combinations |>
    purrr::pmap(
      function(time_period_id, system_level_id) {
        
        message(
          "Period: ", time_period_id,
          " | System level: ", system_level_id
        )
        
        tryCatch(
          {
            ids <- indicator_id
            
            # If no indicator IDs are provided, get all available indicators
            if (is.null(ids)) {
              
              indicators <- cvdprevent::cvd_indicator_list(
                time_period_id = time_period_id,
                system_level_id = system_level_id
              )
              
              ids <- unique(indicators$IndicatorID)
            }
            
            if (length(ids) == 0) {
              stop("No indicators available")
            }
            
            indicator_results <- purrr::map( 
              ids,
              function(id) {
                
                message("  Indicator: ", id)
                
                tryCatch(
                  {
                    # For each indicator ID, extract raw cvd data
                    data <- cvdprevent::cvd_indicator_raw_data( 
                      time_period_id = time_period_id, # accepts a single id
                      system_level_id = system_level_id, # accepts a single id
                      indicator_id = id # accepts a single id
                    ) |>
                      janitor::clean_names() |>
                      dplyr::mutate(
                        time_period_id = time_period_id,
                        system_level_id = system_level_id,
                        indicator_id = id
                      )
                    
                    list(
                      data = data,
                      error = NULL
                    )
                    
                  },
                  error = function(e) {
                    
                    list(
                      data = NULL,
                      error = tibble::tibble(
                        time_period = time_period_id,
                        system_level = system_level_id,
                        indicator_id = id,
                        error_message = conditionMessage(e)
                      )
                    )
                  }
                )
              }
            )
            
            list(
              data = indicator_results |>
                purrr::map("data") |>
                purrr::compact() |>
                dplyr::bind_rows(),
              
              error = indicator_results |>
                purrr::map("error") |>
                purrr::compact() |>
                dplyr::bind_rows()
            )
            
          },
          error = function(e) {
            
            list(
              data = NULL,
              error = tibble::tibble(
                time_period = time_period_id,
                system_level = system_level_id,
                indicator_id = NA_integer_,
                error_message = conditionMessage(e)
              )
            )
          }
        )
      }
    )
  
  raw_data <- results |>
    purrr::map("data") |>
    purrr::compact() |>
    dplyr::bind_rows()
  
  invalid_combinations <- results |>
    purrr::map("error") |>
    purrr::compact() |>
    dplyr::bind_rows()
  
  message("CVDPREVENT extraction completed")
  
  list(
    data = raw_data,
    invalid_combinations = invalid_combinations
  )
}

# result <- get_cvd_indicators(
#   time_period_id = c(31, 33),
#   system_level_id = c(1, 6),
#   indicator_id = c(1, 7, 50)
# )
# 
# result <- get_cvd_indicators(
#   combinations = all_combinations
# )
# head(result$data)
# head(result$invalid_combinations)

