library(rvest)
library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(tibble)
library(fs)

make_absolute_url <- function(link, base_url) {
  
  ifelse(
    stringr::str_starts(link, "http"),
    link,
    paste0(
      stringr::str_remove(base_url, "/$"),
      "/",
      stringr::str_remove(link, "^/")
    )
  )
}

get_child_links <- function(
    parent_url,
    publication_pattern
) {
  
  message("Reading parent page: ", parent_url)
  
  page <- rvest::read_html(parent_url)
  
  links <- page |>
    rvest::html_elements("a") |>
    rvest::html_attr("href") |>
    na.omit() |>
    unique()
  
  links <- links[
    stringr::str_detect(
      links,
      publication_pattern
    )
  ]
  
  base_url <- stringr::str_extract(
    parent_url,
    "^https?://[^/]+"
  )
  
  links <- make_absolute_url(
    links,
    base_url
  )
  
  stringr::str_remove(links, "/$") |>
    unique()
}

get_resource_link <- function(
    publication_url,
    dataset_suffix = "/datasets",
    resource_text,
    period_pattern = "[a-z]+-[0-9]{4}/?$",
    match_period = TRUE,
    file_pattern = "\\.zip($|\\?)"
) {
  
  dataset_url <- paste0(
    stringr::str_remove(publication_url, "/$"),
    dataset_suffix
  )
  
  message("Checking: ", dataset_url)
  
  page <- tryCatch(
    rvest::read_html(dataset_url),
    error = function(e) {
      warning("Could not read: ", dataset_url)
      return(NULL)
    }
  )
  
  if (is.null(page)) {
    return(NA_character_)
  }
  
  link_nodes <- page |>
    rvest::html_elements("a")
  
  links <- tibble::tibble(
    link_text = rvest::html_text2(link_nodes),
    link_url = rvest::html_attr(link_nodes, "href")
  ) |>
    dplyr::filter(
      !is.na(link_url),
      stringr::str_detect(
        link_text,
        stringr::regex(
          resource_text,
          ignore_case = TRUE
        )
      ),
      stringr::str_detect(
        link_url,
        stringr::regex(
          file_pattern,
          ignore_case = TRUE
        )
      )
    )
  
  if (match_period) {
    
    publication_period <- publication_url |>
      stringr::str_extract(period_pattern) |>
      stringr::str_remove("/$")
    
    publication_period_text <- publication_period |>
      stringr::str_replace_all("-", " ") |>
      stringr::str_to_title()
    
    links <- links |>
      dplyr::filter(
        stringr::str_detect(
          link_text,
          stringr::fixed(
            publication_period_text,
            ignore_case = TRUE
          )
        )
      )
  }
  
  links <- links |>
    dplyr::pull(link_url) |>
    unique()
  
  if (length(links) == 0) {
    warning("No matching resource found for: ", publication_url)
    return(NA_character_)
  }
  
  base_url <- stringr::str_extract(
    dataset_url,
    "^https?://[^/]+"
  )
  
  make_absolute_url(
    links[[1]],
    base_url
  )
}

download_resource <- function(
    resource_url,
    download_folder
) {
  
  if (is.na(resource_url)) {
    return(NA_character_)
  }
  
  fs::dir_create(
    download_folder,
    recurse = TRUE
  )
  
  clean_url <- stringr::str_remove(
    resource_url,
    "\\?.*$"
  )
  
  file_name <- basename(clean_url)
  
  output_file <- file.path(
    download_folder,
    file_name
  )
  
  if (file.exists(output_file)) {
    message("Already downloaded: ", file_name)
    return(output_file)
  }
  
  message("Downloading: ", file_name)
  
  tryCatch(
    {
      download.file(
        resource_url,
        destfile = output_file,
        mode = "wb",
        quiet = TRUE
      )
      
      output_file
    },
    error = function(e) {
      warning(
        "Download failed: ",
        resource_url,
        " - ",
        conditionMessage(e)
      )
      
      NA_character_
    }
  )
}

extract_zip <- function(
    zip_file,
    extract_folder
) {
  
  if (
    is.na(zip_file) ||
    !file.exists(zip_file)
  ) {
    return(NA_character_)
  }
  
  folder_name <- tools::file_path_sans_ext(
    basename(zip_file)
  )
  
  output_folder <- file.path(
    extract_folder,
    folder_name
  )
  
  fs::dir_create(
    output_folder,
    recurse = TRUE
  )
  
  message("Extracting: ", basename(zip_file))
  
  tryCatch(
    {
      unzip(
        zipfile = zip_file,
        exdir = output_folder,
        overwrite = TRUE
      )
      
      output_folder
    },
    error = function(e) {
      warning(
        "Could not extract: ",
        zip_file,
        " - ",
        conditionMessage(e)
      )
      
      NA_character_
    }
  )
}

read_csv_files <- function(
    folder,
    all_columns_character = TRUE
) {
  
  csv_files <- list.files(
    folder,
    pattern = "\\.csv$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
  
  if (length(csv_files) == 0) {
    warning("No CSV files found in: ", folder)
    return(tibble::tibble())
  }
  
  purrr::map_dfr(
    csv_files,
    function(csv_file) {
      
      message("Reading: ", basename(csv_file))
      
      if (all_columns_character) {
        
        data <- readr::read_csv(
          csv_file,
          col_types = readr::cols(
            .default = readr::col_character()
          ),
          show_col_types = FALSE,
          progress = FALSE
        )
        
      } else {
        
        data <- readr::read_csv(
          csv_file,
          show_col_types = FALSE,
          progress = FALSE
        )
      }
      
      data |>
        dplyr::mutate(
          source_file = basename(csv_file),
          source_folder = basename(dirname(csv_file)),
          .before = 1
        )
    }
  )
}

scrape_download_read <- function(
    parent_url,
    publication_pattern,
    resource_text,
    download_folder,
    dataset_suffix = "/datasets",
    period_pattern = "[a-z]+-[0-9]{4}/?$",
    match_period = TRUE,
    file_pattern = "\\.zip($|\\?)",
    all_columns_character = TRUE
) {
  
  fs::dir_create(
    download_folder,
    recurse = TRUE
  )
  
  publication_links <- get_child_links(
    parent_url = parent_url,
    publication_pattern = publication_pattern
  )
  
  message(
    "Found ",
    length(publication_links),
    " publication pages."
  )
  
  resource_catalogue <- tibble::tibble(
    publication_url = publication_links
  ) |>
    dplyr::mutate(
      resource_url = purrr::map_chr(
        publication_url,
        get_resource_link,
        dataset_suffix = dataset_suffix,
        resource_text = resource_text,
        period_pattern = period_pattern,
        match_period = match_period,
        file_pattern = file_pattern
      )
    ) |>
    dplyr::filter(!is.na(resource_url)) |>
    dplyr::distinct(resource_url, .keep_all = TRUE)
  
  resource_catalogue <- resource_catalogue |>
    dplyr::mutate(
      downloaded_file = purrr::map_chr(
        resource_url,
        download_resource,
        download_folder = download_folder
      ),
      extracted_folder = purrr::map_chr(
        downloaded_file,
        extract_zip,
        extract_folder = download_folder
      )
    )
  
  combined_data <- read_csv_files(
    folder = download_folder,
    all_columns_character = all_columns_character
  )
  
  list(
    data = combined_data,
    catalogue = resource_catalogue,
    publication_links = publication_links
  )
}

