library(PHEindicatormethods)

# calculate_dsr2 ---------------------------------------------------------------
# This function ONLY allows the multiplier argument to be passed as a scalar value

calculate_dsr2 <-
  function (data, x, n, stdpop = NULL, type = "full", confidence = 0.95,
            multiplier = 1e+05, independent_events = TRUE, eventfreq = NULL,
            ageband = NULL) {
    if (missing(data) | missing(x) | missing(n) | missing(stdpop)) {
      stop("function calculate_dsr requires at least 4 arguments: data, x, n, stdpop")
    }
    if (!is.data.frame(data)) {
      stop("data must be a data frame object")
    }
    if (!deparse(substitute(x)) %in% colnames(data)) {
      stop("x is not a field name from data")
    }
    if (!deparse(substitute(n)) %in% colnames(data)) {
      stop("n is not a field name from data")
    }
    if (!deparse(substitute(stdpop)) %in% colnames(data)) {
      stop("stdpop is not a field name from data")
    }
    data <- data %>% rename(x = {
      {
        x
      }
    }, n = {
      {
        n
      }
    }, stdpop = {
      {
        stdpop
      }
    })
    if (!is.numeric(data$x)) {
      stop("field x must be numeric")
    }
    else if (!is.numeric(data$n)) {
      stop("field n must be numeric")
    }
    else if (!is.numeric(data$stdpop)) {
      stop("field stdpop must be numeric")
    }
    else if (anyNA(data$n)) {
      stop("field n cannot have missing values")
    }
    else if (anyNA(data$stdpop)) {
      stop("field stdpop cannot have missing values")
    }
    else if (any(pull(data, x) < 0, na.rm = TRUE)) {
      stop("numerators must all be greater than or equal to zero")
    }
    else if (any(pull(data, n) <= 0)) {
      stop("denominators must all be greater than zero")
    }
    else if (any(pull(data, stdpop) < 0)) {
      stop("stdpop must all be greater than or equal to zero")
    }
    else if (!(type %in% c("value", "lower", "upper",
                           "standard", "full"))) {
      stop("type must be one of value, lower, upper, standard or full")
    }
    else if (!is.numeric(confidence)) {
      stop("confidence must be numeric")
    }
    else if (length(confidence) > 2) {
      stop("a maximum of two confidence levels can be provided")
    }
    else if (length(confidence) == 2) {
      if (!(confidence[1] == 0.95 & confidence[2] == 0.998)) {
        stop("two confidence levels can only be produced if they are specified as 0.95 and 0.998")
      }
    }
    else if ((confidence < 0.9) | (confidence > 1 & confidence <
                                   90) | (confidence > 100)) {
      stop("confidence level must be between 90 and 100 or between 0.9 and 1")
    }
    else if (!is.numeric(multiplier)) {
      stop("multiplier must be numeric")
    }
    else if (multiplier <= 0) {
      stop("multiplier must be greater than 0")
    }
    else if (!rlang::is_bool(independent_events)) {
      stop("independent_events must be TRUE or FALSE")
    }
    if (!independent_events) {
      if (missing(eventfreq)) {
        stop(paste0("function calculate_dsr requires an eventfreq column ",
                    "to be specified when independent_events is FALSE"))
      }
      else if (!deparse(substitute(eventfreq)) %in% colnames(data)) {
        stop("eventfreq is not a field name from data")
      }
      else if (!is.numeric(data[[deparse(substitute(eventfreq))]])) {
        stop("eventfreq field must be numeric")
      }
      else if (anyNA(data[[deparse(substitute(eventfreq))]])) {
        stop("eventfreq field must not have any missing values")
      }
      if (missing(ageband)) {
        stop(paste0("function calculate_dsr requires an ageband column ",
                    "to be specified when independent_events is FALSE"))
      }
      else if (!deparse(substitute(ageband)) %in% colnames(data)) {
        stop("ageband is not a field name from data")
      }
      else if (anyNA(data[[deparse(substitute(ageband))]])) {
        stop("ageband field must not have any missing values")
      }
    }
    if (independent_events) {
      dsrs <- dsr_inner2(data = data, x = x, n = n, stdpop = stdpop,
                         type = type, confidence = confidence, multiplier = multiplier)
    }
    else {
      data <- data %>% rename(eventfreq = {
        {
          eventfreq
        }
      }, ageband = {
        {
          ageband
        }
      }) %>% group_by(eventfreq, .add = TRUE)
      grps <- group_vars(data)[!group_vars(data) %in% "eventfreq"]
      check_groups <- filter(summarise(group_by(data, pick(all_of(c(grps,
                                                                    "ageband")))), num_n = n_distinct(.data$n),
                                       num_stdpop = n_distinct(.data$stdpop), .groups = "drop"),
                             .data$num_n > 1 | .data$num_stdpop > 1)
      if (nrow(check_groups) > 0) {
        stop(paste0("There are rows with the same grouping variables and ageband",
                    " but with different populations (n) or standard populations",
                    "(stdpop)"))
      }
      freq_var <- data %>% dsr_inner2(x = x, n = n, stdpop = stdpop,
                                      type = type, confidence = confidence, multiplier = multiplier,
                                      rtn_nonindependent_vardsr = TRUE) %>% mutate(freqvars = .data$vardsr *
                                                                                     .data$eventfreq^2) %>% group_by(pick(all_of(grps))) %>%
        summarise(custom_vardsr = sum(.data$freqvars), .groups = "drop")
      event_data <- data %>% mutate(events = .data$eventfreq *
                                      .data$x) %>% group_by(pick(all_of(c(grps, "ageband",
                                                                          "n", "stdpop")))) %>% summarise(x = sum(.data$events,
                                                                                                                  na.rm = TRUE), .groups = "drop")
      dsrs <- event_data %>% left_join(freq_var, by = grps) %>%
        group_by(pick(all_of(grps))) %>% dsr_inner2(x = x,
                                                    n = n, stdpop = stdpop, type = type, confidence = confidence,
                                                    multiplier = multiplier, use_nonindependent_vardsr = TRUE)
    }
    return(dsrs)
  }



# calculate_dsr3 ---------------------------------------------------------------
# This function allows the multiplier argument to be passed as a column name

calculate_dsr3 <-
  function (data, x, n, stdpop = NULL, type = "full", confidence = 0.95,
            multiplier = 1e+05, independent_events = TRUE, eventfreq = NULL,
            ageband = NULL) {
    
    # ---- basic argument checks ----
    if (missing(data) | missing(x) | missing(n) | missing(stdpop)) {
      stop("function calculate_dsr requires at least 4 arguments: data, x, n, stdpop")
    }
    if (!is.data.frame(data)) stop("data must be a data frame object")
    if (!deparse(substitute(x)) %in% colnames(data)) stop("x is not a field name from data")
    if (!deparse(substitute(n)) %in% colnames(data)) stop("n is not a field name from data")
    if (!deparse(substitute(stdpop)) %in% colnames(data)) stop("stdpop is not a field name from data")
    
    # ---- standardise core columns ----
    data <- dplyr::rename(
      data,
      x      = {{ x }},
      n      = {{ n }},
      stdpop = {{ stdpop }}
    )
    
    if (!is.numeric(data$x))      stop("field x must be numeric")
    else if (!is.numeric(data$n)) stop("field n must be numeric")
    else if (!is.numeric(data$stdpop)) stop("field stdpop must be numeric")
    else if (anyNA(data$n))       stop("field n cannot have missing values")
    else if (anyNA(data$stdpop))  stop("field stdpop cannot have missing values")
    else if (any(dplyr::pull(data, x) < 0, na.rm = TRUE)) stop("numerators must all be >= 0")
    else if (any(dplyr::pull(data, n) <= 0))              stop("denominators must all be > 0")
    else if (any(dplyr::pull(data, stdpop) < 0))          stop("stdpop must all be >= 0")
    else if (!(type %in% c("value", "lower", "upper", "standard", "full")))
      stop("type must be one of value, lower, upper, standard or full")
    else if (!is.numeric(confidence))
      stop("confidence must be numeric")
    else if (length(confidence) > 2)
      stop("a maximum of two confidence levels can be provided")
    else if (length(confidence) == 2) {
      if (!(confidence[1] == 0.95 & confidence[2] == 0.998)) {
        stop("two confidence levels can only be produced if they are specified as 0.95 and 0.998")
      }
    } else if ((confidence < 0.9) | (confidence > 1 & confidence < 90) | (confidence > 100)) {
      stop("confidence level must be between 90 and 100 or between 0.9 and 1")
    }
    
    # ---- allow multiplier to be a column OR a scalar ----
    mult_is_col <- deparse(substitute(multiplier)) %in% colnames(data)
    
    if (mult_is_col) {
      data <- dplyr::rename(data, multiplier = {{ multiplier }})
      if (!is.numeric(data$multiplier)) stop("multiplier column must be numeric")
      if (anyNA(data$multiplier))       stop("multiplier column cannot have missing values")
      if (any(data$multiplier <= 0))    stop("multiplier column values must be > 0")
    } else {
      # scalar path -> inject as a column so downstream is uniform
      if (!is.numeric(multiplier)) stop("multiplier must be numeric")
      if (length(multiplier) != 1) stop("multiplier must be length 1 when not a column")
      if (multiplier <= 0)         stop("multiplier must be greater than 0")
      data <- dplyr::mutate(data, multiplier = multiplier)
    }
    
    # ---- handle independent / non-independent events ----
    if (!rlang::is_bool(independent_events)) {
      stop("independent_events must be TRUE or FALSE")
    }
    
    if (independent_events) {
      dsrs <- dsr_inner2(
        data = data, x = x, n = n, stdpop = stdpop,
        type = type, confidence = confidence,
        rtn_nonindependent_vardsr = FALSE,
        use_nonindependent_vardsr = FALSE
      )
    } else {
      # validate eventfreq and ageband
      if (missing(eventfreq)) {
        stop("function calculate_dsr requires an eventfreq column when independent_events is FALSE")
      } else if (!deparse(substitute(eventfreq)) %in% colnames(data)) {
        stop("eventfreq is not a field name from data")
      } else if (!is.numeric(data[[deparse(substitute(eventfreq))]])) {
        stop("eventfreq field must be numeric")
      } else if (anyNA(data[[deparse(substitute(eventfreq))]])) {
        stop("eventfreq field must not have any missing values")
      }
      
      if (missing(ageband)) {
        stop("function calculate_dsr requires an ageband column when independent_events is FALSE")
      } else if (!deparse(substitute(ageband)) %in% colnames(data)) {
        stop("ageband is not a field name from data")
      } else if (anyNA(data[[deparse(substitute(ageband))]])) {
        stop("ageband field must not have any missing values")
      }
      
      data <- data %>%
        dplyr::rename(
          eventfreq = {{ eventfreq }},
          ageband   = {{ ageband }}
        ) %>%
        dplyr::group_by(eventfreq, .add = TRUE)
      
      grps <- dplyr::group_vars(data)[!dplyr::group_vars(data) %in% "eventfreq"]
      
      check_groups <- dplyr::group_by(data, dplyr::pick(dplyr::all_of(c(grps, "ageband")))) %>%
        dplyr::summarise(
          num_n      = dplyr::n_distinct(.data$n),
          num_stdpop = dplyr::n_distinct(.data$stdpop),
          .groups = "drop"
        ) %>%
        dplyr::filter(.data$num_n > 1 | .data$num_stdpop > 1)
      
      if (nrow(check_groups) > 0) {
        stop(paste0("There are rows with the same grouping variables and ageband",
                    " but with different populations (n) or standard populations (stdpop)"))
      }
      
      # vardsr under non-independent events (multiplier not required here)
      freq_var <- data %>%
        dsr_inner2(
          x = x, n = n, stdpop = stdpop, type = type,
          confidence = confidence,
          rtn_nonindependent_vardsr = TRUE,
          use_nonindependent_vardsr = FALSE
        ) %>%
        dplyr::mutate(freqvars = .data$vardsr * .data$eventfreq^2) %>%
        dplyr::group_by(dplyr::pick(dplyr::all_of(grps))) %>%
        dplyr::summarise(custom_vardsr = sum(.data$freqvars), .groups = "drop")
      
      # collapse events and retain multiplier per group
      event_data <- data %>%
        dplyr::mutate(events = .data$eventfreq * .data$x) %>%
        dplyr::group_by(dplyr::pick(dplyr::all_of(c(grps, "ageband", "n", "stdpop")))) %>%
        dplyr::summarise(
          x = sum(.data$events, na.rm = TRUE),
          multiplier = dplyr::first(.data$multiplier),
          .groups = "drop"
        )
      
      dsrs <- event_data %>%
        dplyr::left_join(freq_var, by = grps) %>%
        dplyr::group_by(dplyr::pick(dplyr::all_of(grps))) %>%
        dsr_inner2(
          x = x, n = n, stdpop = stdpop, type = type,
          confidence = confidence,
          rtn_nonindependent_vardsr = FALSE,
          use_nonindependent_vardsr = TRUE
        )
    }
    
    return(dsrs)
  }

# dsr_inner2 --------------------------------------------------------------------
dsr_inner2 <-
  function (data, x, n, stdpop, type, confidence,
            rtn_nonindependent_vardsr = FALSE,
            use_nonindependent_vardsr = FALSE) {
    
    if (isTRUE(rtn_nonindependent_vardsr) &&
        ("custom_vardsr" %in% names(data) || isTRUE(use_nonindependent_vardsr))) {
      stop("cannot get nonindependent vardsr and use nonindependent vardsr in the same execution")
    }
    
    # normalise confidence inputs (allow 95 and 0.95 forms)
    confidence[confidence >= 90] <- confidence[confidence >= 90] / 100
    conf1 <- confidence[1]
    conf2 <- confidence[2]
    
    # ensure multiplier column present & valid
    if (!"multiplier" %in% names(data)) {
      stop("internal error: multiplier column not found. Ensure calculate_dsr2() injected/renamed it.")
    }
    if (!is.numeric(data$multiplier)) stop("multiplier column must be numeric")
    if (anyNA(data$multiplier))       stop("multiplier column cannot have missing values")
    if (any(data$multiplier <= 0))    stop("multiplier column values must be > 0")
    
    # multiplier must be constant within groups
    grps <- dplyr::group_vars(data)
    if (length(grps) > 0) {
      mult_check <- dplyr::summarise(data, nuniq_mult = dplyr::n_distinct(.data$multiplier), .groups = "drop")
      if (any(mult_check$nuniq_mult > 1)) stop("Within-group multiplier must be constant.")
    }
    
    # choose method
    if (!isTRUE(use_nonindependent_vardsr)) {
      method <- "Dobson"
      data <- dplyr::mutate(data, custom_vardsr = NA_real_)
    } else {
      method <- "Dobson, with confidence adjusted for non-independent events"
    }
    
    dsrs <- data %>%
      dplyr::mutate(
        wt_rate = PHEindicatormethods:::na.zero(.data$x) * .data$stdpop / .data$n,
        sq_rate = PHEindicatormethods:::na.zero(.data$x) * (.data$stdpop / (.data$n))^2
      ) %>%
      dplyr::summarise(
        total_count = sum(.data$x, na.rm = TRUE),
        total_pop   = sum(.data$n),
        base_value  = sum(.data$wt_rate) / sum(.data$stdpop),
        vardsr = if (isTRUE(use_nonindependent_vardsr)) {
          unique(.data$custom_vardsr)
        } else {
          1 / sum(.data$stdpop)^2 * sum(.data$sq_rate)
        },
        # vardsr = dplyr::case_when(
        #   isTRUE(use_nonindependent_vardsr) ~ unique(.data$custom_vardsr),
        #   TRUE ~ 1 / sum(.data$stdpop)^2 * sum(.data$sq_rate)
        # ),
        multiplier = dplyr::first(.data$multiplier),
        .groups = "keep"
      )
    
    if (!isTRUE(rtn_nonindependent_vardsr)) {
      dsrs <- dsrs %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          value    = .data$base_value * .data$multiplier,
          lowercl  = .data$value + sqrt(.data$vardsr/.data$total_count) *
            (PHEindicatormethods:::byars_lower(.data$total_count, conf1) - .data$total_count) *
            .data$multiplier,
          uppercl  = .data$value + sqrt(.data$vardsr/.data$total_count) *
            (PHEindicatormethods:::byars_upper(.data$total_count, conf1) - .data$total_count) *
            .data$multiplier,
          lower99_8cl = .data$value + sqrt(.data$vardsr/.data$total_count) *
            (PHEindicatormethods:::byars_lower(.data$total_count, 0.998) - .data$total_count) *
            .data$multiplier,
          upper99_8cl = .data$value + sqrt(.data$vardsr/.data$total_count) *
            (PHEindicatormethods:::byars_upper(.data$total_count, 0.998) - .data$total_count) *
            .data$multiplier,
          confidence = paste0(confidence * 100, "%", collapse = ", "),
          statistic  = paste("dsr per", format(.data$multiplier, scientific = FALSE)),
          method     = method
        )
      
      # rename/drops to mirror PHE functions' dual-CI behavior
      if (!is.na(conf2)) {
        names(dsrs)[names(dsrs) == "lowercl"] <- "lower95_0cl"
        names(dsrs)[names(dsrs) == "uppercl"] <- "upper95_0cl"
      } else {
        dsrs <- dplyr::select(dsrs, !c("lower99_8cl", "upper99_8cl"))
      }
    }
    
    if (isTRUE(rtn_nonindependent_vardsr)) {
      dsrs <- dplyr::select(dsrs, dplyr::group_cols(), "vardsr")
    } else if (type == "lower") {
      dsrs <- dplyr::select(dsrs, !c("total_count","total_pop","value",
                                     dplyr::starts_with("upper"),
                                     "vardsr","confidence","statistic","method"))
    } else if (type == "upper") {
      dsrs <- dplyr::select(dsrs, !c("total_count","total_pop","value",
                                     dplyr::starts_with("lower"),
                                     "vardsr","confidence","statistic","method"))
    } else if (type == "value") {
      dsrs <- dplyr::select(dsrs, !c("total_count","total_pop",
                                     dplyr::starts_with("lower"), dplyr::starts_with("upper"),
                                     "vardsr","confidence","statistic","method"))
    } else if (type == "standard") {
      dsrs <- dplyr::select(dsrs, !c("vardsr","confidence","statistic","method"))
    } else if (type == "full") {
      dsrs <- dplyr::select(dsrs, !c("vardsr"))
    }
    
    return(dsrs)
  }

stat_mode <- function(x) {
  tx <- table(x, useNA = "no")              # count frequency of each unique value
  if (!length(tx)) return(NA_character_)    # handle empty input gracefully
  names(tx)[which.max(tx)]                  # return the name (value) with max frequency
}

# Function to calculate difference ---------------------------------------

calc_difference <- function(df_in){
  
  df_in |> 
    filter(value_type_code %in% c(10, 11)) |> # 10 = Percentage point difference, 11 = Crude difference
    mutate(
      value = numerator - denominator,
      lowercl = NA_real_,
      uppercl = NA_real_
    )
}

# Function to calculate percentage change ---------------------------------------

calc_percentage_change <- function(df_in){
  
  df_in |> 
    filter(value_type_code == 9) |> 
    mutate(
      value = if_else(
        denominator == 0 | is.na(denominator),
        NA_real_, # ignore 0/NA denominator as sometimes there are no plan/previous/baseline values
        (numerator - denominator) / denominator * value_multiplier 
      ),
      lowercl = NA_real_,
      uppercl = NA_real_
    )
}

# Function to calculate percentage ---------------------------------------------

calc_percentage <- function(df_in){
  
  # Inputs:
  #   df_in - dataframe eligible for processing, containing `numerator`, `denominator`,
  #           `value_multiplier`, and `value_type_code` (2 = percentage)
  # Output:
  #   The same rows (where value_type_code == 2) with percentage and 95% CI
  #   columns added by PHEindicatormethods::phe_proportion:
  #   `value`, `lowercl`, `uppercl`, `method`
  
  df_perc <- df_in %>%
    filter(value_type_code == 2)
  
  df_proportion <- df_perc |> 
    filter(!is.na(denominator),
           denominator != 0,
           numerator <= denominator)
  
  multiplier_value <- df_proportion$value_multiplier[1]
  
  df_proportion <- df_proportion |> 
    phe_proportion(
      x = numerator,
      n = denominator,
      confidence = 0.95,
      type = "standard",
      multiplier = multiplier_value
    )
  
  # Achievement against target, growth or increase, or repeat events
  df_other_percentage <- df_perc |> 
    filter(is.na(denominator) |
             denominator == 0 |
             numerator > denominator) |> 
    mutate(
      value = if_else(
        !is.na(denominator) & denominator != 0,
        (numerator / denominator) * value_multiplier,
        NA_real_
      ),
      lowercl = NA_real_,
      uppercl = NA_real_,
    )
  
  output <- bind_rows(
    df_proportion,
    df_other_percentage
  )
  
  return(output)
  
}

# Function to calculate count --------------------------------------------------
calc_count <- function(df_in){
  
  output <- df_in |>
    filter(value_type_code == 1) |>  
    mutate(
      value = numerator,
      lowercl = NA_real_,
      uppercl = NA_real_,
      method = NA_character_
    )
  
  return(output)
  
}

calc_count_poisson <- function(df_in){
  
  output <- df_in |>
    filter(value_type_code == 1) |>
    mutate(
      indicator_value = numerator,
      lowercl = qchisq(0.025, 2 * numerator) / 2,
      uppercl = qchisq(0.975, 2 * (numerator + 1)) / 2,
      method = "Poisson exact"
    )
  
  return(output)
  
}

# Function to calculate crude or ratio -----------------------------------------

calc_crude_or_ratio <- function(df_in){
  
  # Inputs:
  #   df_in - dataframe eligible for processing, containing `numerator`, `denominator`,
  #           `value_multiplier`, and `value_type_code` (3 = crude rate, 7 = ratio)
  # Output:
  #   The same rows (where value_type_code is either 3 or 7) with crude rates or ratios calculated
  #   and 95% CI columns added by PHEindicatormethods::phe_rate:
  #   `value`, `lowercl`, `uppercl`, `method`
  
  df_rate <- df_in |>
    filter(value_type_code %in% c(3, 7)) |>
    mutate(
      num_sign = sign(coalesce(numerator, 0)),
      num_abs  = abs(coalesce(numerator, 0))
    )
  
  output <-  df_rate |>
    phe_rate(
      x = num_abs,
      n = denominator,
      confidence = 0.95,
      type = "standard",
      multiplier = df_rate$value_multiplier
    ) |>
    mutate(numerator = num_abs * num_sign,
           value = value * num_sign,
           lower_tmp = pmin(lowercl, uppercl),
           upper_tmp = pmax(lowercl, uppercl),
           lowercl   = if_else(num_sign < 0, -upper_tmp, lower_tmp),
           uppercl   = if_else(num_sign < 0, -lower_tmp, upper_tmp)) |>
    select(-lower_tmp, -upper_tmp)
  
  
  return(output)
}

# Function to calculate dasr ---------------------------------------------------

calc_dasr <- function(df_in, metadata, age_lookup){
  
  # Inputs:
  #   df_in      - dataframe containing `numerator`, `denominator`, `value_multiplier`,
  #                and `value_type_code` (4 = DASR)
  #   metadata   - dataframe containing `indicator_id` and `age` columns,
  #                used to derive the final age code
  #   age_lookup - dataframe containing age lookups (e.g., `age_code`,
  #                `age_group_label`)
  #
  # Output:
  #   A dataframe filtered to rows where `value_type_code == 4`, with DASR
  #   calculated and 95% confidence interval columns added using
  #   `calculate_dsr3` adapted from pheindicatormethods (utils.R). The output also
  #   includes the correct `age_group_code` derived from `metadata` and
  #   `age_lookup`.
  
  esp2013_lookup <- PHEindicatormethods::esp2013 |>
    as_tibble() |>
    rename(stdpop = value) |>
    mutate(age_group_code = as.integer(c(1:18, 18))) |>
    group_by(age_group_code) |>
    summarise(stdpop = sum(stdpop), .groups = "drop")
  
  dasr_keys <- c(
    "indicator_id","start_date","end_date","imd_code","aggregation_id", "sex_code",
    "ethnicity_code","creation_date", "value_type_code","source_code","time_period_type", "combination_id"
  )
  
  df_calc <- df_in |>
    filter(value_type_code == 4) |>
    left_join(esp2013_lookup, by = "age_group_code")
  
  output <- df_calc |>
    group_by(across(all_of(dasr_keys))) |>
    calculate_dsr3(
      x = numerator,
      n = denominator,
      stdpop = stdpop,
      type = "standard",
      multiplier = value_multiplier # this function accept either value_multiplier or a scalar value e.g., 100000
    ) |>
    ungroup() |>
    left_join(get_age_lookup(metadata = metadata, age_lookup = age_lookup), by = "indicator_id") |>
    mutate(age_group_code = age_code)
  
  
  return(output)
  
}

# Function to calculate SII ----------------------------------------------------
#' Calculate the Slope Index of Inequality
#'
#' Calculates the Slope Index of Inequality (SII) for a binary outcome measured
#' across five deprivation quintiles. A separate inverse-variance-weighted
#' linear regression model is fitted for each combination of the supplied
#' grouping columns.
#'
#' Each grouping level must contain exactly one row for each deprivation
#' quintile from 1 to 5. The numerator and denominator are used to calculate
#' the outcome proportion within each quintile.
#'
#' @section Calculation:
#'
#' For each deprivation quintile, the outcome proportion is calculated as:
#'
#' \deqn{p = \frac{numerator}{denominator}}
#'
#' A weighted linear regression is then fitted:
#'
#' \deqn{p = intercept + slope \times deprivation\ position}
#'
#' The regression uses the five quintile-specific proportions rather than a
#' single pooled proportion.
#'
#' @section Deprivation scaling:
#'
#' Deprivation quintiles are assigned evenly spaced relative deprivation
#' positions:
#'
#' \itemize{
#'   \item Quintile 1, most deprived: 0.9
#'   \item Quintile 2: 0.7
#'   \item Quintile 3: 0.5
#'   \item Quintile 4: 0.3
#'   \item Quintile 5, least deprived: 0.1
#' }
#'
#' The scale runs from the least deprived population at 0.1 to the most
#' deprived population at 0.9. The fitted regression line is extrapolated to
#' positions 0 and 1 to estimate the outcome at the least and most deprived
#' population extremes.
#'
#' Because the full deprivation scale runs from 0 to 1, the regression slope
#' represents the modelled difference in the outcome proportion between the
#' most and least deprived population extremes.
#'
#' @section Regression weighting:
#'
#' Each quintile is weighted using the inverse of its estimated binomial
#' variance:
#'
#' \deqn{variance = \frac{p(1-p)}{n}}
#'
#' \deqn{weight = \frac{1}{variance}}
#'
#' where \eqn{p} is the quintile-specific outcome proportion and \eqn{n} is
#' its denominator.
#'
#' Quintile estimates with greater statistical precision receive more weight
#' in the regression. Estimates based on smaller samples generally have larger
#' variance and therefore receive less weight.
#'
#' @section Interpretation:
#'
#' The signed SII is the regression slope multiplied by 100 and is expressed
#' in percentage points:
#'
#' \deqn{signed\ SII = slope \times 100}
#'
#' Under the scaling used by this function:
#'
#' \itemize{
#'   \item a positive signed SII indicates that the modelled outcome is higher
#'   at the most deprived extreme;
#'   \item a negative signed SII indicates that the modelled outcome is lower
#'   at the most deprived extreme;
#'   \item a signed SII close to zero indicates little modelled socioeconomic
#'   gradient.
#' }
#'
#' The returned \code{indicator_value} is the absolute SII:
#'
#' \deqn{absolute\ SII = |slope \times 100|}
#'
#' It represents the size of the modelled deprivation gap in percentage
#' points, regardless of direction. Whether a higher or lower outcome is
#' favourable depends on the indicator being analysed.
#'
#' The SII is not simply the observed difference between deprivation quintiles
#' 1 and 5. It uses a weighted regression fitted across all five quintiles.
#'
#' @section Output decisions:
#'
#' The function returns one row for each grouping level instead of one row for
#' each deprivation quintile.
#'
#' The standard output is constructed using the following decisions:
#'
#' \itemize{
#'   \item \code{numerator} is pooled by summing the numerators across all five
#'   deprivation quintiles;
#'
#'   \item \code{denominator} is pooled by summing the denominators across all
#'   five deprivation quintiles;
#'
#'   \item \code{indicator_value} is assigned the absolute SII expressed in
#'   percentage points;
#'
#'   \item \code{lower_ci95} and \code{upper_ci95} are assigned
#'   \code{NA_real_} because confidence intervals are not currently calculated;
#'
#'   \item \code{imd_code} is assigned \code{999} because the result uses all
#'   five deprivation quintiles and does not represent an individual quintile;
#'
#'   \item the remaining grouping and metadata columns are retained from the
#'   input data.
#' }
#'
#' The pooled numerator and denominator describe the total observations
#' included across the five quintiles. They are not used as a single pooled
#' rate when calculating the SII. The SII is calculated from the five separate
#' quintile-specific proportions and their weights.
#'
#' @param data A data frame containing one row per deprivation quintile for
#'   each grouping level. The function currently retains rows where
#'   \code{value_type_code == 14}.
#'
#' @param group_cols A character vector containing the columns that uniquely
#'   define one SII result. All five quintile rows within a grouping level must
#'   contain identical values in these columns.
#'
#' @param quintile_col The name of the deprivation quintile column. Defaults
#'   to \code{"imd_code"}. Values must be convertible to integers from 1 to 5,
#'   where 1 represents the most deprived quintile and 5 represents the least
#'   deprived quintile.
#'
#' @param numerator_col The name of the numerator column. Defaults to
#'   \code{"numerator"}.
#'
#' @param denominator_col The name of the denominator column. Defaults to
#'   \code{"denominator"}.
#'
#' @return A tibble with one row per SII grouping level containing:
#'
#' \describe{
#'   \item{\code{indicator_id}}{Indicator identifier.}
#'   \item{\code{start_date}}{Start date of the reporting period.}
#'   \item{\code{end_date}}{End date of the reporting period.}
#'   \item{\code{numerator}}{Pooled numerator across quintiles 1 to 5.}
#'   \item{\code{denominator}}{Pooled denominator across quintiles 1 to 5.}
#'   \item{\code{indicator_value}}{Absolute SII in percentage points.}
#'   \item{\code{lower_ci95}}{Missing numeric value because confidence
#'   intervals are not currently calculated.}
#'   \item{\code{upper_ci95}}{Missing numeric value because confidence
#'   intervals are not currently calculated.}
#'   \item{\code{imd_code}}{Set to 999 to represent all deprivation quintiles.}
#'   \item{\code{aggregation_id}}{Organisation or geography identifier.}
#'   \item{\code{age_group_code}}{Age-group identifier.}
#'   \item{\code{sex_code}}{Sex identifier.}
#'   \item{\code{ethnicity_code}}{Ethnicity identifier.}
#'   \item{\code{creation_date}}{Creation date retained from the input group.}
#'   \item{\code{value_type_code}}{Value-type identifier for the SII measure.}
#'   \item{\code{source_code}}{Data-source identifier.}
#' }
#'
#' @details
#' Before fitting the models, the function validates that:
#'
#' \itemize{
#'   \item all required columns exist;
#'   \item numerator and denominator columns are numeric and non-missing;
#'   \item denominators are greater than zero;
#'   \item numerators are non-negative and do not exceed denominators;
#'   \item each grouping level contains exactly one row for each deprivation
#'   quintile from 1 to 5;
#'   \item the calculated variance values are positive and finite.
#' }
#'
#' The function stops when a quintile-specific proportion is exactly 0 or 1
#' because its estimated binomial variance is zero and the inverse-variance
#' weight is therefore undefined.
#'
#' @examples
#' \dontrun{
#' sii_results <- calculate_sii(
#'   data = staging_data
#' )
#'
#' sii_results <- calculate_sii(
#'   data = my_data,
#'   quintile_col = "imd_code",
#'   numerator_col = "numerator",
#'   denominator_col = "denominator"
#' )
#' }
#'
#' @export
calculate_sii <- function(data, group_cols = c("indicator_id", "start_date", "end_date",
                                               "aggregation_id", "age_group_code", "sex_code",
                                               "ethnicity_code", "value_type_code", "source_code", "creation_date",
                                               "time_period_type", "combination_id"),
                          quintile_col = "imd_code", numerator_col = "numerator", denominator_col = "denominator"){
  
  #1. Check inputs
  
  # Missing function arguments
  if (missing(data)) {
    stop("`data` must be supplied to calculate_sii().")
  }
  
  
  data <- data |> 
    dplyr::filter(value_type_code == 14)
  
  # Capture the supplied column names
  group_names <- group_cols # already a character vector
  quintile_name <- rlang::as_name(rlang::ensym(quintile_col))
  numerator_name <- rlang::as_name(rlang::ensym(numerator_col))
  denominator_name <- rlang::as_name(rlang::ensym(denominator_col))
  
  required_columns <- c(group_names, quintile_name, numerator_name, denominator_name)
  
  missing_columns <- setdiff(required_columns, names(data))
  
  if(length(missing_columns) > 0){
    stop("The following supplied columns do not exist in `data`: ", 
         paste(missing_columns, collapse = ", "))
  }
  
  # Extract the supplied columns needed to calculate SII
  quintiles <- suppressWarnings(
    as.integer(as.character(data[[quintile_name]])) # Convert IMD quintiles to integer if in character
  ) 
  numerators <- data[[numerator_name]]
  denominators <- data[[denominator_name]]
  
  # Check for missing numerators or denominators
  if(anyNA(numerators) || anyNA(denominators)){
    stop("Numerator and denominator cannot contain missing values.")
  }
  
  # Check for non-numeric numerators and denominators
  if(!is.numeric(numerators) || !is.numeric(denominators)){
    stop("Numerators and denominators must be numeric columns.")
  }
  
  # Check for negative denominators
  if(any(denominators <= 0)){
    stop("All denominators must be greater than zero.")
  }
  
  # Check for negative numerators
  if(any(numerators < 0)){
    stop("Numerators cannot be negative.")
  }
  
  # Check for invalid numerators being greater than denominators
  if(any(numerators > denominators)){
    stop("The numerator cannot be greater than the denominator.")
  }
  
  # Check quintiles within every grouping level
  quintile_check <- data |> 
    dplyr::mutate(
      .quintile = quintiles
    ) |> 
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group_names))
    ) |> 
    dplyr::summarise(
      row_count = dplyr::n(),
      distinct_quintiles = dplyr::n_distinct(.quintile),
      has_missing_quintile = anyNA(.quintile),
      duplicated_quintile = anyDuplicated(.quintile) > 0,
      correct_quintile_set = setequal(.quintile, 1:5),
      quintiles_found = paste(sort(unique(.quintile)), collapse = ", "),
      .groups = "drop"
    ) |> 
    dplyr::mutate(
      valid_quintiles = row_count == 5 & 
        distinct_quintiles == 5 &
        !has_missing_quintile &
        !duplicated_quintile &
        correct_quintile_set
      
    )
  
  invalid_groups <- quintile_check |>
    dplyr::filter(!valid_quintiles)
  
  if (nrow(invalid_groups) > 0) {
    
    # Print invalid groups during interactive use, but not during automated tests
    if (interactive()) {
      print(invalid_groups)
    }
    
    stop(
      nrow(invalid_groups),
      " grouping level(s) do not contain exactly one row ",
      "for each IMD quintile 1, 2, 3, 4 and 5."
    )
  }
  
  #2. Create variables needed to fit a linear model
  model_data <- data |> 
    dplyr::mutate(
      deprivation_position = dplyr::recode(
        quintiles,
        `1` = 0.9, # Most deprived
        `2` = 0.7,
        `3` = 0.5,
        `4` = 0.3,
        `5` = 0.1 # Least deprived
      ),
      proportion = numerators / denominators,
      variance = proportion * (1 - proportion) / denominators
    )
  
  if (any(model_data$variance == 0)){
    stop(
      "At least one quintile has variance equal to zero. ",
      "This occurs when the proportion is exactly 0 or 1."
    )
  }
  
  if(any(!is.finite(model_data$variance)) || any(model_data$variance < 0)){
    stop("One or more variance values are invalid.")
  }
  
  #3. Calculate inverse variance weight
  model_data <- model_data |> 
    dplyr::mutate(
      inverse_variance_weight = 1 / variance
    )
  
  #4. Create one nested dataset per grouping level
  model_data <- model_data |> 
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group_names))
    ) |>
    tidyr::nest()
  
  #5. Fit one linear model per grouping level
  model_data <- model_data |> 
    dplyr::mutate(
      model = purrr::map(
        .data$data,
        ~ stats::lm(
          proportion ~ deprivation_position,
          data = .x,
          weights = .x$inverse_variance_weight
        )
      )
    )
  
  #6. Predict the deprivation extremes for each model
  model_data <- model_data |> 
    dplyr::mutate(
      predictions = purrr::map(
        model, 
        ~ stats::predict(
          .x,
          newdata = tibble::tibble(
            deprivation_position = c(0,1)
          )
        )
      )
    )
  
  #7. Extract slope and intercept
  model_data <- model_data |> 
    dplyr::mutate(
      slope = purrr::map_dbl(
        model,
        ~ unname(
          stats::coef(.x)[["deprivation_position"]]
        )
      ),
      intercept =  purrr::map_dbl(
        model,
        ~ unname(
          stats::coef(.x)[["(Intercept)"]]
        )
      )
    )
  
  #8. Create SII measures and pooled counts
  model_data <- model_data |> 
    dplyr::mutate(
      numerator = purrr::map_dbl(
        data,
        ~ sum(.x[[numerator_name]], na.rm = TRUE)
      ),
      denominator = purrr::map_dbl(
        data,
        ~ sum(.x[[denominator_name]], na.rm = TRUE)
      ),
      sii_signed_percentage_points = slope * 100,
      sii_absolute_percentage_points = abs(slope * 100),
      predicted_least_deprived_percent = purrr::map_dbl(
        predictions,
        ~ unname(.x[1]) * 100
      ),
      predicted_most_deprived_percent = purrr::map_dbl(
        predictions,
        ~ unname(.x[2]) * 100
      )
    )
  
  #9. Return final output in standard schema
  final_output <- model_data |> 
    dplyr::transmute(
      indicator_id,
      start_date,
      end_date,
      numerator,
      denominator,
      indicator_value = sii_absolute_percentage_points,
      lower_ci95 = NA_real_,
      upper_ci95 = NA_real_,
      imd_code = 999L,
      aggregation_id, 
      age_group_code,
      sex_code,
      ethnicity_code,
      creation_date,
      value_type_code,
      source_code,
      time_period_type,
      combination_id
    ) |> 
    dplyr::ungroup()
  
  return(final_output)
  
}


