#' @keywords internal
#' Converts character objects to date objects
.parse_string_to_date <- function(date_string, date_format) {

  # Check if each argument is of type 'character'
  if (!is.character(date_string)) {
    stop("`date_string` must be of type character.")
  }
  if (!is.character(date_format)) {
    stop("`date_format` must be of type character.")
  }

  # Process the "yyyy" case for as.Date()
  if (date_format == "yyyy" || date_format == "%Y") {
    if (nchar(date_string) != 4) {
      stop(paste("Expected a date format with 4 characters,",
                 "but found", nchar(date_string)))
    }
    date_string <- paste0("01/01/", date_string)
    date_format <- "dd/mm/yyyy"
  }

  # Process the c("mm/yyyy", "mm-yyyy", "mmyyyy") cases for as.Date()
  if (any(date_format %in% c("mm/yyyy", "mm-yyyy", "mmyyyy"))) {
    date_string <- gsub("[/-]", "", date_string)
    date_string <- paste0("01/", substr(date_string, 1, 2), "/",
                          substr(date_string, 3, 6))
    date_format <- "dd/mm/yyyy"
  }

  # Process the "yyyym00" case for as.Date()
  if (date_format == "yyyym00") {
    date_string <- tolower(date_string)
    date_string <- strsplit(date_string, "m")
    date_string_year <- date_string[[1]][1]
    date_string_month <- date_string[[1]][2]
    date_string <- paste0("01/", date_string_month, "/", date_string_year)
    date_format <- "dd/mm/yyyy"
  }

  # Process the "ddmonyyyy" case for as.Date()
  if (date_format == "ddmonyyyy") {
    date_string <- tolower(date_string)
    date_string_day <- substr(date_string, 1, 2)
    date_string_month <- .undid_env$month_dict[substr(date_string, 3, 5)]
    date_string_year <- substr(date_string, 6, 9)
    date_string <- paste0(date_string_day, "/", date_string_month, "/",
                          date_string_year)
    date_format <- "dd/mm/yyyy"
  }

  # Converts general date formats to R specific formatting
  if (date_format %in% .undid_env$date_formats_general) {
    date_format <- .undid_env$date_format_dict_to_r[date_format]
  } else if (!date_format %in% .undid_env$date_formats_r) {
    stop(paste("Please enter a valid date format.",
               "Call `undid_date_formats()` to see a list",
               "of valid date formats."))
  }
  result <- as.Date(date_string, format = date_format)
  if (is.na(result)) {
    stop(paste("Tried converting date string:", date_string, "to date object",
               "using date format of", date_format, "which resulted in `NA`."))
  }
  return(result)
}

#' @keywords internal
#' Converts date objects to character objects
.parse_date_to_string <- function(date, date_format) {
  if (date_format %in% .undid_env$date_formats_r) {
    date_string <- format(date, date_format)
  } else if (date_format %in% .undid_env$date_formats_general) {
    date_string <- format(date, .undid_env$date_format_dict_to_r[date_format])
  } else if (date_format == "yyyy") {
    date_string <- format(date, "%Y")
  } else if (date_format == "mm/yyyy") {
    date_string <- substr(format(date, "%d/%m/%Y"), 4, 10)
  } else if (date_format == "mm-yyyy") {
    date_string <- substr(format(date, "%d-%m-%Y"), 4, 10)
  } else if (date_format == "mmyyyy") {
    date_string <- substr(format(date, "%d%m%Y"), 3, 8)
  } else if (date_format == "yyyym00") {
    date_string <- format(date, "%Y/%m/%d")
    year <- substr(date_string, 1, 4)
    month <- as.character(as.integer(substr(date_string, 6, 7)))
    date_string <- paste0(year, "m", month)
  } else if (date_format == "ddmonyyyy") {
    date_string <- format(date, "%Y/%m/%d")
    year <- substr(date_string, 1, 4)
    month <- .undid_env$month_dict_reverse[as.integer(substr(date_string,
                                                             6, 7))]
    day <- as.character(substr(date_string, 9, 10))
    date_string <- paste0(day, month, year)
  }

  return(date_string)
}

#' @keywords internal
#' Returns `character()` for `covariates` if covariates is `NULL` or `FALSE`
.init_covariates_check <- function(covariates) {
  if (is.null(covariates) || identical(covariates, FALSE) ||
        all(trimws(covariates) == "")) {
    covariates <- character()
  }
  return(covariates)
}

#' @keywords internal
#' Returns positions of dashes or slashes in a date string
.get_dash_or_slash_positions <- function(date_string, dash_or_slash) {
  if (!dash_or_slash %in% c("-", "/")) {
    stop(paste("`dash_or_slash` must be either '-' or '/'. Got",
               dash_or_slash, "instead."))
  }
  if (!(is.character(date_string))) {
    stop(paste("`date_string` must be a character. Got a",
               typeof(date_string), "instead."))
  }
  return(as.integer(gregexpr(dash_or_slash, date_string)[[1]]))
}

#' @keywords internal
#' Checks for consistent use of slashes and dashes in date strings in `init_df`
.slash_or_dash_check <- function(slash_or_dash, sample_date, init_df) {
  expectation <- sum(.get_dash_or_slash_positions(sample_date,
                                                  slash_or_dash)) *
    nrow(init_df)

  position_matrix <- sapply(init_df$start_time,
                            .get_dash_or_slash_positions, slash_or_dash)
  if (sum(unlist(position_matrix)) != expectation) {
    stop("Date formats are not consistent across `start_time`.")
  }
  position_matrix <- sapply(init_df$end_time,
                            .get_dash_or_slash_positions, slash_or_dash)
  if (sum(unlist(position_matrix)) != expectation) {
    stop("Inconsistent date format found in `end_time`.")
  }
  treatments <- init_df[init_df$treatment_time != "control", "treatment_time"]
  position_matrix <- sapply(treatments, .get_dash_or_slash_positions,
                            slash_or_dash)
  expectation <- sum(.get_dash_or_slash_positions(sample_date,
                                                  slash_or_dash)) *
    length(treatments)

  if (sum(unlist(position_matrix)) != expectation) {
    stop("Inconsistent date format found in `treatment_time`.")
  }
}

#' @keywords internal
#' Checks for consistent date formats across the `init.csv`
.init_checks_date_format <- function(init_df) {
  sample_date <- init_df$start_time[1]
  ncharacters <- nchar(sample_date)
  if (any(nchar(init_df$start_time) != ncharacters) ||
        any(nchar(init_df$end_time) != ncharacters)) {
    stop("All dates must be inputted in the same format.")
  }
  non_control_treatments <- init_df[init_df$treatment_time != "control",
                                    "treatment_time"]
  if (any(nchar(non_control_treatments) != ncharacters)) {
    stop("Inconsistent date format found in `treatment_time`.")
  }
  dash_positions <- as.integer(gregexpr("-", sample_date)[[1]])
  slash_positions <- as.integer(gregexpr("/", sample_date)[[1]])
  if (any(dash_positions != -1) && any(slash_positions != -1)) {
    stop("Date format should have use only /'s or -'s, not both.")
  }
  if (all(dash_positions != -1)) {
    .slash_or_dash_check("-", sample_date, init_df)
  }
  if (all(slash_positions != -1)) {
    .slash_or_dash_check("/", sample_date, init_df)
  }
  if (all(dash_positions == -1) && all(slash_positions == -1)) {
    .slash_or_dash_check("/", sample_date, init_df)
  }
}

#' @keywords internal
#' Run `init.csv` logic checks to ensure required structure
.init_checks <- function(init_df) {

  # Check columns
  columns <- colnames(init_df)
  required_columns <- c("silo_name", "start_time", "end_time", "treatment_time")
  if (!all(required_columns %in% columns)) {
    stop(paste0("Columns `silo_name`, `start_time`, `end_time`, ",
                "and `treatment_time` must be present."))
  }
  if (length(columns) > 4) {
    if (length(columns) > 5) {
      stop("More than five columns found.")
    } else {
      if (columns[5] != "covariates") {
        stop("The fifth column should be `covariates`.")
      }
    }
  }

  if (length(unique(init_df$start_time)) > 1) {
    stop("`start_time` must be the same across silos.")
  }

  if (length(unique(init_df$end_time)) > 1) {
    stop("`end_time` must be the same across silos.")
  }

  # If there are any entries in treatment_time, one has to be control
  if (length(init_df$treatment_time) > 0 &&
        !"control" %in% init_df$treatment_time) {
    stop("`treatment_time` must have at least one \"control\" entry.")
  }

  # Check that there is more than one unique silo
  if (nrow(init_df) > 0) {
    if (length(unique(init_df$silo_name)) < 2) {
      stop("There must be at least two unique silos.")
    }

    # Check that the start_time and end_time are written in the same date_format
    .init_checks_date_format(init_df)
  }
}

#' @keywords internal
#' Ensures that start time < treat time < end time
.start_treat_end_time_check <- function(init_df, date_format) {

  # First check start time < end time
  invalid_rows <- init_df[init_df$start_time >= init_df$end_time, ]

  if (nrow(invalid_rows) > 0) {
    invalid_silos <- paste(invalid_rows$silo_name, collapse = ", ")
    stop(paste("Start times must be earlier than end times.",
               "Issues found in:", invalid_silos))
  }

  # Next ensure that start time < treat time < end time
  treated_rows <- init_df[init_df$treatment_time != "control", ]
  treated_rows$treatment_time <- as.Date(vapply(treated_rows$treatment_time,
                                                .parse_string_to_date,
                                                FUN.VALUE = as.Date(NA),
                                                date_format = date_format),
                                         origin = "1970-01-01")
  invalid_rows <- treated_rows[
    treated_rows$start_time >= treated_rows$treatment_time,
  ]
  if (nrow(invalid_rows) > 0) {
    invalid_silos <- paste(invalid_rows$silo_name, collapse = ", ")
    stop(paste("Start times must be earlier than treatment times.",
               "Issues found in:", invalid_silos))
  }
  invalid_rows <- treated_rows[
    treated_rows$treatment_time > treated_rows$end_time,
  ]
  if (nrow(invalid_rows) > 0) {
    invalid_silos <- paste(invalid_rows$silo_name, collapse = ", ")
    stop(paste("Treatment times must be earlier than end times.",
               "Issues found in:", invalid_silos))
  }
}


#' @keywords internal
#' Ensures filepath and filename are formatted properly
.filename_filepath_check <- function(filenames, filepath) {
  for (filename in filenames) {
    if (!is.character(filename)) {
      stop("Please enter a character value for the filename.")
    } else if (!endsWith(filename, ".csv")) {
      stop("Ensure the filename ends with .csv")
    }
  }
  if (!is.character(filepath)) {
    stop("Ensure the custom filepath is entered as a character value.")
  }
  filepath <- normalizePath(filepath, winslash = "/")
  if (!dir.exists(filepath) || !file.access(filepath, 2) == 0) {
    stop("The specified filepath does not exist or is not writable.")
  }
  return(filepath)
}

#' @keywords internal
#' Read in `diff_df` for the second stage and third stages
.read_diff_df <- function(diff_df_filepath, silo_name = NULL, stage = 3) {

  diff_df <- read.csv(diff_df_filepath, header = TRUE, sep = ",",
                      stringsAsFactors = FALSE)
  diff_df$silo_name <- as.character(diff_df$silo_name)

  if (stage == 2) {
    if (!silo_name %in% unique(diff_df$silo_name)) {
      stop(paste(silo_name, "not found in `diff_df$silo_name`.\n",
                 "Only the following silos were found:",
                 paste(unique(diff_df$silo_name), collapse = ", ")))
    }
    diff_df <- diff_df[diff_df$silo_name == silo_name, ]
  }

  diff_df$treat <- as.integer(diff_df$treat)
  diff_df$start_time <- mapply(
    .parse_string_to_date,
    as.character(diff_df$start_time),
    "yyyy-mm-dd"
  )
  diff_df$end_time <- mapply(
    .parse_string_to_date,
    as.character(diff_df$end_time),
    "yyyy-mm-dd"
  )

  if (all(.undid_env$staggered_columns %in% names(diff_df))) {
    diff_df$gvar <- mapply(
      .parse_string_to_date,
      as.character(diff_df$gvar),
      diff_df$date_format
    )
    diff_times_vec <- do.call(rbind, strsplit(diff_df$diff_times, ";"))
    diff_df$diff_times_post <- diff_times_vec[, 1]
    diff_df$diff_times_pre <- diff_times_vec[, 2]
    t_vec <- do.call(rbind, strsplit(diff_df$gt, ";"))
    diff_df$t <- t_vec[, 2]
    diff_df$diff_times_post <- mapply(
      .parse_string_to_date,
      as.character(diff_df$diff_times_post),
      diff_df$date_format
    )
    diff_df$diff_times_pre <- mapply(
      .parse_string_to_date,
      as.character(diff_df$diff_times_pre),
      diff_df$date_format
    )
    diff_df$t <- mapply(.parse_string_to_date, as.character(diff_df$t),
                        diff_df$date_format)

  } else if (all(.undid_env$common_columns %in% names(diff_df))) {
    diff_df$common_treatment_time <- mapply(
      .parse_string_to_date,
      as.character(diff_df$common_treatment_time),
      diff_df$date_format
    )
  } else {
    stop(paste("diff_df does not match any expected structure.",
               "Expected to find either:",
               "- staggered_adoption: 'silo_name', 'gvar', 'treat',",
               "                      'diff_times', 'gt', 'diff_estimate',",
               "                      'diff_var', 'diff_estimate_covariates',",
               "                      'diff_var_covariates', 'covariates',",
               "                      'date_format', 'freq', 'RI'",
               "",
               "- common_adoption: 'silo_name', 'treat',",
               "                   'common_treatment_time', 'start_time',",
               "                   'end_time', 'weights', 'diff_estimate',",
               "                   'diff_var', 'diff_estimate_covariates',",
               "                   'diff_var_covariates', 'covariates',",
               "                   'date_format', 'freq', 'nsilos'",
               sep = "\n"))
  }

  rownames(diff_df) <- NULL

  # Do conversion from Julia missing to R NA if necessary
  diff_df$diff_estimate[diff_df$diff_estimate == "missing"] <- NA_real_
  diff_df$diff_var[diff_df$diff_var == "missing"] <- NA_real_
  diff_df$diff_estimate_covariates[
    diff_df$diff_estimate_covariates == "missing"
  ] <- NA_real_
  diff_df$diff_var_covariates[
    diff_df$diff_var_covariates == "missing"
  ] <- NA_real_
  diff_df$diff_estimate <- as.numeric(diff_df$diff_estimate)
  diff_df$diff_var <- as.numeric(diff_df$diff_var)
  diff_df$diff_estimate_covariates <- as.numeric(
    diff_df$diff_estimate_covariates
  )
  diff_df$diff_var_covariates <- as.numeric(diff_df$diff_var_covariates)

  return(diff_df)
}