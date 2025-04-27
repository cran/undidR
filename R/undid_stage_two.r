#' Runs UNDID stage two procedures
#'
#' Based on the information given in the received `empty_diff_df.csv`,
#' computes the appropriate differences in mean outcomes at the local silo
#' and saves as `filled_diff_df_$silo_name.csv`. Also stores trends data
#' as `trends_data_$silo_name.csv`.
#'
#' @details Covariates at the local silo should be renamed to match the
#' spelling used in the `empty_diff_df.csv`.
#'
#' @param empty_diff_filepath A character filepath to the `empty_diff_df.csv`.
#' @param silo_name A character indicating the name of the local silo. Ensure
#'  spelling is the same as it is written in the `empty_diff_df.csv`.
#' @param silo_df A data frame of the local silo's data. Ensure any covariates
#'  are spelled the same in this data frame as they are in the
#'  `empty_diff_df.csv`.
#' @param time_column A character which indicates the name of the column in
#'  the `silo_df` which contains the date data. Ensure the `time_column`
#'  references a column of character values.
#' @param outcome_column A character which indicates the name of the column in
#'  the `silo_df` which contains the outcome of interest. Ensure the
#'  `outcome_column` references a column of numeric values.
#' @param silo_date_format A character which indicates the date format which
#'  the date strings in the `time_column` are written in.
#' @param consider_covariates An optional logical parameter which if set to
#' `FALSE` ignores any of the computations involving the covariates.
#'  Defaults to `TRUE`.
#' @param filepath Character value indicating the filepath to
#'  save the CSV files. Defaults to `tempdir()`.
#'
#' @returns A list of data frames. The first being the filled differences
#' data frame, and the second being the trends data data frame. Use
#' the suffix $diff_df to access the filled differences data frame, and
#' use $trends_data to access the trends data data frame.
#'
#' @examples
#' # Load data
#' silo_data <- silo71
#' empty_diff_path <- system.file("extdata/staggered", "empty_diff_df.csv",
#'                                package = "undidR")
#'
#' # Run `undid_stage_two()`
#' results <- undid_stage_two(
#'   empty_diff_filepath = empty_diff_path,
#'   silo_name = "71",
#'   silo_df = silo_data,
#'   time_column = "year",
#'   outcome_column = "coll",
#'   silo_date_format = "yyyy"
#' )
#'
#' # View results
#' head(results$diff_df)
#' head(results$trends_data)
#'
#' # Clean up temporary files
#' unlink(file.path(tempdir(), c("diff_df_71.csv",
#'                              "trends_data_71.csv")))
#' @export
undid_stage_two <- function(empty_diff_filepath, silo_name, silo_df,
                            time_column, outcome_column, silo_date_format,
                            consider_covariates = TRUE, filepath = tempdir()) {

  # Run filepaths and filename checks
  silo_name <- as.character(silo_name)
  trends_filename <- paste0("trends_data_", silo_name, ".csv")
  diff_filename <- paste0("filled_diff_df_", silo_name, ".csv")
  filepath <- .filename_filepath_check(c(trends_filename, diff_filename),
                                       filepath)

  diff_df <- .read_diff_df(empty_diff_filepath, silo_name = silo_name,
                           stage = 2)

  # Check and process the outcome and time columns
  .time_and_outcome_check(silo_df, time_column, outcome_column)
  colnames(silo_df)[colnames(silo_df) == time_column] <- "time"
  colnames(silo_df)[colnames(silo_df) == outcome_column] <- "outcome"
  if (!all(!is.na(suppressWarnings(as.numeric(silo_df$outcome))))) {
    stop("Ensure every value in the outcome column is a numeric value.")
  }
  if (!is.numeric(silo_df$outcome)) {
    stop(paste("Please ensure that the", outcome_column,
               "column has only numeric values."))
  }
  if (!is.character(silo_df$time)) {
    stop(paste("Please ensure that the", time_column,
               "column has only character values."))
  }
  silo_df$time <- mapply(.parse_string_to_date, silo_df$time,
                         silo_date_format)

  # If covariates are present, ensure the columns exist in silo_df
  if (identical(consider_covariates, FALSE)) {
    covariates <- "none"
  } else if (identical(consider_covariates, TRUE)) {
    covariates <- diff_df$covariates[1]
  } else {
    stop("`consider_covariates` must be entered as either `TRUE` or `FALSE`.")
  }
  if (covariates != "none") {
    covariates <- unlist(strsplit(covariates, ";"))
    columns_to_keep <- c(covariates, "time", "outcome")
    columns_to_check <- c("outcome", covariates)
    missing_covariates <- covariates[!covariates %in% colnames(silo_df)]
    if (length(missing_covariates) > 0) {
      stop(paste("Could not find:", paste(missing_covariates,
                                          collapse = ", "),
                 "in the local silo.\nEnsure covariates in the local silo are",
                 "spelled the same as they are in the `empty_diff_df.csv`."))
    }
  } else {
    columns_to_keep <- c("time", "outcome")
    columns_to_check <- c("outcome")
  }

  # Filter out unused columns
  silo_df <- silo_df[, colnames(silo_df) %in% columns_to_keep]

  # Check columns for missing values
  missing_values <- names(silo_df)[sapply(silo_df, anyNA)]
  if (length(missing_values > 0)) {
    stop(paste("Found columns with missing values:", paste(missing_values,
                                                           collapse = ", ")))
  }

  # Check outcome and covariate columns are numeric
  non_numeric_columns <- columns_to_check[!sapply(silo_df[columns_to_check],
                                                  is.numeric)]
  if (length(non_numeric_columns > 0)) {
    stop(paste("Ensure the following columns are numeric:",
               paste(non_numeric_columns, collapse = ", ")))
  }

  # Fill staggered adoption diff_df
  if ("diff_times" %in% names(diff_df)) {
    diff_df <- .fill_diff_df_staggered(silo_df, diff_df, covariates)
    if (as.character(as.integer(diff_df[diff_df$RI == 0, "treat"][1])) == "1") {
      treatment_time <- diff_df[diff_df$RI == 0, "gvar"][1]
    } else {
      treatment_time <- "control"
    }
    # Or fill common adoption diff_df
  } else if ("common_treatment_time" %in% names(diff_df)) {
    diff_df <- .fill_diff_df_common(silo_df, diff_df, covariates)

    # Also do the date matching procedure (as in .fill_diff_df_staggered)
    # for consistent trends_data
    end_date <- seq.Date(as.Date(diff_df$end_time[1],
                                 origin  = "1970-01-01"), length.out = 2,
                         by = diff_df$freq[1])[2]
    empty_diff_times <- seq.Date(as.Date(diff_df$start_time[1],
                                         origin = "1970-01-01"),
                                 as.Date(end_date, origin = "1970-01-01"),
                                 by = diff_df$freq[1])
    silo_diff_times <- unique(silo_df$time)
    missing_times <- c(setdiff(silo_diff_times, empty_diff_times),
                       setdiff(empty_diff_times, silo_diff_times))
    if (length(missing_times > 0)) {
      freq <- diff_df$freq[1]
      date_dict <- .match_dates(as.Date(empty_diff_times,
                                        origin = "1970-01-01"),
                                as.Date(silo_diff_times,
                                        origin = "1970-01-01"), freq)
      silo_df$time <- as.Date(sapply(silo_df$time,
                                     function(x) date_dict[[as.character(x)]]),
                              origin = "1970-01-01")
    }
    # Indicate treatment time for trends_data
    if (as.character(as.integer(diff_df$treat[1])) == "1") {
      treatment_time <- diff_df$common_treatment_time[1]
    } else {
      treatment_time <- "control"
    }
  }

  # Fill trends_data
  trends_data <- .fill_trends_data(silo_df, diff_df, covariates,
                                   silo_name, treatment_time)
  # Save as csv, print filepath, return dataframe
  full_path_diff <- file.path(filepath, diff_filename)
  full_path_trends <- file.path(filepath, trends_filename)
  write.csv(diff_df, full_path_diff, row.names = FALSE, quote = FALSE,
            fileEncoding = "UTF-8")
  message(diff_filename, " saved to: ", full_path_diff)
  write.csv(trends_data, full_path_trends, row.names = FALSE, quote = FALSE,
            fileEncoding = "UTF-8")
  message(trends_filename, " saved to: ", full_path_trends)

  return(list(diff_df = diff_df, trends_data = trends_data))
}

#' @keywords internal
# Checks that the time and outcome columns actually exist
.time_and_outcome_check <- function(silo_df, time_column, outcome_column) {
  if (!is.character(time_column)) {
    stop("Please enter the `time_column` as a character value.")
  }
  if (!is.character(outcome_column)) {
    stop("Please enter the `outcome_column` as a character value.")
  }
  if (!outcome_column %in% colnames(silo_df)) {
    stop(paste(outcome_column, "not found in local silo."))
  }
  if (!time_column %in% colnames(silo_df)) {
    stop(paste(time_column, "not found in local silo."))
  }
}

#' @keywords internal
# Matches silo dates to the most recently passed dates in empty_diff
.match_dates <- function(empty_diff_dates, silo_dates, freq) {

  backward_dates <- as.Date(character(), origin = "1970-01-01")
  forward_dates <- as.Date(character(), origin = "1970-01-01")

  min_diff_date <- min(empty_diff_dates)
  max_diff_date <- max(empty_diff_dates)

  date_to_add <- seq.Date(min_diff_date, length.out = 2,
                          by = paste0("-", freq))[2]
  min_date_req <- seq.Date(min(silo_dates), length.out = 2,
                           by = paste0("-", freq))[2]
  while (date_to_add >= min_date_req) {
    backward_dates <- c(backward_dates, date_to_add)
    date_to_add <- seq.Date(date_to_add, length.out = 2,
                            by = paste0("-", freq))[2]
  }


  date_to_add <- seq.Date(max_diff_date, length.out = 2,
                          by = freq)[2]
  max_date_req <- seq.Date(max(silo_dates), length.out = 2,
                           by = freq)[2]
  while (date_to_add <= max_date_req) {
    forward_dates <- c(forward_dates, date_to_add)
    date_to_add <- seq.Date(date_to_add, length.out = 2,
                            by = freq)[2]
  }

  all_diff_dates <- sort(c(backward_dates, empty_diff_dates, forward_dates))
  date_dict <- list()

  # Match each date in silo_dates with the closest date in all_diff_dates
  for (date in silo_dates) {
    if (date >= min(all_diff_dates)) {
      matched_date <- max(all_diff_dates[all_diff_dates <= date])
      date_dict[[as.character(date)]] <- matched_date
    }
  }
  return(date_dict)
}

#' @keywords internal
# Fill the empty_diff_df for staggered adoption during stage two
.fill_diff_df_staggered <- function(silo_df, diff_df, covariates) {

  # Do date matching procedure if necessary
  empty_diff_times <- unique(c(diff_df$diff_times_post, diff_df$diff_times_pre))
  silo_diff_times <- unique(silo_df$time)
  missing_times <- c(setdiff(silo_diff_times, empty_diff_times),
                     setdiff(empty_diff_times, silo_diff_times))
  if (length(missing_times > 0)) {
    freq <- diff_df$freq[1]
    date_dict <- .match_dates(as.Date(empty_diff_times, origin = "1970-01-01"),
                              as.Date(silo_diff_times,
                                      origin = "1970-01-01"), freq)
    silo_df$time <- as.Date(sapply(silo_df$time,
                                   function(x) date_dict[[as.character(x)]]),
                            origin = "1970-01-01")
  }

  # Fill the staggered diff_df
  pre_periods <- as.Date(unique(diff_df$diff_times_pre), origin = "1970-01-01")
  missing_dates <- c()
  for (pre in pre_periods) {
    post_periods <- unique(diff_df[diff_df$diff_times_pre == pre,
                                   "diff_times_post"])
    for (post in post_periods) {
      time_filter <- (silo_df$time == pre) | (silo_df$time == post)
      x <- silo_df[time_filter, "time"]
      x <- ifelse(x == pre, 0, 1)
      count_pre <- sum(x == 0)
      count_post <- sum(x == 1)

      # Keep track of dates with no obs
      if (count_pre == 0 || count_post == 0) {
        missing_dates <- .track_missing_dates(missing_dates, count_pre,
                                              count_post, pre, post)
      } else {
        x <- cbind(rep(1, length(x)), x)
        y <- as.numeric(silo_df[time_filter, ]$outcome)
        reg <- .regress(x, y)
        diff_df[(diff_df$diff_times_pre == pre) &
                  (diff_df$diff_times_post == post),
                "diff_estimate"] <- reg$beta_hat[2]
        diff_df[(diff_df$diff_times_pre == pre) &
                  (diff_df$diff_times_post == post),
                "diff_var"] <- reg$beta_hat_var[2]
        if (!identical(covariates, "none")) {
          x <- as.matrix(cbind(x, silo_df[time_filter, covariates]))
          reg <- .regress(x, y)
          diff_df[(diff_df$diff_times_pre == pre) &
                    (diff_df$diff_times_post == post),
                  "diff_estimate_covariates"] <- reg$beta_hat[2]
          diff_df[(diff_df$diff_times_pre == pre) &
                    (diff_df$diff_times_post == post),
                  "diff_var_covariates"] <- reg$beta_hat_var[2]
        }
      }

    }
  }
  .warn_missing_dates(missing_dates)

  diff_df$gvar <- as.Date(diff_df$gvar, origin = "1970-01-01")
  diff_df$gvar <- .parse_date_to_string(diff_df$gvar,
                                        diff_df$date_format[1])
  diff_df <- diff_df[, !(names(diff_df) %in%
                           c("diff_times_post", "diff_times_pre", "t"))]
  return(diff_df)
}

#' @keywords internal
#' Keeps track of missing dates in `fill_diff_df_staggered()`
.track_missing_dates <- function(missing_dates, count_pre,
                                 count_post, pre, post) {
  if (count_pre == 0) {
    missing_dates <- c(missing_dates, pre)
  }
  if (count_post == 0) {
    missing_dates <- c(missing_dates, post)
  }
  return(missing_dates)
}

#' @keywords internal
#' Send a warning if there are any missing dates when running
#'`fill_diff_df_staggered()`
.warn_missing_dates <- function(missing_dates) {
  if (length(missing_dates) > 0) {
    warning(paste("No obs found for:",
                  paste(sort(unique(as.Date(missing_dates,
                                            origin = "1970-01-01"))),
                        collapse = ", ")))
  }
}

#' @keywords internal
# Fill the empty_diff_df for common adoption during stage two
.fill_diff_df_common <- function(silo_df, diff_df, covariates) {

  treatment_time <- diff_df$common_treatment_time[1]
  start_time <- diff_df$start_time[1]
  end_time <- diff_df$end_time[1]
  silo_df <- silo_df[(silo_df$time >= start_time) &
                       (silo_df$time <= end_time), ]
  weights <- diff_df$weights[1]
  date_format <- diff_df$date_format[1]


  x <- silo_df$time
  x <- ifelse(x >= treatment_time, 1, 0)
  count_post <- sum(x == 1)
  count_pre <- sum(x == 0)
  x <- cbind(1, x)
  y <- as.numeric(silo_df$outcome)
  if (count_post == 0 || count_pre == 0) {
    stop(paste("Pre-treatment n. obs:", count_pre, "\nPost-treatment n. obs:",
               count_post,
               "\nNeed at least one obs on both sides of treatment."))
  }

  if (weights == "standard") {
    diff_df$weights[1] <- (count_post) / (count_pre + count_post)
  }

  reg <- .regress(x, y)
  diff_df$diff_estimate[1] <- reg$beta_hat[2]
  diff_df$diff_var[1] <- reg$beta_hat_var[2]
  if (!identical(covariates, "none")) {
    x <- as.matrix(cbind(x, silo_df[, covariates]))
    reg <- .regress(x, y)
    diff_df$diff_estimate_covariates[1] <- reg$beta_hat[2]
    diff_df$diff_var_covariates[1] <- reg$beta_hat_var[2]
  }

  diff_df$common_treatment_time <- .parse_date_to_string(
    as.Date(diff_df$common_treatment_time, origin = "1970-01-01"), date_format
  )
  diff_df$start_time <- as.Date(diff_df$start_time, origin = "1970-01-01")
  diff_df$end_time <- as.Date(diff_df$end_time, origin = "1970-01-01")
  return(diff_df)
}

#' @keywords internal
# Fill trends_data
.fill_trends_data <- function(silo_df, diff_df, covariates, silo_name,
                              treatment_time) {
  silo_times <- seq.Date(from = as.Date(diff_df$start_time[1],
                                        origin = "1970-01-01"),
                         to = as.Date(diff_df$end_time[1],
                                      origin = "1970-01-01"),
                         by = diff_df$freq[1])
  ntimes <- length(silo_times)
  headers <- c("silo_name", "treatment_time", "time", "mean_outcome",
               "mean_outcome_residualized", "covariates", "date_format", "freq")
  trends_data <- data.frame(matrix(ncol = length(headers), nrow = ntimes))
  colnames(trends_data) <- headers
  trends_data$silo_name <- rep(silo_name, ntimes)
  trends_data$treatment_time <- rep(treatment_time, ntimes)
  trends_data$time <- as.Date(silo_times, origin = "1970-01-01")
  trends_data$covariates <- rep(paste(covariates, collapse = ";"), ntimes)
  trends_data$date_format <- rep(diff_df$date_format[1], ntimes)
  trends_data$freq <- rep(diff_df$freq[1], ntimes)
  for (period in silo_times) {
    trends_data[trends_data$time == period, "mean_outcome"] <- mean(
      as.vector(
        silo_df[silo_df$time == period, ]$outcome
      )
    )
  }
  if (!identical(covariates, "none")) {
    for (period in silo_times) {
      x <- as.matrix(silo_df[silo_df$time == period, covariates])
      y <- as.vector(silo_df[silo_df$time == period, ]$outcome)
      beta_hat <- .inv(t(x) %*% x) %*% t(x) %*% y
      resid <- y - x %*% beta_hat
      trends_data[trends_data$time == period,
                  "mean_outcome_residualized"] <- mean(resid)
    }
  } else {
    trends_data$mean_outcome_residualized <- rep(NA_real_, ntimes)
  }



  trends_data$time <- .parse_date_to_string(trends_data$time,
                                            trends_data$date_format[1])

  return(trends_data)
}