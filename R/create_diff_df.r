#' Creates the `empty_diff_df.csv`
#'
#' Creates the `empty_diff_df.csv` which lists all of the differences that
#' need to calculated at each silo in order to compute the aggregate ATT.
#' The `empty_diff_df.csv` is then to be sent out to each silo to be filled out.
#'
#' @details Ensure that dates in the `init.csv` are entered consistently
#' in the same date format. Call [undid_date_formats()] to see a list of valid
#' date formats. Covariates specified when calling `create_diff_df()` will
#' override any covariates specified in the `init.csv`.
#'
#' @param init_filepath A character filepath to the `init.csv`.
#' @param date_format A character specifying the date format used in the
#'  `init.csv`. Call [undid_date_formats()] to see a list of valid date formats.
#' @param freq A character indicating the length of the time periods to be used
#'  when computing the differences in mean outcomes between periods at each
#'  silo. Options are: `"yearly"`, `"monthly"`, `"weekly"`, or `"daily"`.
#' @param covariates A character vector specifying covariates to be considered
#'  at each silo. If `FALSE` (default) uses covariates from the `init.csv`.
#' @param freq_multiplier A numeric value or `FALSE` (default).
#'  Specify if the frequency should be multiplied by a non-zero integer.
#' @param weights A character indicating the weighting to use in the case of
#'  common adoption. The `"standard"` (default) weight is calculated as
#'  \eqn{w_s = \frac{N_s^{\text{post}}}{N_s^{\text{post}} + N_s^{\text{pre}}}}.
#'  Options are: `"standard"`.
#' @param filename A character filename for the created CSV file. Defaults to
#'  `"empty_diff_df.csv"`
#' @param filepath Filepath to save the CSV file. Defaults to `tempdir()`.
#'
#' @returns A data frame detailing the silo and time combinations for which
#'  differences must be calculated in order to compute the aggregate ATT. A
#'  CSV copy is saved to the specified directory which is then to be sent out
#'  to each silo.
#'
#' @examples
#' file_path <- system.file("extdata/staggered", "init.csv",
#'                          package = "undidR")
#' create_diff_df(
#'   init_filepath = file_path,
#'   date_format = "yyyy",
#'   freq = "yearly"
#' )
#' unlink(file.path(tempdir(), "empty_diff_df.csv"))
#' @importFrom utils read.csv write.csv
#' @export
create_diff_df <- function(init_filepath, date_format, freq, covariates = FALSE,
                           freq_multiplier = FALSE, weights = "standard",
                           filename = "empty_diff_df.csv",
                           filepath = tempdir()) {

  # Run filepaths and filename checks
  filepath <- .filename_filepath_check(filename, filepath)


  # Force freq to lowercase
  freq <- tolower(freq)

  # Read in the init.csv, ensure everything is a string
  init_df <- read.csv(init_filepath, header = TRUE, sep = ",",
                      stringsAsFactors = FALSE)
  init_df <- data.frame(lapply(init_df, as.character), stringsAsFactors = FALSE)
  init_df$treatment_time <- tolower(init_df$treatment_time)

  # Run init logic checks
  .init_checks(init_df)

  # Convert start_time and end_time columns to date objects
  init_df$start_time <- as.Date(vapply(init_df$start_time,
                                       .parse_string_to_date,
                                       FUN.VALUE = as.Date(
                                         NA, origin = "1970-01-01"
                                       ),
                                       date_format = date_format),
  origin = "1970-01-01")
  init_df$end_time <- as.Date(vapply(init_df$end_time, .parse_string_to_date,
                                     FUN.VALUE = as.Date(
                                       NA, origin = "1970-01-01"
                                     ),
                                     date_format = date_format),
  origin = "1970-01-01")

  # Ensure that start times < treat times < end times
  .start_treat_end_time_check(init_df, date_format)


  # Process freq_multiplier and freq
  freq_string <- .parse_freq_freq_multiplier(freq, freq_multiplier)

  # Consider the case of common adoption
  if (length(unique(init_df$treatment_time)) == 2) {
    diff_df <- .create_common_diff_df(init_df, weights)

    # Consider the case of staggered adoption
  } else if (length(unique(init_df$treatment_time)) > 2) {
    diff_df <- .create_staggered_diff_df(init_df, date_format, freq_string)
  } else {
    stop("Only one unique `treatment_time` value found.")
  }

  # Add the diff_estimate columns
  diff_df$diff_estimate <- rep(NA_real_, nrow(diff_df))
  diff_df$diff_var <- rep(NA_real_, nrow(diff_df))
  diff_df$diff_estimate_covariates <- rep(NA_real_, nrow(diff_df))
  diff_df$diff_var_covariates <- rep(NA_real_, nrow(diff_df))

  # Add the covariates if they exist
  if (identical(covariates, FALSE)) {
    if ("covariates" %in% colnames(init_df)) {
      covariates <- init_df[, "covariates"][1]
    } else {
      covariates <- "none"
    }
  } else {
    covariates <- paste(covariates, collapse = ";")
  }
  diff_df$covariates <- rep(covariates, nrow(diff_df))

  # Note date_format and freq info
  if (date_format %in% .undid_env$date_formats_r) {
    date_format <- .undid_env$date_format_dict_from_r[date_format]
  }
  diff_df$date_format <- rep(date_format, nrow(diff_df))
  diff_df$freq <- rep(freq_string, nrow(diff_df))

  full_path <- file.path(filepath, filename)
  # Save as csv, print filepath, return dataframe
  write.csv(diff_df, full_path, row.names = FALSE, quote = FALSE,
            fileEncoding = "UTF-8")
  message(filename, " saved to: ", full_path)
  rownames(diff_df) <- NULL
  return(diff_df)

}

#' @keywords internal
# Process freq and freq_multiplier for create_diff_df
.parse_freq_freq_multiplier <- function(freq, freq_multiplier) {
  # Process freq_multiplier and freq
  if (!freq %in% names(.undid_env$freq_map)) {
    stop("Choose: `\"yearly\"`, `\"monthly\"`, `\"weekly\"`, or `\"daily\"`.")
  } else {
    freq <- .undid_env$freq_map[[freq]]
  }
  if (freq_multiplier == FALSE) {
    freq_multiplier <- "1"
  } else if (is.numeric(freq_multiplier)) {
    freq_multiplier <- as.character(as.integer(freq_multiplier))
    if (freq_multiplier == "0") {
      stop("Ensure `freq_multiplier` is set to `FALSE` or a non-zero integer.")
    }
    freq <- paste0(freq, "s")
  } else {
    stop("Ensure `freq_multiplier` is set to `FALSE` or a non-zero integer.")
  }
  freq_string <- paste0(freq_multiplier, " ", freq)
  return(freq_string)
}

#' @keywords internal
# Create empty_diff_df.csv for common treatment time
.create_common_diff_df <- function(init_df, weights) {
  silo_name <- c()
  treat <- c()
  common_treatment_time <- rep(init_df[init_df$treatment_time != "control",
                                       "treatment_time"][1], nrow(init_df))
  start_time <- c()
  end_time <- c()
  for (silo in unique(init_df$silo_name)) {
    if (init_df[init_df$silo_name == silo, "treatment_time"] != "control") {
      treat <- c(treat, 1)
    } else if (init_df[init_df$silo_name == silo,
                       "treatment_time"] == "control") {
      treat <- c(treat, 0)
    }
    start_time <- c(start_time, init_df[init_df$silo_name == silo,
                                        "start_time"])
    end_time <- c(end_time, init_df[init_df$silo_name == silo,
                                    "end_time"])
    silo_name <- c(silo_name, silo)
  }
  if (weights == "standard") {
    weights <- rep("standard", nrow(init_df))
  }
  diff_df <- data.frame(silo_name = silo_name, treat = treat,
                        common_treatment_time = common_treatment_time,
                        start_time = start_time, end_time = end_time,
                        weights = weights)
  diff_df$start_time <- as.Date(diff_df$start_time, origin = "1970-01-01")
  diff_df$end_time <- as.Date(diff_df$end_time, origin = "1970-01-01")
  return(diff_df)
}

#' @keywords internal
# Create empty_diff_df.csv for staggered adoption
.create_staggered_diff_df <- function(init_df, date_format, freq_string) {

  # First grab all unique treatment times
  init_df$treatment_time_date <- vapply(init_df$treatment_time, function(x) {
    if (x != "control") {
      return(.parse_string_to_date(x, date_format = date_format))
    } else if (x == "control") {
      return(NA)
    }
  }, FUN.VALUE = as.Date(NA, origin = "1970-01-01"))
  all_treatment_times <- sort(as.Date(na.omit(init_df$treatment_time_date),
                                      origin = "1970-01-01"))

  # Grab start and end times
  start <- init_df$start_time[1]
  end <- init_df$end_time[1]
  gt_control <- do.call(rbind,
                        lapply(all_treatment_times, function(treatment_time) {
                          times <- seq.Date(
                            from = as.Date(
                              treatment_time,
                              origin = "1970-01-01"
                            ),
                            to = end, by = freq_string
                          )
                          data.frame(g = treatment_time, t = times)
                        }))
  gt_control <- unique(gt_control)

  # Create an empty list to store diff_df subsets for each silo
  diff_df_list <- list()
  # Loop through each silo and create appropriate rows
  for (silo in unique(init_df$silo_name)) {
    treatment_time <- init_df[init_df$silo_name == silo, "treatment_time"]

    if (treatment_time != "control") {
      treatment_time <- .parse_string_to_date(treatment_time, date_format)
      gt <- data.frame(g = treatment_time,
                       t = seq.Date(from = treatment_time,
                                    to = end, by = freq_string))
      diff_times <- expand.grid(post = gt$t,
                                pre = seq(treatment_time, length = 2,
                                          by = paste0("-", freq_string))[2])
      treat <- "1"
    } else if (treatment_time == "control") {
      gt <- gt_control
      diff_times <- data.frame(post = NULL,
                               pre = NULL)
      for (g in unique(gt$g)) {
        g <- as.Date(g, origin = "1970-01-01")
        post_periods <- seq(from = g, to = end, by = freq_string)
        pre_period <- seq(g, length = 2,
                          by = paste0("-", freq_string))[2]
        diff_times <- rbind(diff_times,
                            expand.grid(post = post_periods,
                                        pre = pre_period))
      }
      treat <- "0"
    }
    # Define number of rows for the subset, parse dates to strings
    diff_times_nrows <- nrow(diff_times)
    gvar_vector <- gt$g
    t_vector <- .parse_date_to_string(gt$t, date_format)
    post_vector <- .parse_date_to_string(diff_times$post, date_format)
    pre_vector <- .parse_date_to_string(diff_times$pre, date_format)

    # Create the diff_df subset for that silo
    temp_df <- data.frame(silo_name = rep(silo, diff_times_nrows),
                          gvar = gvar_vector,
                          treat = rep(treat, diff_times_nrows),
                          diff_times = paste(post_vector,
                                             pre_vector, sep = ";"),
                          gt = paste(.parse_date_to_string(gvar_vector,
                                                           date_format),
                                     t_vector, sep = ";"),
                          stringsAsFactors = FALSE)
    # Store the diff_df subset for each silo in list
    diff_df_list[[length(diff_df_list) + 1]] <- temp_df
  }
  # Combine all the diff_df subsets of each silo into a final diff_df
  diff_df <- do.call(rbind, diff_df_list)

  # Now add rows for RI
  diff_df$RI <- rep(0, nrow(diff_df))
  diff_df <- diff_df[order(diff_df$gvar), ]
  ri_df_list <- list()
  control_silo <- diff_df[diff_df$treat == "0", "silo_name"][1]
  for (silo in unique(diff_df[diff_df$treat == "1", "silo_name"])) {
    gvar <- unique(diff_df[diff_df$silo_name == silo, "gvar"])
    gvars <- diff_df[
      (diff_df$gvar != gvar) & (diff_df$silo_name == control_silo), "gvar"
    ]
    diff_times <- diff_df[
      (diff_df$gvar != gvar) & (diff_df$silo_name == control_silo), "diff_times"
    ]
    gt <- diff_df[
      (diff_df$gvar != gvar) & (diff_df$silo_name == control_silo), "gt"
    ]
    n <- length(gt)
    temp_df <- data.frame(silo_name = rep(silo, n), gvar = gvars,
                          treat = rep(-1, n), diff_times = diff_times, gt = gt,
                          RI = rep(1, n))
    ri_df_list[[length(ri_df_list) + 1]] <- temp_df
  }
  ri_df <- do.call(rbind, ri_df_list)
  diff_df <- rbind(diff_df, ri_df)
  diff_df$gvar <- .parse_date_to_string(diff_df$gvar, date_format)
  diff_df$start_time <- .parse_date_to_string(start, date_format)
  diff_df$end_time <- .parse_date_to_string(end, date_format)
  return(diff_df)
}
