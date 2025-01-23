#' Populate the internal `.undid_env` environment
#'
#' This function populates the `.undid_env` environment with pre-defined
#' variables for use within the package. It is called during `.onLoad()`.
#'
#' @param env An environment to populate with package variables.
#' @keywords internal
#' @noRd
.populate_undid_env <- function(env) {

  # Define the easily parsed date formats
  env$date_formats_general <- c("yyyy/mm/dd", "yyyy-mm-dd", "yyyymmdd",
                                "yyyy/dd/mm", "yyyy-dd-mm", "yyyyddmm",
                                "dd/mm/yyyy", "dd-mm-yyyy", "ddmmyyyy",
                                "mm/dd/yyyy", "mm-dd-yyyy", "mmddyyyy",
                                "yyyy")

  # Define the R date formats
  env$date_formats_r <- c("%Y/%m/%d", "%Y-%m-%d", "%Y%m%d", "%Y/%d/%m",
                          "%Y-%d-%m", "%Y%d%m", "%d/%m/%Y", "%d-%m-%Y",
                          "%d%m%Y", "%m/%d/%Y", "%m-%d-%Y", "%m%d%Y",
                          "%Y")

  # Other compatiable date formats
  env$other_formats <- c("ddmonyyyy", "yyyym00")

  # Define a dictionary to convert general date formats into R date formats
  env$date_format_dict_to_r <- c("yyyy/mm/dd" = "%Y/%m/%d",
                                 "yyyy-mm-dd" = "%Y-%m-%d",
                                 "yyyymmdd" = "%Y%m%d",
                                 "yyyy/dd/mm" = "%Y/%d/%m",
                                 "yyyy-dd-mm" = "%Y-%d-%m",
                                 "yyyyddmm" = "%Y%d%m",
                                 "dd/mm/yyyy" = "%d/%m/%Y",
                                 "dd-mm-yyyy" = "%d-%m-%Y",
                                 "ddmmyyyy" = "%d%m%Y",
                                 "mm/dd/yyyy" = "%m/%d/%Y",
                                 "mm-dd-yyyy" = "%m-%d-%Y",
                                 "mmddyyyy" = "%m%d%Y",
                                 "yyyy" = "%Y")

  # Define a dictionary to convert R date formats to general date formats
  env$date_format_dict_from_r <- c("%Y/%m/%d" = "yyyy/mm/dd",
                                   "%Y-%m-%d" = "yyyy-mm-dd",
                                   "%Y%m%d"   = "yyyymmdd",
                                   "%Y/%d/%m" = "yyyy/dd/mm",
                                   "%Y-%d-%m" = "yyyy-dd-mm",
                                   "%Y%d%m"   = "yyyyddmm",
                                   "%d/%m/%Y" = "dd/mm/yyyy",
                                   "%d-%m-%Y" = "dd-mm-yyyy",
                                   "%d%m%Y"   = "ddmmyyyy",
                                   "%m/%d/%Y" = "mm/dd/yyyy",
                                   "%m-%d-%Y" = "mm-dd-yyyy",
                                   "%m%d%Y"   = "mmddyyyy",
                                   "%Y" = "yyyy")

  # Define a dictionary to convert month abbrevations to numeric strings
  env$month_dict <- c("jan" = "01", "feb" = "02", "mar" = "03",
                      "apr" = "04", "may" = "05", "jun" = "06",
                      "jul" = "07", "aug" = "08", "sep" = "09",
                      "oct" = "10", "nov" = "11", "dec" = "12")

  # And define a reverse dictionary for integers to months
  env$month_dict_reverse <- c(`1` = "jan", `2` = "feb", `3` = "mar",
                              `4` = "apr", `5` = "may", `6` = "jun",
                              `7` = "jul", `8` = "aug", `9` = "sep",
                              `10` = "oct", `11` = "nov", `12` = "dec")

  # Define a mapping for frequencies
  env$freq_map <- c("yearly" = "year", "year" = "year",
                    "annual" = "year",
                    "annually" = "year", "monthly" = "month",
                    "month" = "month", "weekly" = "week",
                    "week" = "week",
                    "daily" = "day", "day" = "day")

  # Define expected columns for staggered adoption diff_df
  env$staggered_columns <- c("silo_name", "gvar", "treat", "diff_times",
                             "gt", "diff_estimate", "diff_var",
                             "diff_estimate_covariates",
                             "diff_var_covariates", "covariates",
                             "date_format", "freq", "RI", "start_time",
                             "end_time")

  # Define expected columns for common adoption diff_df
  env$common_columns <- c("silo_name", "treat", "common_treatment_time",
                          "start_time", "end_time", "weights",
                          "diff_estimate", "diff_var",
                          "diff_estimate_covariates",
                          "diff_var_covariates",
                          "covariates", "date_format", "freq")

  # Define interpolation options
  env$interpolation_options <- c("linear_function", "nearest_value",
                                 "piecewise_linear")

}