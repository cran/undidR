#' Shows valid date formats
#'
#' The `undid_date_formats()` function returns a list of all valid date formats
#' that can be used within the `undidR` package.
#'
#' @details
#' The date formats returned by this function are used to ensure
#' consistency in date processing within the `undidR` package.
#'
#' @returns A named list containing valid date formats:
#' * `General_Formats`: General date formats compatible with the package.
#' * `R_Specific_Formats`: Date formats specific to R.
#' * `Other_Formats`: Formats seen sometimes in Stata.
#'
#' @examples
#' undid_date_formats()
#' @export
undid_date_formats <- function() {
  list(General_Formats = .undid_env$date_formats_general,
       R_Specific_Formats = .undid_env$date_formats_r,
       Other_Formats = .undid_env$other_formats)
}
