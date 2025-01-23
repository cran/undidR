#' Example merit data
#'
#' A dataset containing college enrollment and demographic data for analyzing
#' the effects of merit programs in state 71.
#'
#' @format A tibble with 569 rows and 7 variables:
#' \describe{
#'   \item{coll}{Binary indicator for college enrollment (outcome variable)}
#'   \item{merit}{Binary indicator for merit program (treatment variable)}
#'   \item{male}{Binary indicator for male students}
#'   \item{black}{Binary indicator for Black students}
#'   \item{asian}{Binary indicator for Asian students}
#'   \item{year}{Year of observation}
#'   \item{state}{State identifier}
#' }
#' @source <https://economics.uwo.ca/people/conley_docs/code_to_download.html>
"silo71"