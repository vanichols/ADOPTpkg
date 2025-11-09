#' Example of questionaire input
#'
#' A dataset containing example rating/confidence/pesticide loads for two strategies
#'  The variables are as follows:
#'
#' @format A data frame with 12 rows and 5 variables:
#' \describe{
#'   \item{title}{A user-defined name for the approach, 2 total}
#'   \item{metric}{The performance metric being evaluated, 6 total}
#'   \item{rating_1to5}{The user-assigned rating on a scale from 1-5, integer}
#'   \item{confidence}{User-assigned confidence in rating, l, m, h, vh}
#'   \item{pesticide_load}{Pesticide load, calculated by user in identical units for both approaches}
#' }
"adopt_example"
