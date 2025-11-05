#' Binned beta distributions for confidence levels
#'
#' A dataset containing binned distributions for each rating/confidence combination, taken from Adrian Leach
#'  The variables are as follows:
#'
#' @format A data frame with 100 rows and 4 variables:
#' \describe{
#'   \item{rating}{One of five value ratings (very high, high, neutral, low, very low)}
#'   \item{confidence}{One of four confidence ratings (l = low, m = medium, h = high, vh = very high)}
#'   \item{score}{An arbitrary scale from 0-100 to represent the density function of 'value'}
#'   \item{value_bin}{A bin representing integer value ranging from 1-5, with 5 being very high value}
#' }
"opat_betas"
