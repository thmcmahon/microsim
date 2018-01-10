#' Compound a principal by varying rates of interest.
#'
#' @param p Principal
#' @param rates Vector of interest rates for each period
#'
#' @return The compounded principal
#' @export
#'
#' @examples
#' variable_compound(100, c(.03, .03, .04))
variable_compound <- function(p, rates) {
  for (i in rates) {
    p <- p * (1 + i)
  }
  p
}
