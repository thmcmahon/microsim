#' Summary method for microsim objects
#'
#' @param x A microsim object
#'
#' @export
summary.microsim <- function(x) {
  stopifnot(class(x) == "microsim")
  cat('Parameters:')
  for (i in names(x$input_params)) {
    cat(i, ':', paste0(x$input_params[[i]]), '\n')
  }
  cat('\n')
  cat('Annual revenue difference', scales::dollar(round(x$revenue, -1)), 'm\n')
  cat('Number who will receive tax cut: ', scales::comma(round(x$n_affected[[1]], -3)), '\n')
  cat('Number who will receive tax increase: ', scales::comma(round(x$n_affected[[2]], -3)), '\n')
  cat('Number who will receive no change: ', scales::comma(round(x$n_affected[[3]], -3)), '\n')
}
