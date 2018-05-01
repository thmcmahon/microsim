#' Print method for microsim objects
#'
#' @param x A microsim object
#'
#' @export
#'
#' @import dplyr
print.microsim <- function(x) {
  stopifnot(class(x) == "microsim")
  cat('Parameters:\n')
  for (i in names(x$input_params)) {
    cat(i, ':', paste0(x$input_params[[i]]), '\n')
  }
  cat('\n')
  cat("Total revenue:", scales::dollar(x$revenue), "($m)\n")
  cat("Number who will receive tax cut:", scales::comma(x$n_affected[[1]]), "\n")
  cat("Number who will receive tax increase:", scales::comma(x$n_affected[[2]]), "\n")
  cat("Number who will receive no change:", scales::comma(x$n_affected[[3]]), "\n\n")
  cat("Distributional analysis:", "\n")
  x$distribution %>%
    mutate(income_from = scales::dollar(income_from),
           income_to = scales::dollar(income_to),
           revenue = scales::dollar(revenue),
           avg_change = scales::dollar(avg_change)) %>%
    select(decile, income_from, income_to, `revenue ($m)` = revenue, avg_change) %>%
    as.data.frame %>%
    print
  cat("\n")
  cat("Gender analysis:", "\n")
  x$gender %>%
    mutate(revenue = scales::dollar(revenue),
           avg_change = scales::dollar(avg_change)) %>%
    select(`revenue ($m)` = revenue, everything()) %>%
    as.data.frame %>%
    print
}
