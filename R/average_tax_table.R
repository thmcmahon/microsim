#' Create an average tax table comparing two scenarios using the `calculate_tax` function
#'
#' @param inc_start Income range start
#' @param inc_stop Income range stop
#' @param step Increment amount
#' @param ... Paramaters for the `calculate_tax` function
#'
#' @return A data frame showing average tax rates by income level for two scenarios
#' @export
#'
#' @examples
#' average_tax_table(base_tax_brackets = c(18200, 37000, 80000, 1.8e5, Inf),
#'                   change_tax_brackets = c(18200, 37000, 87000, 1.8e5, Inf))
#'
#' @import dplyr
average_tax_table <- function(inc_start = 1000, inc_stop = 3e5, step = 1000,
                              ...) {
  incomes <- seq(inc_start, inc_stop, by = step)
  df <- sapply(incomes, function(x) calculate_tax(x, ...)) %>%
    t %>%
    unlist %>%
    matrix(ncol = 3) %>%
    as.data.frame %>%
    cbind(incomes, .)
  names(df) <- c('income', 'base', 'change', 'difference')
  df %>% mutate(base_avg = base / income, change_avg = change / income,
                difference_avg = difference / income)
}
