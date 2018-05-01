#' Pretty table for gender analysis
#'
#' @param x A microsim object
#'
#' @export
#'
#' @import dplyr
#' @import knitr
gender_kable <- function(x) {
  stopifnot(class(x) == "microsim")
  x$gender %>%
    mutate(avg_change_share = scales::percent(round(avg_change_share, 4))) %>%
    kable(digits = -1, format.args = list(big.mark=','),
          col.names = c('Gender', 'Revenue ($m)',
                        'Average Change ($)', 'Average Change (%)'),
          align = c('l', 'r', 'r', 'r'))
}
