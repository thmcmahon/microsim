#' Pretty table for distributional analysis
#'
#' @param x A microsim object
#'
#' @export
#'
#' @import dplyr
#' @import knitr
distro_kable <- function(x) {
  stopifnot(class(x) == "microsim")
  x$distribution %>%
    mutate(decile = ifelse(row_number(decile) < 10,
           paste0(decile, ' (', scales::comma(income_from), '-',
                  scales::comma(income_to), ')'),
           paste0(decile, ' (', scales::comma(income_from), '+)'))
      ) %>%
    select(decile, revenue, avg_change, avg_change_share) %>%
    mutate(avg_change_share = scales::percent(round(avg_change_share, 4))) %>%
    kable(digits = -1,
          format.args = list(big.mark=','),
          col.names = c('Decile (Income Range)', 'Revenue ($m)',
                        'Average Change ($)', 'Average Change (%)'),
          align = c('l', 'r', 'r', 'r'))
}
