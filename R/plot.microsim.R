#' A plot method for microsim objects
#'
#' @param x Microsimulation to plot
#' @param type Whether to show a plot of average tax rates or a distributional plot
#'
#' @export
#'
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @import themeTom
plot.microsim <- function(x, type = "average") {
  if (type == "average") {
    x$summary_tbl %>%
      select(income, base_avg, change_avg) %>%
      gather(type, avg_rate, -income) %>%
      ggplot(aes(income, avg_rate, colour = type)) +
      geom_line() +
      theme_tom() +
      scale_x_continuous(labels = scales::dollar) +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "Income", y = "Average Tax Rate",
           title = "Average Tax Rates", subtitle = "Base vs Change") +
      theme(legend.position = c(0.9, 0.2), legend.background = element_blank())
  } else if (type == "distribution") {
    x$distribution %>%
      select(decile, revenue, avg_change) %>%
      gather(category, value, -decile) %>%
      mutate(category = recode(category, avg_change = "Average change ($)",
                               revenue = "Revenue ($m)")) %>%
      ggplot(aes(as.factor(decile), value)) + geom_col() +
      facet_wrap(~ category) +
      scale_y_continuous(labels = scales::dollar) +
      labs(x = "Taxable Income Decile", y = "",
           title = "Distribution of impact of changes",
           subtitle = "Positive numbers mean increased tax, negative numbers mean a tax cut") +
      theme_tom()
  } else {
    stop("Type must either by `average` or `distribution`")
  }
}
