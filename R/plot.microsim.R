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
plot.microsim <- function(x, type = "average", base_title = "base", change_title = "change") {
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
      scale_colour_hue(labels = c(base_title, change_title)) +
      theme(legend.position = c(0.9, 0.2), legend.background = element_blank())
  } else if (type == "distribution") {
    x$distribution %>%
      select(decile, avg_change) %>%
      ggplot(aes(as.factor(decile), avg_change)) +
      geom_col() +
      scale_y_continuous(labels = scales::dollar) +
      labs(x = "Taxable Income Decile", y = "",
           title = "Distribution of impact of changes in dollars",
           subtitle = "Positive numbers mean increased tax, negative numbers mean a tax cut") +
      theme_tom()
  } else if (type == "distribution_share") {
    x$distribution %>%
      select(decile, avg_change_share) %>%
      ggplot(aes(as.factor(decile), avg_change_share)) +
      geom_col() +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "Taxable Income Decile", y = "",
           title = "Distribution of impact of changes as percentage of income",
           subtitle = "Positive numbers mean increased tax, negative numbers mean a tax cut") +
      theme_tom()
  } else if (type == "dollar_change") {
    x$summary_tbl %>%
      filter(income < 200000) %>%
      ggplot(aes(x = income, y = difference)) +
      geom_line() +
      scale_y_continuous("", labels = scales::dollar) +
      scale_x_continuous("", labels = scales::dollar) +
      labs(x = "Income", y = "",
           title = "Distribution of impact of changes in dollars",
           subtitle = "Positive numbers mean increased tax, negative numbers mean a tax cut") +
      theme_tom()
  } else {
    stop("Type must either by `average`, `distribution` or `distribution_share`")
  }
}
