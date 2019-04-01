#' Perform a microsimulation of tax changes using the 2016-17 two per cent
#' sample of Australian taxpayers.
#'
#' @description
#'
#' The analysis year is 2019-20. The assumptions for employment growth and wages
#' are from the 2018-19 Budget and are as follows.
#'
#' Wages 2.25%, 2.75%, 3.25%, 3.5%
#' Employment 2.75%, 1.5%, 1.5%, 1.25%
#'
#' @param keep_df Whether to keep the amended tax file, mainly useful for
#'   debugging.
#' @param ... Parameters for the `calculate_tax` function
#' @param employment_growth A vector containing forecasts for employment growth
#' @param wages_growth A vector containing forecasts for wages growth
#' @param parallel Should this compute using the parallel package
#'
#' @return A microsim object
#' @export
#'
#' @examples
#' microsim(base_tax_brackets = c(18200, 37000, 80000, 1.8e5, Inf),
#'          change_tax_brackets = c(18200, 37000, 87000, 1.8e5, Inf))
#'
#' @import dplyr
#' @import ozTaxData
#' @import parallel
microsim <- function(keep_df = FALSE,
                     employment_growth = c(.0275, .015, .015, .0125),
                     wages_growth = c(.0225, .0275, .0325, .035),
                     parallel = FALSE,
                     ...) {
  tax_file <- uprate_data(ozTaxData::sample_16_17, wages_growth)
  if (parallel == TRUE) {
    n_cores <- detectCores() - 1
    cl <- makeCluster(n_cores)
    tax_file$difference <- parSapply(cl, tax_file$Taxable_Income,
                                   function(x) calculate_tax(x, ...)$difference)
  } else {
    tax_file$difference <- sapply(tax_file$Taxable_Income,
                                  function(x) calculate_tax(x, ...)$difference)
  }
  tax_file <- tax_file %>% mutate(decile = ntile(Taxable_Income, 10)) %>%
    select(Gender, decile, Partner_status, Tot_inc_amt, Taxable_Income,
           difference)
  distribution <- tax_file %>% group_by(decile) %>% summarise(
    revenue = sum(variable_compound(difference, employment_growth)) * 50 / 1e6,
    income_from = round(min(Taxable_Income), -2),
    income_to = round(max(Taxable_Income), -2),
    avg_change = mean(difference),
    avg_change_share = mean(difference) / mean(Taxable_Income)
  )
  gender <- tax_file %>% group_by(Gender) %>% summarise(
    revenue = sum(variable_compound(difference, employment_growth)) * 50 / 1e6,
    avg_change = mean(difference),
    avg_change_share = mean(difference) / mean(Taxable_Income)
  )
  revenue <- sum(variable_compound(tax_file$difference, employment_growth)) * 50 / 1e6

  n_tax_cut <- variable_compound(sum(tax_file$difference < 0), employment_growth) * 50
  n_tax_increase <- variable_compound(sum(tax_file$difference > 0), employment_growth) * 50
  n_no_difference <- variable_compound(sum(tax_file$difference == 0), employment_growth) * 50

  summary_tbl <- average_tax_table(...)

  input_params <- as.list(match.call()[-1])

  if (keep_df == FALSE) {
    tax_file <- NULL
  }
  output <- list(tax_file = tax_file,
                 revenue = revenue,
                 n_affected = list(n_tax_cut = n_tax_cut,
                                   n_tax_increase = n_tax_increase,
                                   n_no_difference = n_no_difference),
                 distribution = distribution,
                 gender = gender,
                 input_params = input_params,
                 summary_tbl = summary_tbl)
  class(output) <- "microsim"
  return(output)
}
