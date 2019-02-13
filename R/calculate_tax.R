#' Calculate tax owed under two different scenarios
#'
#' @param income Individual income
#' @param base_income_tax_parameters Income tax parameters under the 'base' case
#' @param change_income_tax_parameters Income tax parameters under the 'change' case
#' @param base_ml_parameters Medicare levy parameters under the 'base' case
#' @param change_ml_parameters Medicare levy parameters under the 'change' case
#' @param base_lito_parameters LITO parameters under the 'base' case
#' @param change_lito_parameters LITO parameters under the 'change' case
#' @param base_lmito_parameters LMITO parameters under the 'base' case
#' @param change_lmito_parameters LMITO parameters under the 'base' case
#'
#' @return A list of the amount of tax owed under both scenarios and the
#'   difference.
#' @export
#'
#' @examples
#' calculate_tax(1e5, change_income_tax_parameters = list(
#' brackets = c(18200, 37000, 90000, 180000, Inf)))
#'
#' @import taxFunctions
calculate_tax <- function(income,
                          base_income_tax_parameters = list(
                            rates = c(0, .19, .325, .37, .45),
                            brackets = c(18200, 37000, 87000, 180000, Inf)),
                          change_income_tax_parameters = list(
                            rates = c(0, .19, .325, .37, .45),
                            brackets = c(18200, 37000, 87000, 180000, Inf)),
                          base_ml_parameters = list(
                            rate = .02,
                            lower_bound_single = 21665),
                          change_ml_parameters = list(
                            rate = .02,
                            lower_bound_single = 21665),
                          base_lito_parameters = list(
                            value = 445,
                            taper_start = 37000,
                            taper_rate = .015,
                            second_taper_start = NA,
                            second_taper_rate = NA),
                          change_lito_parameters = list(
                            value = 445,
                            taper_start = 37000,
                            taper_rate = .015,
                            second_taper_start = NA,
                            second_taper_rate = NA),
                          base_lmito_parameters = list(
                            initial_value = 200,
                            increased_value = 530,
                            taper_in_rate = .03,
                            taper_out_rate = .015),
                          change_lmito_parameters = list(
                            initial_value = 200,
                            increased_value = 530,
                            taper_in_rate = .03,
                            taper_out_rate = .015)) {

  pit <- function_difference(income,
                             income_tax,
                             base_arguments = base_income_tax_parameters,
                             change_arguments = change_income_tax_parameters)

  ml <- function_difference(income,
                            medicare_levy,
                            base_arguments = base_ml_parameters,
                            change_arguments = change_ml_parameters)


  lito_benefit <- function_difference(income,
                                      lito,
                                      base_arguments = base_lito_parameters,
                                      change_arguments = change_lito_parameters)

  lmito_benefit <- function_difference(
    income,
    lmito,
    base_arguments = base_lmito_parameters,
    change_arguments = change_lmito_parameters)

  base_tax_owed <- if(pit$base + ml$base + lito_benefit$base + lmito_benefit$base > 0) {
    pit$base + ml$base + lito_benefit$base + lmito_benefit$base
  } else {
    0
  }
  change_tax_owed <- if(pit$change + ml$change + lito_benefit$change + lmito_benefit$change > 0) {
    pit$change + ml$change + lito_benefit$change + lmito_benefit$change
  } else {
    0
  }
  return(list(base = base_tax_owed, change = change_tax_owed,
              difference = change_tax_owed - base_tax_owed))
}
