#' Calculate tax owed under two differenct scenarios
#'
#' @param income Individual income
#' @param base_tax_brackets Tax brackets under the 'base' case
#' @param base_tax_rates Tax rates under the 'base' case
#' @param change_tax_brackets Tax brackets under the 'change' case
#' @param change_tax_rates Tax rates under the 'change' case
#' @param base_ml_taper_start Medicare levy taper start under the 'base' case
#' @param base_ml_rate Medicare levy rate under the 'base' case
#' @param change_ml_taper_start Medicare levy taper start under the 'change' case
#' @param change_ml_rate Medicare levy rate under the 'change' case
#' @param base_lito_amt Amount of LITO under the 'base' case
#' @param change_lito_amt Amount of LITO under the 'change' case
#' @param base_lito_taper_start LITO taper start under the 'base' case
#' @param change_lito_taper_start LITO taper start under the 'change' case
#' @param base_lito_taper_rate LITO taper rate under the 'base' case
#' @param change_lito_taper_rate LITO taper rate under the 'change' case
#' @param change_ml_progressive Whether to calculate a progressive medicare levy under the 'change' case
#' @param change_ml_cutoff Where should the higher rate cut in under the 'change' case
#' @param change_ml_prog_rate What shoudl the higher medicare levy rate be under the 'change' case
#'
#' @return A list of the amount of tax owed under both scenarios and the difference.
#' @export
#'
#' @examples
#' calculate_tax(90000, base_tax_brackets = c(18200, 37000, 80000, 1.8e5, Inf),
#'                      change_tax_brackets = c(18200, 37000, 87000, 1.8e5, Inf))
#'
#' @import taxFunctions
calculate_tax <- function(income,
                          base_tax_brackets = c(18200, 37000, 87000, 1.8e5, Inf),
                          base_tax_rates = c(0, .19, .325, .37, .45),
                          change_tax_brackets = c(18200, 37000, 87000, 1.8e5, Inf),
                          change_tax_rates = c(0, .19, .325, .37, .45),
                          base_ml_taper_start = 21335,
                          base_ml_rate = .02,
                          change_ml_taper_start = 21335,
                          change_ml_rate = .02,
                          base_lito_amt = 445,
                          change_lito_amt = 445,
                          base_lito_taper_start = 37000,
                          change_lito_taper_start = 37000,
                          base_lito_taper_rate = .015,
                          change_lito_taper_rate = .015,
                          change_ml_progressive = FALSE,
                          change_ml_cutoff = 87000,
                          change_ml_prog_rate = .025) {
  tax_base <- income_tax(income, rates = base_tax_rates,
                         brackets = base_tax_brackets)
  tax_change <- income_tax(income, rates = change_tax_rates,
                           brackets = change_tax_brackets)
  ml_base <- medicare_levy(income, rate = base_ml_rate,
                           lower_bound_single = base_ml_taper_start)
  if (change_ml_progressive == FALSE) {
    ml_change <- medicare_levy(income, rate = change_ml_rate,
                               lower_bound_single = change_ml_taper_start)
  } else if (change_ml_progressive == TRUE) {
    ml_change <- ml_progressive(income, cutoff = change_ml_cutoff,
                                base_rate = change_ml_rate,
                                increase_rate = change_ml_prog_rate)
  }

  lito_base <- lito(income, value = base_lito_amt,
                    taper_start = base_lito_taper_start,
                    taper_rate = base_lito_taper_rate)
  lito_change <- lito(income, value = change_lito_amt,
                      taper_start = change_lito_taper_start,
                      taper_rate = change_lito_taper_rate)

  base_tax_owed <- if(tax_base + ml_base + lito_base > 0) {
    tax_base + ml_base + lito_base
  } else {
    0
  }
  change_tax_owed <- if(tax_change + ml_change + lito_change > 0) {
    tax_change + ml_change + lito_change
  } else {
    0
  }
  return(list(base = base_tax_owed, change = change_tax_owed,
              difference = change_tax_owed - base_tax_owed))
}
