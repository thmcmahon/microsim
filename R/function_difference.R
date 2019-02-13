#' Compare tax owed or benefit provided from tax/transfer calculators
#'
#' @param income Taxable income
#' @param FUN Function to calculate
#' @param base_arguments Arguments under the base scenario
#' @param change_arguments Arguments under the change scenario
#'
#' @return A list with three elements, the tax owed/benefit provided under the
#'   base and change scenarios and the difference between the two scenarios
#' @export
#'
#' @examples
#' function_difference(100000, income_tax,
#' base_arguments = list(brackets = c(18200, 37000, 87000, 180000, Inf)),
#' change_arguments = list(brackets = c(18200, 37000, 90000, 180000, Inf)))

function_difference <- function(income, FUN, base_arguments, change_arguments) {
  base_arguments <- c(income = income, base_arguments)
  change_arguments <- c(income = income, change_arguments)
  # Check to see if the base and change scenarios are the same, if so don't
  # calculate the tax function twice.
  if (identical(base_arguments, change_arguments)) {
    base <- do.call(FUN, base_arguments)
    change <- base
  } else {
    base <- do.call(FUN, base_arguments)
    change <- do.call(FUN, change_arguments)
  }
  return(list(base = base, change = change, difference = base - change))
}
