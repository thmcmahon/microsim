#' Uprade income data by vector of rates
#'
#' @param tax_file
#'
#' @return An uprated tax sample
uprate_data <- function(tax_file, uprate_vector) {
  tax_file$Tot_inc_amt <- variable_compound(tax_file$Tot_inc_amt, uprate_vector)
  tax_file$Taxable_Income <- variable_compound(tax_file$Taxable_Income, uprate_vector)
  tax_file
}
