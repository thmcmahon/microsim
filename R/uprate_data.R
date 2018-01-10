#' Uprade data by wage price index
#'
#' @param tax_file
#'
#' @return An uprated tax sample
uprate_data <- function(tax_file) {
  wpi_13_14_to_19_20 <- c(.022, .02, .019, .0225, .0275, .0325)
  tax_file$Tot_inc_amt <- variable_compound(tax_file$Tot_inc_amt, wpi_13_14_to_19_20)
  tax_file$Taxable_Income <- variable_compound(tax_file$Taxable_Income, wpi_13_14_to_19_20)
  tax_file
}