% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_tax.R
\name{calculate_tax}
\alias{calculate_tax}
\title{Calculate tax owed under two different scenarios}
\usage{
calculate_tax(income, base_income_tax_parameters = list(rates = c(0,
  0.19, 0.325, 0.37, 0.45), brackets = c(18200, 37000, 87000, 180000,
  Inf)), change_income_tax_parameters = list(rates = c(0, 0.19, 0.325,
  0.37, 0.45), brackets = c(18200, 37000, 87000, 180000, Inf)),
  base_ml_parameters = list(rate = 0.02, lower_bound_single = 21665),
  change_ml_parameters = list(rate = 0.02, lower_bound_single = 21665),
  base_lito_parameters = list(value = 445, taper_start = 37000,
  taper_rate = 0.015, second_taper_start = NA, second_taper_rate = NA),
  change_lito_parameters = list(value = 445, taper_start = 37000,
  taper_rate = 0.015, second_taper_start = NA, second_taper_rate = NA),
  base_lmito_parameters = list(initial_value = 200, increased_value =
  530, taper_in_rate = 0.03, taper_out_rate = 0.015),
  change_lmito_parameters = list(initial_value = 200, increased_value =
  530, taper_in_rate = 0.03, taper_out_rate = 0.015))
}
\arguments{
\item{income}{Individual income}

\item{base_income_tax_parameters}{Income tax parameters under the 'base' case}

\item{change_income_tax_parameters}{Income tax parameters under the 'change' case}

\item{base_ml_parameters}{Medicare levy parameters under the 'base' case}

\item{change_ml_parameters}{Medicare levy parameters under the 'change' case}

\item{base_lito_parameters}{LITO parameters under the 'base' case}

\item{change_lito_parameters}{LITO parameters under the 'change' case}

\item{base_lmito_parameters}{LMITO parameters under the 'base' case}

\item{change_lmito_parameters}{LMITO parameters under the 'base' case}
}
\value{
A list of the amount of tax owed under both scenarios and the
  difference.
}
\description{
Calculate tax owed under two different scenarios
}
\examples{
calculate_tax(1e5, change_income_tax_parameters = list(
brackets = c(18200, 37000, 90000, 180000, Inf)))

}
