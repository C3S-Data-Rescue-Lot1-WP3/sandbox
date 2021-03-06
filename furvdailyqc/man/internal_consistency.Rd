% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/furv-dailyQC-internal-consistency.R
\name{internal_consistency}
\alias{internal_consistency}
\title{Internal consistency test:
determines the coherence between daily maximum temperature (Tmax) values and daily minimum temperature (Tmin) values.}
\usage{
internal_consistency()
}
\description{
Given daily maximum (Tmax) and minimum (Tmin) temperatures values, compute the differences between the two variables,
the output is a list of days with Tmin >= Tmax.
}
\details{
Input: A text file: each line is for one calendar day.
  The order of the columns should be: Year - Month - RR - Tmax - Tmin.
  Without header and NA values = -99.9.

  Output: writes a list with suspicious values (Tmin >= Tmax) <tmaxmin.txt>;
  each line represents one day and the order of columns is: Year - Month -
  RR - Tmax - Tmin.
}
