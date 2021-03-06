#'
#' Internal consistency test:
#' determines the coherence between daily maximum temperature (Tmax) values and daily minimum temperature (Tmin) values.
#'
#' Given daily maximum (Tmax) and minimum (Tmin) temperatures values, compute the differences between the two variables,
#' the output is a list of days with Tmin >= Tmax.
#'
#' @details Input: A text file: each line is for one calendar day.
#'   The order of the columns should be: Year - Month - RR - Tmax - Tmin.
#'   Without header and NA values = -99.9.
#'
#'   Output: writes a list with suspicious values (Tmin >= Tmax) <tmaxmin.txt>;
#'   each line represents one day and the order of columns is: Year - Month -
#'   RR - Tmax - Tmin.
#'
#' @usage internal_consistency()
#'
#' @import stats
#' @import utils
#'
#' @export

internal_consistency <- function() {
  if (exists('daily_data')) {
    daily_data <- daily_data
  } else {
    daily_data_file <- file.choose()
    daily_data <- read.table(daily_data_file, 
      col.names = c("year", "month", "day", "rr", "tmax", "tmin"),
      na.strings = "-99.9")
    assign("daily_data", daily_data, envir=globalenv())
  }
    no_natmax <- length(subset(daily_data$tmax, !is.na(daily_data$tmax)))
    no_natmin <- length(subset(daily_data$tmin, !is.na(daily_data$tmin)))
    if (no_natmax > 0 & no_natmin > 0) {
        filena <- paste("tmaxmin.txt", sep = "")
        write.table(subset(daily_data, (daily_data$tmax - daily_data$tmin) <= 0),
          file = filena, quote = FALSE, row.names = FALSE, col.names = FALSE)
        rm(list = ls())
        print("Internal consistency test finished")
        print("Check < tmaxmin.txt >")
    }
}
