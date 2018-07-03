#'
#' Rounding problems evolution test:
#' plots for daily maximum (Tmax) and minimum temperature (Tmin) and
#' precipitation (RR) histograms showing rounding.
#'
#'
#' It plots the values after the decimal point to analyse rounding
#' problems evolution. It shows how frequently each of the 10 possible
#' values (.0 to .9) appears.
#'
#'
#' @details Input: A text file: each line is for one calendar day.
#'   The order of the columns should be: Year - Month - RR - Tmax - Tmin.
#'   Without header and NA values = -99.9.
#'
#'   Output: .pdf file with three histograms, one for RR > 0mm
#'   another for Tmax and the third for Tmin.
#'
#' @usage rounding_check()
#'
#' @import grDevices
#' @import graphics
#' @import stats
#' @import utils
#'
#' @export

rounding_check <- function() {
    file_name <- paste("rounding_evolution.pdf", sep = "")
    pdf(file = file_name)
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
    no_narr <- length(subset(daily_data$rr, !is.na(daily_data$rr)))
    par(mfrow = c(1, 3))
    rr_dif0 <- subset(daily_data$rr, daily_data$rr > 0)
    if (no_narr > 0) {
        hist(rr_dif0 %% 1, col = "blue", main = "RR > 0 rounding",
          breaks = c(seq(-0.1, 0.9, 0.1)), xlab = "")
    }
    if (no_natmax > 0) {
        hist(daily_data$tmax %% 1, col = "red", main = "Tmax rounding",
          breaks = c(seq(-0.1, 0.9, 0.1)), xlab = "")
    }
    if (no_natmin > 0) {
        hist(daily_data$tmin %% 1, col = "cyan", main = "Tmin rounding",
          breaks = c(seq(-0.1, 0.9, 0.1)), xlab = "")
    }
    dev.off()
    rm(list = ls())
    print("Rounding problems evolution test finished")
    print("Check < rounding_evolution.pdf >")
}
