#'
#' Tolerance test:
#'
#' report on occurrences of 4 or more equal consecutive values in daily
#' maximum (Tmax) and minimum temperatures (Tmin).
#'
#'
#' @details Input: A text file: each line is for one calendar day.
#'   The order of the columns should be: Year - Month - RR - Tmax - Tmin.
#'   Without header and NA values = -99.9.
#'
#'   Output: writes two list (one for Tmax and another for Tmin) with a line
#'   for each sequence of 4 or more consecutive values. Each line represents
#'   one day and the order of columns is: Year - Month - Tmax (or Tmin) -
#'   length of sequence.
#'
#' @usage tolerance_test()
#'
#' @import grDevices
#' @import graphics
#' @import stats
#' @import utils
#'
#' @export

tolerance_test <- function() {
    file <- file.choose()
    data <- read.table(file,
      col.names = c("year", "month", "day", "rr", "tmax", "tmin"),
      na.strings = "-99.9")
    no_natmax <- length(subset(data$tmax, !is.na(data$tmax)))
    if (no_natmax > 0) {
        file_name <- paste("flatline_tmax.txt", sep = "")
        tmax <- data$tmax
        vector_count <- rle(tmax)$lengths
        id_value <- cumsum(vector_count)
        value_flat <- which(vector_count > 3)
        number_flat <- subset(vector_count, vector_count > 3)
        flat <- id_value[value_flat]
        tmax_flatline <- data[flat, c(1:3, 5)]
        tmax_flatline <- cbind(tmax_flatline, number_flat)
        write.table(tmax_flatline, file = file_name, quote = FALSE,
          row.names = FALSE, col.names = TRUE)
        rm(list = ls())
        print("Tmax tolerance test finished")
        print("Check < flatlines_tmax.txt >")
    }
    file <- file.choose()
    data <- read.table(file,
      col.names = c("year", "month", "day", "rr", "tmax", "tmin"),
      na.strings = "-99.9")
    no_natmin <- length(subset(data$tmin, !is.na(data$tmin)))
    if (no_natmin > 0) {
        file_name <- paste("flatline_tmin.txt", sep = "")
        tmin <- data$tmin
        vector_count <- rle(tmin)$lengths
        id_value <- cumsum(vector_count)
        value_flat <- which(vector_count > 3)
        number_flat <- subset(vector_count, vector_count > 3)
        flat <- id_value[value_flat]
        tmin_flatline <- data[flat, c(1:3, 6)]
        tmin_flatline <- cbind(tmin_flatline, number_flat)
        write.table(tmin_flatline, file = file_name, quote = FALSE,
          row.names = FALSE, col.names = TRUE)
        rm(list = ls())
        print("Tmin tolerance test finished")
        print("Check < flatlines_tmin.txt >")
    }
}
