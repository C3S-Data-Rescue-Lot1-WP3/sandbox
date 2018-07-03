#'
#' Duplicate dates test:
#' includes all dates that appears more than once.
#'
#'
#' @details Input: A text file: each line is for one calendar day.
#'   The order of the columns should be: Year - Month - daily precipitation
#'   (RR) - Daily Maximum temperature (Tmax) - Daily Minimum Temperature (Tmin).
#'   Without header and NA values = -99.9.
#'
#'   Output: writes a list with duplicated dates, that appear more than
#'   once. Each line represents one day and the order of columns is: Year - Month -
#'   RR - Tmax - Tmin.
#'
#' @usage duplicate_dates()
#'
#' @import stats
#' @import utils
#'
#' @export

duplicate_dates <- function() {
  if (exists('daily_data')) {
    daily_data <- daily_data
  } else {
    daily_data_file <- file.choose()
    daily_data <- read.table(daily_data_file, 
      col.names = c("year", "month", "day", "rr", "tmax", "tmin"),
      na.strings = "-99.9")
    assign("daily_data", daily_data, envir=globalenv())
  }
    file_name <- paste("duplicate_dates.txt", sep = "")
    is_dupli <- cbind(daily_data$year, daily_data$month, daily_data$day)
    write.table(subset(is_dupli, duplicated(is_dupli) == TRUE),
      file = file_name, quote = FALSE, row.names = FALSE, col.names = FALSE)
    rm(list = ls())
    print("Duplicate dates test finished")
    print("Check < duplicate_dates.txt >")
}
