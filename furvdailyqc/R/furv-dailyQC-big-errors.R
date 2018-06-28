#'
#' Big errors test:
#' determines the daily maximum, minimum and precipitation that exceed a
#' given threshold.
#'
#' Given daily maximum (Tmax) and minimum (Tmin) temperatures and daily precipitation
#' (RR) values, reports RR values exceeding 200mm and Tmax and Tmin values exceeding
#' 50 degC.The output is a list with the days in which RR or Tmax or Tmin exceeds
#' these thresholds.
#'
#' @details Input: A text file: each line is for one calendar day.
#'   The order of the columns should be: Year - Month - RR - Tmax - Tmin.
#'   Without header and NA values = -99.9.
#'
#'   Output: writes a list with suspicious values (RR > 200 mm or Tmax > 50 degC
#'   or Tmax < -50 degC or Tmin > 50 degC or Tmin < -50 degC) in <out_of_range.txt>;
#'   each line represents one day and the order of columns is: Year - Month -
#'   RR - Tmax - Tmin.
#'
#' @usage out_of_range()
#'
#' @import stats
#' @import utils
#'
#' @export

out_of_range <- function() {
  data <- read.table(file.choose(),
    col.names = c("year", "month", "day", "rr", "tmax", "tmin"),
    na.strings = "-99.9")
  out <- subset(data, (data$tmax > 50 | data$tmax < -50 | data$tmin > 50 |
      data$tmin < -50 | data$rr > 200 | data$rr < 0))
  file_name <- paste("out_of_range.txt", sep = "")
  write.table(out, file = file_name, quote = FALSE, row.names = FALSE,
    col.names = FALSE)
  rm(list = ls())
  print("Big errors test finished")
  print("Check < out_of_range.txt >")
}
