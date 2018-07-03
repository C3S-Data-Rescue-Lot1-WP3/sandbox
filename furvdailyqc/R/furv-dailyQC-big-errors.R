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
  if (exists('daily_data')) {
    daily_data <- daily_data
  } else {
    daily_data_file <- file.choose()
    daily_data <- read.table(daily_data_file, 
      col.names = c("year", "month", "day", "rr", "tmax", "tmin"),
      na.strings = "-99.9")
    assign("daily_data", daily_data, envir=globalenv())
  }
  write("          ",file="")
  tmax_upper_thres <- readline("Tmax upper threshold: ")
  tmax_upper_thres <- as.numeric(tmax_upper_thres)
  tmax_lower_thres <- readline("Tmax lower threshold: ")
  tmax_lower_thres <- as.numeric(tmax_lower_thres)
  tmin_upper_thres <- readline("Tmin upper threshold: ")
  tmin_upper_thres <- as.numeric(tmin_upper_thres)
  tmin_lower_thres <- readline("Tmin lower threshold: ")
  tmin_lower_thres <- as.numeric(tmin_lower_thres)
  rr_upper_thres <- readline("RR upper threshold: ")
  rr_upper_thres <- as.numeric(rr_upper_thres)
  
  out <- subset(daily_data, (daily_data$tmax > tmax_upper_thres | 
      daily_data$tmax < tmax_lower_thres | daily_data$tmin > tmin_upper_thres | 
      daily_data$tmin < tmin_lower_thres | daily_data$rr > rr_upper_thres))
  
  file_name <- paste("out_of_range.txt", sep = "")
  write.table(out, file = file_name, quote = FALSE, row.names = FALSE,
    col.names = FALSE)
  rm(list = ls())
  print("Big errors test finished")
  print("Check < out_of_range.txt >")
}

