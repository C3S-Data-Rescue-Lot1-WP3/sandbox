
#'
#' Temporal coherence test:
#' determines those records where daily maximum or minimum temperature differences
#' with previous day is too large.
#'
#' Given daily maximum (Tmax) and minimum (Tmin) temperatures values of two consecutive days,
#' reports those records where the Tmax or Tmin differences with the previous day is >= 20 degC.
#' #' The output is a list with the days in which Tmax or Tmin of two consecutive days is
#' >= 20 degC.
#'
#' @details Input: A text file: each line is for one calendar day.
#'   The order of the columns should be: Year - Month - RR - Tmax - Tmin.
#'   Without header and NA values = -99.9.
#'
#'   Output: writes two lists (one for Tmax and one for Tmin) with suspicious values
#'   (Tmax or Tmin of two consecutive days >= 20 degC) <jumpstmax.txt and jumpstmin.txt>;
#'   each line represents one day and the order of columns is: Year - Month -
#'   RR - Tmax - Tmin.
#'
#' @usage temporal_coherence()
#'
#' @import stats
#' @import utils
#'
#' @export

temporal_coherence <- function() {
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
    if (no_natmax > 0) {
        dif_tmax <- abs(round(diff(daily_data$tmax, lag = 1, differences = 1),
          digits = 1))
        id_dif <- data.frame(c.ndx = cumsum(rle(dif_tmax)$lengths),
          c.type = rle(dif_tmax)$values)
        id_dif <- na.omit(id_dif)
        names(id_dif) <- c("id", "value")
        tmax_table <- data.frame(id = row(daily_data), year = daily_data$year,
          month = daily_data$month, day = daily_data$day, day = daily_data$tmax)
        tmax_table <- tmax_table[, 6:10]
        names(tmax_table) <- c("id", "year", "month", "day", "tmax")
        jumps <- merge(tmax_table, id_dif, by = "id", all.id_dif = F,
          all.tmax_table = T)
        jumps <- subset(jumps, (jumps$value >= 20))
        jumps <- jumps[, -1]
        names(jumps) <- c("year", "month", "day", "Tmax", "jump")
        file_name <- paste("jumps_tmax.txt", sep = "")
        write.table(jumps, file = file_name, quote = FALSE, row.names = FALSE,
          col.names = TRUE)
        rm(list = ls())
        print("Internal consistency test finished")
        print("Check < jumps_tmax.txt >")
    }
    # now we go with Tmin
    if (exists('daily_data')) {
      daily_data <- daily_data
    } else {
      daily_data_file <- file.choose()
      daily_data <- read.table(daily_data_file, 
        col.names = c("year", "month", "day", "rr", "tmax", "tmin"),
        na.strings = "-99.9")
      assign("daily_data", daily_data, envir=globalenv())
    }
    no_natmin <- length(subset(daily_data$tmin, !is.na(daily_data$tmin)))
    if (no_natmin > 0) {
        dif_tmin <- abs(round(diff(daily_data$tmin, lag = 1, differences = 1),
          digits = 1))
        id_dif <- data.frame(c.ndx = cumsum(rle(dif_tmin)$lengths),
          c.type = rle(dif_tmin)$values)
        id_dif <- na.omit(id_dif)
        names(id_dif) <- c("id", "value")
        tmin_table <- data.frame(id = row(daily_data), year = daily_data$year,
          month = daily_data$month, day = daily_data$day, day = daily_data$tmin)
        tmin_table <- tmin_table[, 6:10]
        names(tmin_table) <- c("id", "year", "month", "day", "tmin")
        jumps <- merge(tmin_table, id_dif, by = "id", all.id_dif = F,
          all.tmin_table = T)
        jumps <- subset(jumps, (jumps$value >= 20))
        jumps <- jumps[, -1]
        names(jumps) <- c("year", "month", "day", "Tmin", "jump")
        file_name <- paste("jumps_tmin.txt", sep = "")
        write.table(jumps, file = file_name, quote = FALSE, row.names = FALSE,
          col.names = TRUE)
        rm(list = ls())
        print("Internal consistency test finished")
        print("Check < jumps_tmin.txt >")
    }
}
