#'
#' Climatic outliers test:
#'
#' considers as outliers all daily maximum and minimum temperature values falling
#' outside a range with p25 - 3 interquartile ranges (IQR, lower bound) and p75 + 3 interquartile
#' ranges (upper bound). For daily precipitation values the same range but using 5 IQR.
#'
#' @details   There are one graphical and one text outputs. The graphical output produces four boxplots
#'   one for precipitation (RR), one for daily maximum temperature (Tmax), one for daily minimum temperature
#'   (Tmin) and another one for daily temperature range (DTR).Values flagged as outlier represented by
#'   round circles. The values identified by these boxplots are listed with a simple text file.
#'   The file lists the outliers grouped by the element that produced the inclusion of the value
#'   as suspicious.
#'
#'   Input: A text file: each line is for one calendar day.
#'   The order of the columns should be: Year - Month - RR - Tmax - Tmin.
#'   Without header and NA values = -99.9.
#'
#'   Output: writes a list with suspicious values (outliers) <climatic_outliers.txt>;
#'   each line represents one day and the order of columns is: Year - Month -
#'   RR - Tmax - Tmin - DTR. Grouped by the element that produced the inclusion of the value
#'   as suspicious
#'
#' @usage climatic_outliers()
#'
#' @import grDevices
#' @import graphics
#' @import stats
#' @import utils
#'
#' @export

climatic_outliers <- function() {
    name <- paste("climatic_outliers_boxplots.pdf", sep = "")
    pdf(file = name)
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
    outrange <- 3
    if (no_natmax > 0 & no_natmin > 0) {
      daily_data$dtr <- daily_data$tmax - daily_data$tmin
    }
    if (no_narr > 0) {
        rr_noz <- subset(daily_data, daily_data$rr > 0)
    }
    par(mfrow = c(2, 2))
    if (no_narr > 0) {
        boxplot_rr <- boxplot(rr_noz$rr ~ rr_noz$month, main = "NON ZERO RR",
          col = "blue", range = outrange + 2)
    }
    if (no_natmax > 0) {
        boxplot_tmax <- boxplot(daily_data$tmax ~ daily_data$month, main = "Tmax",
          col = "red", range = outrange)
    }
    if (no_natmin > 0) {
        boxplot_tmin <- boxplot(daily_data$tmin ~ daily_data$month, main = "Tmin",
          col = "cyan", range = outrange)
    }
    if (no_natmax > 0 & no_natmin > 0) {
        boxplot_dtr <- boxplot(daily_data$dtr ~ daily_data$month, main = "DTR",
          col = "yellow", range = outrange)
    }

    # open a file for writing outliers
    file_name <- paste("climatic_outliers.txt", sep = "")

    # now we go with precipitation
    if (no_narr > 0) {
        write.table("RR up", file = file_name, append = FALSE, quote = FALSE,
          row.names = FALSE, col.names = FALSE)
        mis_months <- NULL
        for (months in 1:12) {
            out_rr <- subset(daily_data$rr, daily_data$month == months & daily_data$rr > 0)
            if (length(out_rr) > 0) {
                mis_months <- c(mis_months, months)
            }
        }
        baximo <- length(mis_months)
        for (a in 1:baximo) {
            out_rr <- subset(daily_data, daily_data$month == mis_months[a] &
                daily_data$rr > boxplot_rr$stats[5, a])
            write.table(out_rr, file = file_name, append = TRUE, quote = FALSE,
              row.names = FALSE, col.names = FALSE)
        }
    }
    # now daily maximum temperature
    if (no_natmax > 0) {
        write.table("Tmax up", file = file_name, append = TRUE, quote = FALSE,
          row.names = FALSE, col.names = FALSE)
        for (a in 1:12) {
            out_tmax <- subset(daily_data, daily_data$month == a &
                daily_data$tmax > boxplot_tmax$stats[5, a])
            write.table(out_tmax, file = file_name, append = TRUE,
              quote = FALSE, row.names = FALSE, col.names = FALSE)
        }
        write.table("Tmax low", file = file_name, append = TRUE, quote = FALSE,
          row.names = FALSE, col.names = FALSE)
        for (a in 1:12) {
            out_tmax <- subset(daily_data, daily_data$month == a &
                daily_data$tmax < boxplot_tmax$stats[1, a])
            write.table(out_tmax, file = file_name, append = TRUE,
              quote = FALSE, row.names = FALSE, col.names = FALSE)
        }
    }
    # now daily minimum temperature
    if (no_natmin > 0) {
        write.table("Tmin up", file = file_name, append = TRUE, quote = FALSE,
          row.names = FALSE, col.names = FALSE)
        for (a in 1:12) {
            out_tmin <- subset(daily_data, daily_data$month == a &
                daily_data$tmin > boxplot_tmin$stats[5, a])
            write.table(out_tmin, file = file_name, append = TRUE,
              quote = FALSE, row.names = FALSE, col.names = FALSE)
        }
        write.table("Tmin low", file = file_name, append = TRUE, quote = FALSE,
          row.names = FALSE, col.names = FALSE)
        for (a in 1:12) {
            out_tmin <- subset(daily_data, daily_data$month == a &
                daily_data$tmin < boxplot_tmin$stats[1, a])
            write.table(out_tmin, file = file_name, append = TRUE,
              quote = FALSE, row.names = FALSE, col.names = FALSE)
        }
    }
    # now daily temperature range
    if (no_natmax > 0 & no_natmin > 0) {
        write.table("DTR up", file = file_name, append = TRUE, quote = FALSE,
          row.names = FALSE, col.names = FALSE)
        for (a in 1:12) {
            out_dtr <- subset(daily_data, daily_data$month == a &
                daily_data$dtr > boxplot_dtr$stats[5, a])
            write.table(out_dtr, file = file_name, append = TRUE, quote = FALSE,
              row.names = FALSE, col.names = FALSE)
        }
        write.table("DTR low", file = file_name, append = TRUE, quote = FALSE,
          row.names = FALSE, col.names = FALSE)
        for (a in 1:12) {
            out_dtr <- subset(daily_data, daily_data$month == a &
                daily_data$dtr < boxplot_dtr$stats[1, a])
            write.table(out_dtr, file = file_name, append = TRUE, quote = FALSE,
              row.names = FALSE, col.names = FALSE)
        }
    }
    dev.off()
    rm(list = ls())
    print("Climatic outliers test finished")
    print("Check < climatic_outliers.txt >")
    print("Check < climatic_outliers.pdf >")
}
