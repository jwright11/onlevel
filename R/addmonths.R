#' Add a specified number of months to a date
#'
#' Adds a specified number of months to a given date.  If the number of days in the start date is greater than the number of days in the month of the end date, the last day of the month is returned.
#' @param dt A date or vector of dates.
#' @param m An integer or vector of integers representing the number of months to add.
#' @return A date or vector of dates with the number of months added.
#' @export
#' @examples
#' addmonths(as.Date("2015-01-31"),13)
#' addmonths(as.Date("2015-02-28"),-2)
#' addmonths(as.Date(c("2015-01-31","2015-02-28")),-2)
#' addmonths(as.Date(c("2015-01-31","2015-02-28")),c(-1,-2))
addmonths <- function(dt, m){
  m1 <- as.numeric(format(dt,"%m"))
  y1 <- as.numeric(format(dt,"%Y"))
  d1 <- as.numeric(format(dt,"%d"))
  m2 <- ((m1 + m - 1) %% 12) + 1
  y2 <- y1 + ifelse(m1 + m <= 0, trunc((m1 + m) / 12) - 1, trunc((m1 + m - 1) / 12))
  j1 <- as.Date(paste(y2, m2, "1", sep = "-"))
  d2 <- pmin(d1, daysinmonth(j1))
  return(as.Date(paste(y2, m2, d2, sep = "-")))
}
