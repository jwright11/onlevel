#' Number of days in a month
#'
#' Calculates the number of days in the month of a given date.
#' @param dt A date or vector of dates.
#' @return The number of days in the month of a given date.
#' @export
#' @examples
#' daysinmonth(as.Date("2016-01-15"))
#' daysinmonth(as.Date(c("2015-02-19","2016-02-11")))
daysinmonth <- function(dt){
  m1 <- as.numeric(format(dt,"%m"))
  y1 <- as.numeric(format(dt,"%Y"))
  m2 <- ifelse(m1 == 12, 1, m1 + 1)
  y2 <- ifelse(m1 == 12, y1 + 1, y1)
  d1 <- as.Date(paste(y1, m1, "1", sep = "-"))
  d2 <- as.Date(paste(y2, m2, "1", sep = "-"))
  return(as.numeric(d2 - d1))
}
