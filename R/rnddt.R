#' Round a date to the nearest half month
#'
#' Takes a date or vector of dates and rounds each value to the nearest half month.  Days 1 to 8 get rounded down to 1, days 9 to 23 get rounded to the 15th, and days 23+ get rounded to the 1st of the following month.
#' @param dt A date or vector of dates to be rounded.
#' @return The rounded date or vector of dates.
#' @export
#' @examples
#' dt <- seq(as.Date("2015-01-01"), to = as.Date("2015-01-31"), by = "day")
#' data.frame(dt = dt, rnddt = rnddt(dt))
rnddt <- function(dt){
  d <- as.numeric(format(dt, "%d"))
  f <- ifelse(d <= 8, dt - d + 1,
              ifelse(d <= 23, dt - d + 15,
                     dt - d + daysinmonth(dt) + 1))
  return(as.Date(f, origin = "1970-01-01"))
}
