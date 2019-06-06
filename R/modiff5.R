#' Approximate number of months between two dates rounded to nearest half month
#'
#' Calculates the approximate number of months between two dates rounded to the nearest half month.  This function was created with the idea that dt1 and dt2 are already rounded using rnddt().
#' @param dt1 The starting date or vector of dates.
#' @param dt2 The ending date or vector of dates.
#' @return The approximate number of months between the dates rounded to the nearest half month.
#' @export
#' @examples
#' modiff5(as.Date("2015-01-01"),as.Date("2015-02-23"))
#' modiff5(as.Date("2015-01-01"),as.Date("2015-02-24"))
modiff5 <- function(dt1, dt2){
  return(round(as.numeric(dt2 - dt1) * 12 / 365.25 / .5) * .5)
}
