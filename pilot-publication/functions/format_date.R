#' Format Date
#'
#' Formats a date by extracting the month and year.
#'
#' @param date A vector of Date objects.
#'
#' @return A character vector with the formatted date in the format "month year". No default.
#'
#' @examples
#' format_date(Sys.Date())
#'
#' @export
format_date <- function(date) {
  assertthat::assert_that(is.Date(date), msg = "The 'date' parameter must be a Date object.")
  assertthat::assert_that(length(date) >= 1, msg = "The 'date' parameter must contain at least one element.")
  
  formatted.date <- paste0(month(date[1], label = TRUE, abbr = FALSE), " ", year(date[1]))
  return (formatted.date)
}


