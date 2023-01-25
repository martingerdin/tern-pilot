#' Get Inter Quartile Range
#'
#' @param data Numeric. The data for which to calculate the interquartile range. No default.
get_iqr <- function(data) {
    assertthat::assert_that(is.numeric(data))
    paste0(quantile(data, probs = c(0.25, 0.75), na.rm = TRUE), collapse = "-")
}
