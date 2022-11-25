#' Create a Table of Descriptive Statistics
#'
#' Function to create a table of descriptive statistics, such as mean
#' and median and proportions, given a dataset.
#' @param table.data A data.frame. The data to use when creating the
#'     table. No default.
#' @param strata Character or NULL. The variable to use as the stratifying variable. If NULL the table is not stratified. Defaults to NULL.
#' @param include.overall Logical. If TRUE a column called "Overall" is included in the table, which includes descriptive statistics for the whole sample. Defaults to TRUE.
create_descriptive_table <- function(table.data, strata = NULL, include.overall = TRUE) {
    ## Check arguments
    assertthat::assert_that(is.data.frame(table.data))
    assertthat::assert_that(is.character(strata) | is.null(strata))
    assertthat::assert_that(is.logical(include.overall))

    ## Create table
    if (!is.null(strata)) {
        table.data$strata <- table.data[, strata]
        table.data[, strata] <- NULL
    }
    if (include.overall) {
        overall.data <- table.data
        overall.data$strata <- "Overall"
        table.data <- cbind(overall.data, table.data)
    }
        
}
