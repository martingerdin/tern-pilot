#' Create a Table of Descriptive Statistics
#'
#' Function to create a table of descriptive statistics, such as mean
#' and median and proportions, given a dataset.
#' @param table.data A data.frame. The data to use when creating the
#'     table. No default.
#' @param variables Character or NULL. The variables to be included in
#'     the table. Defaults to NULL, in which case all variables in
#'     table.data are included.
#' @param strata Character or NULL. The variable to use as the
#'     stratifying variable. If NULL the table is not
#'     stratified. Defaults to NULL.
#' @param show.all.levels Logical. If TRUE all levels of a factor are shown, even for binary variables. Defaults to TRUE.
#' @param include.overall Logical. If TRUE a column called "Overall"
#'     is included in the table, which includes descriptive statistics
#'     for the whole sample. Defaults to TRUE, unless strata is NULL,
#'     in which case it defaults to FALSE.
#' @param caption Character or NULL. The table caption. Defaults to
#'     NULL, as in no caption.
#' @param return.as.gt Logical. If TRUE the table is returned as a gt
#'     object. Defaults to FALSE.
create_descriptive_table <- function(table.data,
                                     variables = NULL,
                                     strata = NULL,
                                     show.all.levels = TRUE,
                                     include.overall = TRUE,
                                     caption = NULL,
                                     return.as.gt = FALSE) {
    ## Define pipe operator
    `%>%` <- magrittr::`%>%`

    ## Check arguments
    assertthat::assert_that(is.data.frame(table.data))
    assertthat::assert_that(is.character(variables) | is.null(variables))
    assertthat::assert_that(is.character(strata) | is.null(strata))
    assertthat::assert_that(is.logical(include.overall))
    assertthat::assert_that(is.character(caption) | is.null(caption))

    ## Set include.overall to FALSE if strata is NULL
    if (is.null(strata)) {
        include.overall <- FALSE
    }

    ## Create table
    table.data <- as.data.frame(table.data)
    if (!is.null(variables)) {
        table.data <- table.data[, variables]
    }

    type <- list()
    if (show.all.levels) {
        type <- list(all_dichotomous() ~ "categorical")
    }

    descriptive.table <- gtsummary::tbl_summary(
        data = table.data,
        by = strata,
        type = type,
        missing_text = "Missing"
    )

    ## Add overall column
    if (include.overall) {
        descriptive.table <- descriptive.table %>%
            gtsummary::add_overall(last = TRUE)
    }

    ## Add caption
    if (!is.null(caption)) {
        descriptive.table <- descriptive.table %>%
            gtsummary::modify_caption(caption)
    }

    ## Return table as gt object
    if (return.as.gt) {
        descriptive.table <- gtsummary::as_gt(descriptive.table)
    }

    ## Return table
    return(descriptive.table)
}
