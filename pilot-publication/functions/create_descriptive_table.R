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
#' @param include.overall Logical. If TRUE a column called "Overall"
#'     is included in the table, which includes descriptive statistics
#'     for the whole sample. Defaults to TRUE.
#' @param use.labels Logical. If TRUE the function checks if the data
#'     is labelled and if so, uses those labels. Defaults to TRUE.
#' @param caption Character or NULL. The table caption. Defaults to
#'     NULL, as in no caption.
create_descriptive_table <- function(table.data,
                                     variables = NULL,
                                     strata = NULL,
                                     include.overall = TRUE,
                                     use.labels = TRUE,
                                     caption = NULL) {
    ## Check arguments
    assertthat::assert_that(is.data.frame(table.data))
    assertthat::assert_that(is.character(variables) | is.null(variables))
    assertthat::assert_that(is.character(strata) | is.null(strata))
    assertthat::assert_that(is.logical(include.overall))
    assertthat::assert_that(is.logical(use.labels))
    assertthat::assert_that(is.character(caption) | is.null(caption))

    ## Create table
    table.data <- as.data.frame(table.data)
    if (!is.null(variables))
        table.data <- table.data[, variables]
    strata.data <- rep("Overall", nrow(table.data))
    if (!is.null(strata)) {
        strata.data <- table.data[, strata]
        table.data[, strata] <- NULL
    }
    strata.list <- base::split(table.data, as.factor(strata.data))
    if (!is.null(strata) & include.overall)
        strata.list <- c(strata.list, list("Overall" = table.data))
    labels <- list(variables = setNames(nm = names(table.data)))
    if (use.labels) {
        labels <- lapply(labels$variables, function(column.name) {
            label <- table.data %>% dplyr::pull(.data[[column.name]]) %>% attr("label")
            if (is.null(label))
                label <- column.name
            return (label)
        })
        labels <- list(variables = labels)
    }
    descriptive.table <- table1::table1(strata.list,
                                        labels = labels,
                                        caption = caption,
                                        droplevels = TRUE,
                                        render.missing = table1::render.missing.default)
    return(descriptive.table)
}
