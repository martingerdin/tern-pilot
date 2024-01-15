#' Create a retrospective comparison table
#'
#' This function takes in a data frame and creates a comparison table
#' between retrospective and directly observed data.
#' @param data A data frame containing the data for comparison.
#' @return A comparison table with descriptive statistics.
#' @export
create_retrospective_comparison_table <- function(data) {
    ## Define borrowed functions
    `%>%` <- magrittr::`%>%`

    ## Check arguments
    assertthat::assert_that(is.data.frame(data))

    ## Get variables to compare
    retrospective.variables <- names(data)[grep("_r__", names(data), fixed = TRUE)]
    directly.observed.variables <- retrospective.variables %>%
        stringr::str_replace_all(pattern = "\\_r\\_\\_", replacement = "\\_\\_") %>%
        stringr::str_replace_all(pattern = "\\_r$", replacement = "")

    ## Keep only observations with completed retrospective data collection
    data <- data %>% dplyr::filter(data_status_r__collection_completed_r == "Yes")

    ## Extract retrospective and directly observed data
    retrospective.data <- data[, retrospective.variables]
    colnames(retrospective.data) <- directly.observed.variables
    directly.observed.data <- data[, directly.observed.variables]
    retrospective.data$collection.mode <- "Retrospective"
    directly.observed.data$collection.mode <- "Directly observed"
    original.classes <- sapply(directly.observed.data, class)

    ## Merge datasets
    directly.observed.data[] <- lapply(directly.observed.data, as.character)
    retrospective.data[] <- lapply(retrospective.data, as.character)
    merged.data <- dplyr::bind_rows(directly.observed.data, retrospective.data)

    ## Change column classes to match original classes
    merged.data[] <- mapply(function(column, class.name) {
        class.function <- paste0("as.", class.name[1])
        new.column <- match.fun(class.function)(column)
        return(new.column)
    }, merged.data, original.classes, SIMPLIFY = FALSE)
    merged.data <- merged.data %>%
        labelled::copy_labels_from(data)

    ## Create table
    table.variables <- c(
        "patinfo__pt_age", "patinfo__pt_gender",
        "incident__dominating_injury_type",
        "patvitals__ed_rr", "patvitals__ed_sat",
        "patvitals__ed_hr", "patvitals__ed_sbp",
        "collection.mode"
    )
    table.data <- merged.data[, table.variables]
    comparison.table <- create_descriptive_table(table.data,
        strata = "collection.mode",
        include.overall = FALSE
    )

    ## Return result
    return(comparison.table)
}
