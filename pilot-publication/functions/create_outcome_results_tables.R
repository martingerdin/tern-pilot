#' Create outcome results tables
#'
#' This function takes in the outcome results and creates tables summarizing the results.
#'
#' @param outcome.results The outcome results data.
#'
#' @return The outcome results tables.
#'
#' @export
create_outcome_results_tables <- function(outcome.results) {
    ## Check arguments
    assertthat::assert_that(
        is.list(outcome.results),
        "point.estimates" %in% names(outcome.results),
        is.numeric(outcome.results$point.estimates),
        "confidence.intervals" %in% names(outcome.results),
        is.list(outcome.results$confidence.intervals)
    )

    ## Extract cell references
    cell.references <- names(outcome.results$point.estimates)

    ## Extract table names
    table.names <- strsplit(cell.references, split = "\\|\\|\\.") %>%
        lapply(function(table.name) table.name[1]) %>%
        unique()

    ## Check table names
    assertthat::assert_that(identical(unlist(table.names), get_table_names(return.original.names = TRUE)),
        msg = "Invalid table names, maybe they have been changed?"
    )

    ## Extract confidence interval levels
    ci.levels <- outcome.results$confidence.intervals[[2]][, 1]

    ## Combine point estimates and confidence intervals into a single table
    combined.outcome.results <- lapply(ci.levels, function(ci.level) {
        lapply(cell.references, function(cell.reference) {
            point.estimate <- outcome.results$point.estimates[[cell.reference]]
            ci.data <- outcome.results$confidence.intervals[[cell.reference]]
            ci <- ci.data[ci.data[, "level"] == ci.level, c("lower", "upper")]
            combined <- NA
            if (!all(is.na(ci))) {
                combined <- round(c(point.estimate, ci), 2)
                combined <- paste0(combined[1], " (", combined[2], ", ", combined[3], ")")
            }
            return(combined)
        }) %>%
            setNames(cell.references)
    }) %>%
        setNames(paste0("ci.level.", ci.levels))

    ## Change table names
    new.table.names <- get_table_names()

    ## Create tables for each confidence interval level
    tables <- lapply(
        combined.outcome.results, function(ci.data) {
            {
                lapply(table.names, create_ci_level_tables, ci.data = ci.data)
            } %>%
                setNames(new.table.names)
        }
    )

    ## Return tables
    return(tables)
}

## Create outcome tables
#' Create confidence interval level tables
#'
#' This function creates tables for displaying confidence interval (CI) level results.
#'
#' @param table.name The name of the table.
#' @param ci.data A list containing the CI data.
#'
#' @return A table object in the form of a gt object.
#'
#' @export
create_ci_level_tables <- function(table.name, ci.data) {
    ## Check arguments
    assertthat::assert_that(
        is.character(table.name),
        is.list(ci.data)
    )

    ## Extract table data
    table.data <- ci.data[grep(table.name, names(ci.data), fixed = TRUE)]
    column.names <- strsplit(names(table.data), split = "\\|\\|\\.") %>%
        lapply(function(column) column[2]) %>%
        unique() %>%
        stringr::str_replace_all(pattern = "\\|\\|column\\:\\:", replacement = "") %>%
        stringr::str_replace_all(pattern = "\\*\\*", replacement = "")
    row.names <- strsplit(names(table.data), split = "\\|\\|\\.") %>%
        lapply(function(row) row[3]) %>%
        unique() %>%
        stringr::str_replace_all(pattern = "\\|\\|row\\:\\:index\\:[0-9]*\\:\\:", replacement = "") %>%
        stringr::str_replace_all(pattern = "\\|\\|", replacement = "")

    ## Replace outcome names with labels
    all.outcomes <- all_outcomes()
    outcomes.column.list <- lapply(row.names, function(row.name) {
        new.row.name <- row.name
        index <- FALSE
        if (!is.null(all.outcomes[[row.name]])) {
            new.row.name <- all.outcomes[[row.name]]$label
            index <- TRUE
        }
        return(list(row.name = new.row.name, index = index))
    })
    outcomes.column.index <- sapply(outcomes.column.list, function(outcomes.column) outcomes.column$index)
    outcomes.column <- sapply(outcomes.column.list, function(outcomes.column) outcomes.column$row.name)

    ## Create table
    ci.table <- matrix(table.data, ncol = length(column.names)) %>%
        tibble::as_tibble(.name_repair = "minimal") %>%
        tibble::add_column(outcomes.column, .before = 1, .name_repair = "minimal") %>%
        setNames(c("Outcome", column.names))

    ## Replace NA with empty string
    ci.table[] <- lapply(ci.table, function(column) {
        column[is.na(column)] <- ""
        return(column)
    })

    ## Convert table to gt
    ci.table <- ci.table %>%
        gt::gt() %>%
        gt::tab_style(
            style = gt::cell_text(weight = "bold"),
            locations = gt::cells_body(
                columns = c(Outcome),
                rows = outcomes.column.index
            )
        ) %>%
        gt::tab_style(
            style = gt::cell_text(weight = "bold"),
            locations = gt::cells_column_labels()
        ) %>%
        gt::tab_options(
            table.font.size = 10
        )

    ## Return table
    return(ci.table)
}

get_table_names <- function(return.original.names = FALSE) {
    ## Define original table names
    table.names <- list(
        "||table::overall" = "Outcomes in all patients during the entire study period, by treatment arm",
        "||table::pre.training" = "Outcomes in all patients before training, by treatment arm",
        "||table::post.training" = "Outcomes in all patients after training, by treatment arm",
        "||table::absolute.difference" = paste0(
            "Absolute change from baseline for all outcomes,",
            " comparing the period after training with the period before training,",
            " by treatment arms"
        ),
        "||table::relative.difference" = paste0(
            "Relative change from baseline for all outcomes,",
            " comparing the period after training with the period before training,",
            " by treatment arms"
        ),
        "||table::Post training outcome Standard care vs. ATLS" = paste0(
            "Absolute and relative differences in outcomes after training,",
            " comparing standard care with ATLS"
        ),
        "||table::Post training outcome Standard care vs. PTC" = paste0(
            "Absolute and relative differences in outcomes after training,",
            " comparing standard care with PTC"
        ),
        "||table::Post training outcome ATLS vs. PTC" = paste0(
            "Absolute and relative differences in outcomes after training,",
            " comparing ATLS with PTC"
        ),
        "||table::Change from baseline Standard care vs. ATLS" = paste0(
            "Absolute and relative differences in changes from baseline for all outcomes,",
            " comparing standard care with ATLS"
        ),
        "||table::Change from baseline Standard care vs. PTC" = paste0(
            "Absolute and relative differences in changes from baseline for all outcomes,",
            " comparing standard care with PTC"
        ),
        "||table::Change from baseline ATLS vs. PTC" = paste0(
            "Absolute and relative differences in changes from baseline for all outcomes,",
            " comparing ATLS with PTC"
        )
    )

    ## Return original table names
    if (return.original.names) {
        table.names <- names(table.names)
    }

    ## Return table names
    return(table.names)
}
