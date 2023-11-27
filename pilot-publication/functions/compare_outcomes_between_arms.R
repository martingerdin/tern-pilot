#' Compare outcomes between two arms in a clinical trial
#'
#' This function takes the combination of two arms in a clinical trial, the name of the outcomes table, and a list of outcomes tables as input.
#' It calculates the absolute and relative differences between the outcomes of the two arms and returns the comparison as a tibble.
#'
#' @param arm.combination A vector of length 2, specifying the names of the two arms to compare.
#' @param table.name A character string specifying the name of the outcomes table to be used for comparison.
#' @param outcomes.tables A list containing the outcomes tables, each stored as a data frame.
#'
#' @return A tibble containing the absolute and relative differences between the outcomes of the two arms.
#'
#' @importFrom dplyr select add_column as_tibble
#' @importFrom assertthat assert_that is.list is.data.frame
#' @export
compare_outcomes_between_arms <- function(arm.combination, table.name, outcomes.tables, outcomes.row.names) {
    assertthat::assert_that(is.list(outcomes.tables))
    assertthat::assert_that(is.data.frame(outcomes.tables[[table.name]]))
    outcomes.table <- outcomes.tables[[table.name]] %>%
        select(contains(arm.combination[1]), contains(arm.combination[2]))
    ## Calculate the difference between the two arms
    outcomes.comparison <- data.frame(
        absolute.difference = outcomes.table[, 1] - outcomes.table[, 2],
        relative.difference = outcomes.table[, 1] / outcomes.table[, 2]
    )
    colnames(outcomes.comparison) <- c("Absolute difference", "Relative difference")
    ## Convert outcomes comparison to tibble
    outcomes.comparison <- as_tibble(outcomes.comparison) %>%
        add_column(outcomes.row.names, .before = 1)
    ## Return the table
    return(outcomes.comparison)
}
