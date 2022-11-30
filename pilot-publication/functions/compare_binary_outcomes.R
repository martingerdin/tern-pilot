#' Compare binary outcomes
#'
#' Compares binary outcomes between ATLS, PTC and control arms
#' @param arms.data.list A list of data.frames. The ATLS, PTC and control arms data. No default.
compare_binary_outcomes <- function(arms.data.list) {
    ## Check arguments
    assertthat::assert_that(is.list(arms.data.list) & all(sapply(arms.data.list, is.data.frame)))
    ## Estimate
    arms <- names(arms.data.list)
    estimates <- lapply(arms.data.list, function(arm.data) {
        outcome.estimates <- lapply(binary_outcomes(), estimate_binary_outcome, arm.data) %>%
            bind_rows()
        return (outcome.estimates)
    })
    ## Compare
    comparisons <- combn(names(arms.data.list), 2, simplify = FALSE)
}

estimate_binary_outcome <- function(binary.outcome, arm.data) {
    outcome.column <- arm.data %>% pull(.data[[binary.outcome$name]])
    count <- sum(outcome.column == binary.outcome$positive.class, na.rm = TRUE)
    percentage <- round(count/nrow(arm.data) * 100)
    outcome.estimate <- data.frame(count = count, percentage = percentage)
    rownames(outcome.estimate) <- binary.outcome$label
    return (outcome.estimate)
}
