#' Compare binary outcomes
#'
#' Compares binary outcomes between ATLS, PTC and control arms
#' @param arms.data.list A list of data.frames. The ATLS, PTC and
#'     control arms data. No default.
#' @param pre.post.break.points A list or NULL. Specifies the date that should
#'     be considered the break point between pre and post training. No defaults.
compare_binary_outcomes <- function(arms.data.list, pre.post.break.points) {
    ## Check arguments
    assertthat::assert_that(is.list(arms.data.list) & all(sapply(arms.data.list, is.data.frame)))
    assertthat::assert_that(is.list(pre.post.break.points))
    ## Split arms data based on pre-post break point and return only post
    arms.data.list <- lapply(arms.data.list, function(arm.data) {
        centre.ids <- as.character(unique(arm.data$id__reg_hospital_id))
        baseline <- rep(FALSE, nrow(arm.data))
        for (centre.id in centre.ids) {
            baseline[arm.data$id__reg_hospital_id == centre.id & arm.data$incident__date_of_arrival < pre.post.break.points[[centre.id]][1]] <- TRUE
        }
        arm.data <- split(arm.data, baseline)
        return (arm.data$`FALSE`) # Revise this whn estimating change from baseline
    })
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
