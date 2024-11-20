#' Label outcome variables
#'
#' This function labels the outcome variables in the data frame with the labels from the codebook.
#'
#' @param data The data frame containing the outcome variables.
#'
#' @return The data frame with the outcome variables labelled.
label_outcome_variables <- function(data) {
    all.outcomes <- all_outcomes()
    for (outcome in all.outcomes) {
        # Check if the outcome variable has a name and a label before proceeding
        if (!is.null(names(outcome)) && !is.null(data[[outcome$name]]) && !is.null(outcome$label)) {
            # Label the outcome variable
            labelled::var_label(data[[outcome$name]]) <- outcome$label
        }
    }
    return(data)
}
