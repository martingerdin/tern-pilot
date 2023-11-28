#' Create outcomes table from data
#'
#' This function takes a data frame as input and creates an outcomes table based on the specified summarization settings.
#' The outcomes table is created using `tbl_summary()` from the `gtsummary` package and includes summary statistics for different arms of the study.
#' The summary statistics are calculated for dichotomous variables as categorical data, and for continuous and categorical variables as median and proportion (p), respectively.
#' Missing values are labeled as "Missing" in the outcomes table.
#'
#' @param data The data frame containing the study data.
#'
#' @return An outcomes table as a tibble containing summary statistics for different arms of the study.
#'
#' @importFrom gtsummary tbl_summary all_dichotomous all_continuous all_categorical
#' @importFrom dplyr as_tibble
#'
#' @export
create_outcomes_table <- function(data) {
    outcomes.table <- data %>%
        tbl_summary(
            by = "arm",
            type = list(
                where(is.numeric) ~ "continuous",
                where(is.factor) ~ "categorical"
            ),
            statistic = list(
                all_continuous() ~ "{median}",
                all_categorical() ~ "{p}"
            ),
            missing = "always",
            missing_text = "Missing"
        ) %>%
        as_tibble()
    return(outcomes.table)
}
