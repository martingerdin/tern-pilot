#' Estimate outcome results from study data
#'
#' This function takes a data frame as input and estimates outcome
#' results from the study data. It performs various calculations,
#' including creating outcomes tables, calculating absolute and
#' relative differences between pre and post-training outcomes,
#' comparing post-training outcomes between different arms, and
#' comparing change from baseline between arms. The function returns
#' the results as a named vector, to be used for bootstrapping.
#'
#' @param data The data frame containing the study data.
#' @param index The index of the bootstrap sample.
#'
#' @return A named vector containing the estimated outcome results from the study data.
#'
#' @importFrom dplyr select filter
#' @importFrom stats combn
#'
#' @export
estimate_outcome_results <- function(data, index) {
    ## Check arguments
    assertthat::assert_that(is.data.frame(data))
    assertthat::assert_that(is.numeric(index))

    ## Write index to disk
    index.file <- data.frame(i = index)
    if (file.exists("out/index.csv")) {
        index.file <- read.csv("out/index.csv")
        index.file <- index.file %>% add_column(i = index, .name_repair = "unique")
    }
    write.csv(index.file, "out/index.csv", row.names = FALSE)

    ## Create bootstrap sample
    data <- data[index, ]

    ## Create outcomes tables
    outcomes <- c(
        names(binary_outcomes()),
        categorical_outcomes(),
        quantitative_outcomes()
    )
    outcomes.data <- data[, c(outcomes, "arm", "post.training")]
    outcomes.data.list <- list(
        overall = outcomes.data %>% select(-post.training),
        pre.training = outcomes.data %>% filter(!post.training) %>% select(-post.training),
        post.training = outcomes.data %>% filter(post.training) %>% select(-post.training)
    )
    outcomes.tables <- lapply(outcomes.data.list, create_outcomes_table)
    outcomes.row.names <- outcomes.tables$overall[, 1]

    ## Convert all outcome data in tables to numeric
    outcomes.tables <- lapply(outcomes.tables, convert_table_data_to_numeric)

    ## Calculate the absolute difference between the pre and post training outcomes
    ## in the same trial arms using only the three columns with the arms data and not the characteristics column
    outcomes.tables$absolute.difference <- outcomes.tables$post.training[2:4] - outcomes.tables$pre.training[2:4]
    outcomes.tables$absolute.difference <- outcomes.tables$absolute.difference %>%
        as_tibble() %>%
        add_column(outcomes.row.names, .before = 1)

    ## And now calculate the relative difference, still in the same trial arms
    outcomes.tables$relative.difference <- outcomes.tables$post.training[2:4] / outcomes.tables$pre.training[2:4]
    outcomes.tables$relative.difference <- outcomes.tables$relative.difference %>%
        as_tibble() %>%
        add_column(outcomes.row.names, .before = 1)

    ## Now compare post training outcomes between trial arms. First identify all possible combinations of arms
    arm.combinations <- combn(unique(data$arm), 2, simplify = FALSE)
    arm.combination.names <- lapply(arm.combinations, paste, collapse = " vs. ")

    ## Now compare post training outcomes between arms
    post.outcomes.compared.between.arms <- setNames(lapply(arm.combinations,
        compare_outcomes_between_arms,
        table.name = "post.training",
        outcomes.tables = outcomes.tables,
        outcomes.row.names = outcomes.row.names
    ), paste0("Post training outcome ", arm.combination.names))

    ## And finally compare change from baseline between arms
    change.from.baseline.compared.between.arms <- setNames(lapply(arm.combinations,
        compare_outcomes_between_arms,
        table.name = "absolute.difference",
        outcomes.tables = outcomes.tables,
        outcomes.row.names = outcomes.row.names
    ), paste0("Change from baseline ", arm.combination.names))

    ## Compile all outcome comparisons in a single list
    outcome.comparisons.list <- c(
        outcomes.tables,
        post.outcomes.compared.between.arms,
        change.from.baseline.compared.between.arms
    )
    names(outcome.comparisons.list) <- paste0("||table::", names(outcome.comparisons.list), "||")

    ## Convert all tables to named vectors
    outcome.comparisons.vectors <- lapply(outcome.comparisons.list, convert_tibble_to_named_vector)
    outcome.comparisons.vector <- unlist(outcome.comparisons.vectors)

    ## Save results to disk
    timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
    if (identical(index, 1:nrow(data))) {
        timestamp <- paste0(timestamp, "-original")
    }
    saveRDS(outcome.comparisons.vector, paste0("out/outcome-comparisons-vector-", timestamp, ".Rds"))

    ## Return results
    results <- outcome.comparisons.vector
    return(results)
}
