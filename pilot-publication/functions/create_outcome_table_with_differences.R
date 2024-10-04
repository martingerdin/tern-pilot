create_outcome_table_with_differences <- function(data) {
    # Filter and modify data
    table.data <- data |>
        filter(post.training) |>
        select(outcomes__alive_after_30_days, outcomes__discharge_alive, arm)

    # Convert outcomes to boolean and set variable labels
    table.data <- table.data |>
        mutate(
            outcomes__alive_after_30_days = outcomes__alive_after_30_days == "Yes",
            outcomes__discharge_alive = outcomes__discharge_alive == "Yes"
        )

    labelled::var_label(table.data$outcomes__alive_after_30_days) <- binary_outcomes()$outcomes__alive_after_30_days$label
    labelled::var_label(table.data$outcomes__discharge_alive) <- binary_outcomes()$outcomes__discharge_alive$label

    # Create the outcome table
    outcome.table <- table.data |>
        droplevels() |>
        gtsummary::tbl_summary(by = arm) |>
        modify_header(label = "**Outcome**")

    # Create difference tables
    standard.care.atls.difference <- create_summary_with_difference(table.data, c("Standard care", "ATLS"), "**Standard care vs. ATLS**")
    standard.care.ptc.difference <- create_summary_with_difference(table.data, c("Standard care", "PTC"), "**Standard care vs. PTC**")
    atls.vs.ptc.difference <- create_summary_with_difference(table.data, c("ATLS", "PTC"), "**ATLS vs. PTC**")

    # Merge all tables
    outcome.table.with.differences <- tbl_merge(list(
        outcome.table,
        standard.care.atls.difference,
        standard.care.ptc.difference,
        atls.vs.ptc.difference
    )) |>
        modify_spanning_header(c(stat_1_1, stat_2_1, stat_3_1) ~ NA) |>
        modify_spanning_header(c(estimate_2, estimate_3, estimate_4) ~ "**Differences**") |>
        modify_caption("Mortality after training by the trial arms standard care, Advanced Trauma Life Support (ATLS) and Primary Trauma Care (PTC)") |>
        modify_table_styling(columns = everything(), missing = "")

    # Return outcome table
    return(outcome.table.with.differences)
}
