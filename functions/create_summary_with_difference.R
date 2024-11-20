# Function to filter data, create summary table, and add differences
create_summary_with_difference <- function(data, arms, header_text) {
    data |>
        filter(arm %in% arms) |>
        droplevels() |>
        gtsummary::tbl_summary(
            by = arm
        ) |>
        add_difference() |>
        modify_column_hide(c(stat_1, stat_2, conf.low, p.value)) |>
        modify_footnote(estimate ~ NA) |>
        modify_header(estimate = header_text)
}
