inline_outcome_text <- function(gt.table, outcome, level, column) {
    ## Define borrowed functions
    `%>%` <- magrittr::`%>%`
    pull <- dplyr::pull

    ## Check arguments
    assertthat::assert_that(
        "gt_tbl" %in% class(gt.table),
        is.character(outcome),
        is.character(level),
        is.character(column) | is.numeric(column)
    )

    ## Extract data
    gt.data <- gt.table$`_data`
    outcome.index <- na.omit(gt.table$`_styles`$rownum) # This relies on outcome names being styled as bold
    valid.outcomes <- gt.data$Outcome[outcome.index]
    outcome.pos <- grep(paste0("^", outcome, "$"), valid.outcomes)

    ## Stop if outcome not found
    if (length(outcome.pos) == 0) {
        stop(
            "No outcome found for outcome ",
            outcome,
            ". Choose one from c(",
            paste0(paste0("\"", valid.outcomes, "\""), collapse = ", "), ")."
        )
    }

    ## Extract only the relevant part of the table
    first.row <- outcome.index[outcome.pos] + 1
    if (outcome.pos == rev(outcome.index)[1]) {
        last.row <- nrow(gt.data)
    } else {
        last.row <- outcome.index[outcome.pos + 1] - 1
    }
    outcome.limits <- c(first.row, last.row)
    outcome.subtable <- gt.data[outcome.limits[1]:outcome.limits[2], ]

    ## Stop if column not found
    if (!is.numeric(column)) {
        new.column <- grep(column, names(outcome.subtable))
        if (length(new.column) > 1) {
            stop(
                "More than one column found for column ",
                column,
                ". Choose one from c(",
                paste0(paste0("\"", names(outcome.subtable), "\""), collapse = ", "), ")."
            )
        }
        if (length(new.column) == 0) {
            stop(
                "No column found for column ",
                column,
                ". Choose one from c(",
                paste0(paste0("\"", names(outcome.subtable), "\""), collapse = ", "), ")."
            )
        }
    }

    ## Stop if level not found
    new.level <- grep(level, outcome.subtable$Outcome)
    if (length(new.level) > 1) {
        stop(
            "More than one level found for level ",
            level,
            ". Choose one from c(",
            paste0(paste0("\"", outcome.subtable$Outcome, "\""), collapse = ", "), ")."
        )
    }
    if (length(new.level) == 0) {
        stop(
            "No level found for level ",
            level,
            ". Choose one from c(",
            paste0(paste0("\"", outcome.subtable$Outcome, "\""), collapse = ", "), ")."
        )
    }

    ## Extract inline text
    outcome.text <- outcome.subtable[outcome.subtable$Outcome == level, new.column] %>%
        pull() %>%
        unlist()

    ## Return inline text
    return(outcome.text)
}
