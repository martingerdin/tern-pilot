#' Save outcome tables to file and create an PDF document
#'
#' This function takes a list of outcome tables and saves them to a file in R Markdown format.
#' It then renders the R Markdown file into an PDF document with a table of contents.
#'
#' @param outcome.tables A list of outcome tables to be saved
#' @param supplementary.material Logical. If TRUE the tables are numbered from S2 to how many
#' tables there are. Defaults to FALSE.
#' @param only.95.ci Logical. If TRUE only the tables with 95% confidence intervals are saved.
#' Set to FALSE if someone requests the full results. Defaults to FALSE.
#'
#' @export
save_tables_to_file <- function(outcome.results.tables, supplementary.material = FALSE, only.95.ci = FALSE) {
    ## Check arguments
    assertthat::assert_that(
        is.list(outcome.results.tables)
    )

    ## Save outcome tables to file
    document.string <- lapply(
        names(outcome.results.tables), function(ci.level.tables.name) {
            ## Extract confidence level as percentage
            ci.level <- stringr::str_remove(ci.level.tables.name, pattern = "ci\\.level\\.") %>%
                as.numeric() %>%
                scales::percent()
            ci.level.tables <- outcome.results.tables[[ci.level.tables.name]]
            if (!only.95.ci || (only.95.ci && ci.level == "95%")) {
                paste0(
                    "\\pagebreak\n\n",
                    "# Results with ", ci.level, " confidence intervals (CI)\n\n",
                    lapply(seq_along(ci.level.tables), function(ci.level.table.index) {
                        ci.level.table.name <- names(ci.level.tables)[ci.level.table.index]
                        ci.level.table <- ci.level.tables[[ci.level.table.name]]
                        paste0(
                            "## ", ifelse(supplementary.material, paste0("Additional Online Table ", ci.level.table.index, ". "), ""), ci.level.table.name, " (", ci.level, " CI)", "\n\n",
                            "Empty cells indicate that there were no observations to calculate the statistic.", "\n\n",
                            "\`\`\`{r, echo = FALSE} \n\n",
                            "outcome.results.tables[[\"", ci.level.tables.name, "\"]]",
                            "[[\"", ci.level.table.name, "\"]]", "\n\n",
                            "\`\`\`\n\n",
                            "\\pagebreak\n\n"
                        )
                    }) %>%
                        paste0(collapse = "\n\n")
                )
            }
        }
    ) %>%
        unlist() %>%
        paste0(collapse = "\n\n")

    ## Write document string to file
    file.name <- ifelse(supplementary.material, "supplementary-outcome-tables.Rmd", "outcome-tables.Rmd")
    fileConn <- file(file.name)
    writeLines(document.string, fileConn)
    close(fileConn)

    ## Create html document
    rmarkdown::render(file.name, output_format = rmarkdown::pdf_document(toc = TRUE))
}
