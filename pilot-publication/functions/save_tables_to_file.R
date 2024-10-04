#' Save outcome tables to file and create an HTML document
#'
#' This function takes a list of outcome tables and saves them to a file in R Markdown format.
#' It then renders the R Markdown file into an HTML document with a table of contents.
#'
#' @param outcome.tables A list of outcome tables to be saved
#'
#' @export
save_tables_to_file <- function(outcome.results.tables) {
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
            paste0(
                "\\pagebreak\n\n",
                "# Results with ", ci.level, " confidence level\n\n",
                lapply(names(ci.level.tables), function(ci.level.table.name) {
                    ci.level.table <- ci.level.tables[[ci.level.table.name]]
                    paste0(
                        "## ", ci.level.table.name, "\n\n",
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
    ) %>%
        unlist() %>%
        paste0(collapse = "\n\n")

    ## Write document string to file
    fileConn <- file("outcome-tables.Rmd")
    writeLines(document.string, fileConn)
    close(fileConn)

    ## Create html document
    rmarkdown::render("outcome-tables.Rmd", output_format = rmarkdown::pdf_document(toc = TRUE))
}
