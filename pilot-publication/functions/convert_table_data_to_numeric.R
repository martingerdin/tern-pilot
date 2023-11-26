#' Convert table data to numeric
#'
#' This function takes a table object as input and converts the data in the last three columns of the table to numeric format.
#' It removes any commas from the data before converting it to numeric and returns the modified table.
#'
#' @param table.object The table object to be converted to numeric.
#'
#' @return The modified table object with the last three columns converted to numeric format.

#' @importFrom base gsub as.numeric
#'
#' @export
convert_table_data_to_numeric <- function(table.object) {
    ## This function converts the three last columns of the table to numeric
    ## and returns the table
    table.object[, 2:4] <- lapply(table.object[, 2:4], function(column)
        as.numeric(gsub(",", "", column)))
    return (table.object)
}
