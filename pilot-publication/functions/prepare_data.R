#' Prepare data
#'
#' This functions prepares the data by changing column types, merging
#' categories, labelling columns and so on.
#' @param data A data.frame. The data to be prepared. No default.
#' @param codebook A list. A list describing the columns of data or
#'     NULL. Defaults to NULL.
prepare_data <- function(data, codebook = NULL) {
    ## Check arguments
    assertthat::assert_that(is.data.frame(data))
    assertthat::assert_that(is.list(codebook) | is.null(codebook))
    ## Prepare data
    prepared.data <- data
    return (prepared.data)
}
