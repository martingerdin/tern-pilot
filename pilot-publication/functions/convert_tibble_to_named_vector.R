#' Convert a tibble to a named vector
#'
#' This function takes a tibble as input and converts it to a named vector by concatenating the row and column names.
#'
#' @param tibble A tibble to be converted to a named vector.
#'
#' @return A named vector with concatenated row and column names.
#'
#' @examples
#' tib <- tibble::tribble(
#'     ~name, ~age, ~city,
#'     "Alice", 28, "New York",
#'     "Bob", 32, "Los Angeles"
#' )
#' named_vector <- convert_tibble_to_named_vector(tib)
#' print(named_vector)
#' # Output:
#' # ||column::age||row::index:1::28||row::index:2::32||
#' # ||column::city||row::index:1::New York||row::index:2::Los Angeles||
#' # ||column::name||row::index:1::Alice||row::index:2::Bob||
#'
#' @importFrom tibble tibble
#' @importFrom dplyr nrow
#' @importFrom stats setNames
#' @importFrom purrr map lapply unlist
#'
#' @export
convert_tibble_to_named_vector <- function(tibble) {
    listed.tibble <- as.list(tibble)
    row.names <- paste0("||row::index:", 1:nrow(tibble), "::", listed.tibble[[1]], "||")
    tibble.data.columns <- listed.tibble[-1]
    names(tibble.data.columns) <- paste0("||column::", names(tibble.data.columns), "||")
    unlisted.tibble.data.columns <- lapply(tibble.data.columns, unlist)
    named.tibble.data.columns <- lapply(unlisted.tibble.data.columns, setNames, nm = row.names)
    vector <- unlist(named.tibble.data.columns)
    return (vector)
}
