number_to_text <- function(number, first = FALSE) {
    number <- as.numeric(number)
    if (number < 13 || first) {
        number <- english::english(number) |> as.character()
    }
    if (is.character(number) && first) {
        number <- stringr::str_to_sentence(number) |> as.character()
    }
    return(number)
}
