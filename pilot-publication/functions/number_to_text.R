number_to_text <- function(number, first = FALSE) {
    if (number < 13 || first) {
        number <- english::english(number)
    }
    if (is.character(number)) {
        number <- stringr::str_to_sentence(number)
    }
    return(number)
}