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
    ## Modify codebook
    variable.names <- codebook$survey$name
    codebook$survey$type <- setNames(codebook$survey$type, nm = variable.names)
    codebook$survey$label <- setNames(codebook$survey$label, nm = variable.names)
    for (binary.outcome in binary_outcomes()) {
        codebook$survey$label[unlist(strsplit(binary.outcome$name, "__"))[2]] <- binary.outcome$label
    }
    ## Rename variables
    names(data) <- gsub("/", "__", names(data), fixed = TRUE)
    ## Prepare data
    prepared.data <- data
    prepared.data$`patinfo/pt_age` <- as.numeric(prepared.data$patinfo__pt_age)
    ## Replace with missing
    prepared.data <- prepared.data %>%
        naniar::replace_with_na_if(.predicate = is.character,
                                   condition = ~ .x == "999") %>%
        naniar::replace_with_na_if(.predicate = is.numeric,
                                   condition = ~ .x == 999)
    ## Label variables
    prepared.data[] <- lapply(names(prepared.data), function(column.name) {
        column.data <- prepared.data %>%
            pull(.data[[column.name]]) %>%
            label_variable(name = column.name, codebook = codebook)
        return (column.data)
    })
    return (prepared.data)
}

label_variable <- function(variable.data, name, codebook) {
    relabelled.data <- variable.data
    name.components <- strsplit(name, "__", fixed = TRUE)
    variable.name <- name.components[[1]][2]
    variable.label <- codebook$survey$label[variable.name]
    type <- codebook$survey$type[variable.name]
    if (!is.null(type) && grepl("select_one", type)) {
        type.index <- grep(gsub("select_one ", "", type), codebook$choices$list_name)
        levels <- codebook$choices$name[type.index]
        labels <- codebook$choices$label[type.index]
        if ()
        relabelled.data <- factor(variable.data, levels, labels)
    }
    if (!is.null(type))
        labelled::var_label(relabelled.data) <- variable.label
    return (relabelled.data)
}





