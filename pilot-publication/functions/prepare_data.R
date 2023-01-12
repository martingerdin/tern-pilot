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
                                   condition = ~ .x %in% c("999", "unknown")) %>%
        naniar::replace_with_na_if(.predicate = is.numeric,
                                   condition = ~ .x == 999)
    ## Deal with edge cases
    variable <- prepared.data$complications__failure_of_conservative_management
    variable[variable == "0" | variable == "NO" | variable == "no"] <- "No"
    variable[variable == "Yes - Surgery on the 3rd day"] <- "Yes"
    variable <- as.factor(variable)
    prepared.data$complications__failure_of_conservative_management <- variable
    ## Generate AIS codes
    icd10.data <- prepared.data[, c("interventions__injury_first_surg_icd10",
                                    "interventions__injury_xray_icd10",
                                    "interventions__injury_external_1_icd10",
                                    "interventions__injury_first_ct_icd10",
                                    "interventions__injury_second_ct_icd10")]
    icd10.data <- icd10.data %>% naniar::replace_with_na_all(condition = ~.x %in% c("0", "NAD"))
    split.icd10.data <- do.call(cbind, lapply(icd10.data, function(column) {
        split.data <- strsplit(column, ",")
        max.length <- max(lengths(split.data))
        split.data <- as.data.frame(do.call(rbind, lapply(split.data, function(element) {
            c(trimws(element), rep(NA, max.length - length(element)))
        })))
        split.data[] <- lapply(split.data, function(split.column) {
            split.column <- gsub("(^[0-9])", "S\\1", split.column)
            ## split.column <- gsub("(^[LETTERS])", "\\1\\.", split.column)
            return (split.column)
        })
        return (split.data)
    }))
    colnames(split.icd10.data) <- paste0("dx", 1:ncol(split.icd10.data))
    iss.data <- icdpicr::cat_trauma(split.icd10.data,
                                    dx_pre = "dx",
                                    icd10 = "base",
                                    i10_iss_method = "roc_max_NIS")[, c("mxaisbr_General",
                                                                        "mxaisbr_HeadNeck",
                                                                        "mxaisbr_Face",
                                                                        "mxaisbr_Extremities", 
                                                                        "mxaisbr_Chest",
                                                                        "mxaisbr_Abdomen",
                                                                        "maxais",
                                                                        "riss",
                                                                        "niss")]
    prepared.data <- cbind(prepared.data, iss.data)
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
        relabelled.data <- factor(variable.data, levels, labels)
        if (any(name == names(binary_outcomes()))) {
            logical.data <- relabelled.data == binary_outcomes()[[name]]$positive.class
            relabelled.data <- factor(logical.data, levels = c(TRUE, FALSE), labels = c("Yes", "No"))
        }
    }
    if (!is.null(type))
        labelled::var_label(relabelled.data) <- variable.label
    return (relabelled.data)
}





