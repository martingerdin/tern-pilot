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
    prepared.data$patinfo__pt_age <- as.numeric(prepared.data$patinfo__pt_age)
    prepared.data$id__reg_hospital_id <- as.factor(prepared.data$id__reg_hospital_id)

    ## Replace with missing
    prepared.data <- prepared.data %>%
        naniar::replace_with_na_if(
            .predicate = is.character,
            condition = ~ .x %in% c("999", "unknown")
        ) %>%
        naniar::replace_with_na_if(
            .predicate = is.numeric,
            condition = ~ .x == 999
        )
    ## Deal with edge cases
    variable <- prepared.data$complications__failure_of_conservative_management
    variable[variable == "0" | variable == "NO" | variable == "no"] <- "No"
    variable[variable == "Yes - Surgery on the 3rd day"] <- "Yes"
    variable[variable == "YES"] <- "Yes"
    variable <- as.factor(variable)
    prepared.data$complications__failure_of_conservative_management <- variable
    variable <- prepared.data$complications__number_of_hospitalizations_for_this_injury
    variable[variable == 995] <- NA ## Entered as 995 instead of 999 by mistake
    prepared.data$complications__number_of_hospitalizations_for_this_injury <- variable

    ## Generate AIS codes
    icd10.data <- prepared.data[, c(
        "interventions__injury_first_surg_icd10",
        "interventions__injury_xray_icd10",
        "interventions__injury_external_1_icd10",
        "interventions__injury_first_ct_icd10",
        "interventions__injury_second_ct_icd10"
    )]
    icd10.data <- icd10.data %>% naniar::replace_with_na_all(condition = ~ .x %in% c("0", "NAD"))
    split.icd10.data <- do.call(cbind, lapply(icd10.data, function(column) {
        split.data <- strsplit(column, ",")
        max.length <- max(lengths(split.data))
        split.data <- as.data.frame(do.call(rbind, lapply(split.data, function(element) {
            c(trimws(element), rep(NA, max.length - length(element)))
        })))
        split.data[] <- lapply(split.data, function(split.column) {
            split.column <- gsub("(^[0-9])", "S\\1", split.column)
            ## split.column <- gsub("(^[LETTERS])", "\\1\\.", split.column)
            return(split.column)
        })
        return(split.data)
    }))
    colnames(split.icd10.data) <- paste0("dx", 1:ncol(split.icd10.data))
    iss.data <- icdpicr::cat_trauma(split.icd10.data,
        dx_pre = "dx",
        icd10 = "base",
        i10_iss_method = "roc_max_NIS"
    )[, c(
        "mxaisbr_General",
        "mxaisbr_HeadNeck",
        "mxaisbr_Face",
        "mxaisbr_Extremities",
        "mxaisbr_Chest",
        "mxaisbr_Abdomen",
        "maxais",
        "riss",
        "niss"
    )]
    prepared.data <- cbind(prepared.data, iss.data)

    ## Classify mechanism of injury (incident__moi and incident__moi_001) as factors
    prepared.data$incident__moi <- as.factor(prepared.data$incident__moi)
    prepared.data$incident__moi_001 <- as.factor(prepared.data$incident__moi_001)

    ## Define the post intervention period

    ## The pre-post breakt points are the dates to use when comparing
    ## before training to after training. For intervention centres these
    ## are the dates when the training happened. For standard care centres
    ## these are one month after data collection started.
    arrival.dates <- data %>%
        select(incident__date_of_arrival, id__reg_hospital_id) %>%
        arrange(incident__date_of_arrival)
    centre.start.dates <- arrival.dates %>%
        group_by(id__reg_hospital_id) %>%
        summarise(start_date = format(min(incident__date_of_arrival), "%Y-%m-%d")) %>%
        deframe()
    pre.post.break.points <- list(
        "11542" = ymd(centre.start.dates["11542"]) + months(1), # standard care
        "44805" = c("2022-05-30", "2022-06-20"), # atls, two dates because two students did not pass the first time
        "55356" = "2022-09-02", # ptc
        "78344" = "2022-06-03", # atls
        "95846" = "2022-09-01", # ptc
        "88456" = ymd(centre.start.dates["88456"]) + months(1), # standard care
        "10263" = ymd(centre.start.dates["10263"]) + months(1)
    ) # standard care
    pre.post.break.points <- lapply(pre.post.break.points, ymd)

    prepared.data.with.post.indicator <- do.call(rbind, lapply(split(prepared.data, prepared.data$id__reg_hospital_id), function(centre.data) {
        centre.id <- as.character(unique(centre.data$id__reg_hospital_id))
        centre.data$post.training <- centre.data$incident__date_of_arrival > pre.post.break.points[[centre.id]][1]
        return(centre.data)
    })) %>% labelled::copy_labels_from(prepared.data)

    ## This is the data that should be used as the basis for the synthetic data
    prepared.data <- prepared.data.with.post.indicator

    ## Add variable indicating the trial arm patients were enrolled in
    atls.centres <- c("44805", "78344")
    ptc.centres <- c("55356", "95846")
    control.centres <- c("11542", "88456", "10263")
    prepared.data$arm <- NA
    prepared.data[as.character(prepared.data$id__reg_hospital_id) %in% atls.centres, ]$arm <- "ATLS"
    prepared.data[as.character(prepared.data$id__reg_hospital_id) %in% ptc.centres, ]$arm <- "PTC"
    prepared.data[as.character(prepared.data$id__reg_hospital_id) %in% control.centres, ]$arm <- "Standard care"
    prepared.data$arm <- factor(prepared.data$arm, levels = c("Standard care", "ATLS", "PTC"))

    ## Label variables
    prepared.data[] <- lapply(names(prepared.data), function(column.name) {
        column.data <- prepared.data %>%
            pull(.data[[column.name]]) %>%
            label_variable(name = column.name, codebook = codebook)
        return(column.data)
    })

    ## Add variables indicating the different TQIP cohorts
    prepared.data <- add_tqip_cohorts(prepared.data)

    # Drop unused levels
    prepared.data <- droplevels(prepared.data)

    ## Add additional labels
    labelled::var_label(prepared.data$riss) <- "Injury Severity Score"
    labelled::var_label(prepared.data$niss) <- "New Injury Severity Score"
    labelled::var_label(prepared.data$patinfo__pt_age) <- "Age, years"
    labelled::var_label(prepared.data$patinfo__pt_gender) <- "Sex"
    labelled::var_label(prepared.data$incident__dominating_injury_type) <- "Dominating injury type"
    labelled::var_label(prepared.data$patvitals__ed_rr) <- "Respiratory rate, breaths per minute"
    labelled::var_label(prepared.data$patvitals__ed_sat) <- "Oxygen saturation, %"
    labelled::var_label(prepared.data$patvitals__ed_hr) <- "Heart rate, beats per minute"
    labelled::var_label(prepared.data$patvitals__ed_sbp) <- "Systolic blood pressure, mmHg"
    labelled::var_label(prepared.data$patvitals__ed_gcs) <- "Glasgow Coma Scale"
    labelled::var_label(prepared.data$incident__moi) <- "Mechanism of injury"
    labelled::var_label(prepared.data$blunt.multisystem.trauma) <- "Blunt multisystem trauma"
    labelled::var_label(prepared.data$penetrating.trauma) <- "Penetrating trauma"
    labelled::var_label(prepared.data$shock) <- "Shock"
    labelled::var_label(prepared.data$severe.tbi) <- "Severe traumatic brain injury"
    labelled::var_label(prepared.data$elderly) <- "Elderly"
    labelled::var_label(prepared.data$male) <- "Male"
    labelled::var_label(prepared.data$female) <- "Female"

    ## Return prepared data
    return(prepared.data)
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
    if (!is.null(type)) {
        labelled::var_label(relabelled.data) <- variable.label
    }

    return(relabelled.data)
}
