#' Get basic results
#'
#' Function to calculate basic results.
#' @param data A data.frame with the trial data. No default.
#' @param missing.variables A character vector of variable names to include in the missing data summary. Defaults to NULL, in which case all variables are included.
#' @export
get_basic_results <- function(data, missing.variables = NULL) {
    results <- list()

    # Dates
    arrival.dates <- data %>%
        pull(incident__date_of_arrival) %>%
        as.Date()
    results$start.date <- arrival.dates %>%
        min() %>%
        format_date()
    results$end.date <- arrival.dates %>%
        max() %>%
        format_date()

    # Consent and ICC
    n.no.consent <- list(
        "11542" = 40,
        "44805" = 10,
        "55356" = 43,
        "78344" = 3,
        "95846" = 9,
        "88456" = 3,
        "10263" = 2
    )
    results$p.consent <- round(nrow(data) / (nrow(data) + sum(unlist(n.no.consent))) * 100)
    results$icc.m30d <- estimate_icc("outcomes__alive_after_30_days", "id__reg_hospital_id", data)
    results$icc.inhosp <- estimate_icc("outcomes__discharge_alive", "id__reg_hospital_id", data)
    results$n.patients <- nrow(data)

    # Resident data
    results$n.atls.residents <- 4 + 3 # The total number of residents trained in ATLS, per ATLS centre
    results$n.ptc.residents <- 9 + 6 # The total number of residents trained in PTC, per centre
    results$n.residents <- with(results, n.atls.residents + n.ptc.residents)
    n.passed <- 7 # 2 had to retake the exam, but all passed the second time
    results$pass.rate <- round(n.passed / results$n.atls.residents * 100)
    results$n.eligible.residents <- results$n.residents # Need to update this
    results$recruitment.rate.residents <- round(results$n.residents / results$n.eligible.residents * 100)

    # Resident comfort/confidence
    resident.comfort <- data$resident__res_comfort
    resident.comfort.numeric <- resident.comfort %>%
        stringr::str_extract("[0-9]+") %>%
        as.numeric()
    results$median.confidence <- median(resident.comfort.numeric, na.rm = TRUE)
    results$iqr.confidence <- get_iqr(resident.comfort.numeric)
    results$median.comfort.standard.care.pre <- median(resident.comfort.numeric[data$arm == "Standard care" & !data$post.training], na.rm = TRUE)
    results$iqr.comfort.standard.care.pre <- get_iqr(resident.comfort.numeric[data$arm == "Standard care" & !data$post.training])
    results$median.comfort.standard.care.post <- median(resident.comfort.numeric[data$arm == "Standard care" & data$post.training], na.rm = TRUE)
    results$iqr.comfort.standard.care.post <- get_iqr(resident.comfort.numeric[data$arm == "Standard care" & data$post.training])
    results$median.comfort.atls.pre <- median(resident.comfort.numeric[data$arm == "ATLS" & !data$post.training], na.rm = TRUE)
    results$iqr.comfort.atls.pre <- get_iqr(resident.comfort.numeric[data$arm == "ATLS" & !data$post.training])
    results$median.comfort.atls.post <- median(resident.comfort.numeric[data$arm == "ATLS" & data$post.training], na.rm = TRUE)
    results$iqr.comfort.atls.post <- get_iqr(resident.comfort.numeric[data$arm == "ATLS" & data$post.training])
    results$median.comfort.ptc.pre <- median(resident.comfort.numeric[data$arm == "PTC" & !data$post.training], na.rm = TRUE)
    results$iqr.comfort.ptc.pre <- get_iqr(resident.comfort.numeric[data$arm == "PTC" & !data$post.training])
    results$median.comfort.ptc.post <- median(resident.comfort.numeric[data$arm == "PTC" & data$post.training], na.rm = TRUE)
    results$iqr.comfort.ptc.post <- get_iqr(resident.comfort.numeric[data$arm == "PTC" & data$post.training])

    # Patient outcomes
    results$n.centres <- data %>%
        pull(id__reg_hospital_id) %>%
        unique() %>%
        length()
    results$n.atls <- sum(data$arm == "ATLS")
    results$n.ptc <- sum(data$arm == "PTC")
    results$n.control <- sum(data$arm == "Standard care")
    results$n.females <- with(data, sum(patinfo__pt_gender == "Female"))
    results$p.females <- round(results$n.females / nrow(data) * 100)
    results$median.age <- median(data$patinfo__pt_age, na.rm = TRUE)
    results$iqr.age <- get_iqr(data$patinfo__pt_age)
    results$median.iss <- median(data$riss, na.rm = TRUE)
    results$iqr.iss <- get_iqr(data$riss)
    results$median.niss <- median(data$niss, na.rm = TRUE)
    results$iqr.niss <- get_iqr(data$niss)
    results$n.admitted <- with(data, sum(interventions__admitted == "Yes"))
    results$p.admitted <- round(results$n.admitted / nrow(data) * 100)
    results$n.lost.to.follow.up <- with(data, sum(is.na(outcomes__alive_after_30_days)))
    results$rate.lost.to.follow.up <- round(results$n.lost.to.follow.up / nrow(data) * 100)
    results$p.missing.in.hospital.mortality <- with(data, round(sum(is.na(outcomes__discharge_alive)) / nrow(data) * 100))
    results$n.control.complete.m30d <- with(data[data$arm == "Standard care", ], sum(!is.na(outcomes__alive_after_30_days)))
    results$n.control.complete.hd <- with(data[data$arm == "Standard care", ], sum(!is.na(outcomes__discharge_alive)))
    results$n.atls.complete.m30d <- with(data[data$arm == "ATLS", ], sum(!is.na(outcomes__alive_after_30_days)))
    results$n.atls.complete.hd <- with(data[data$arm == "ATLS", ], sum(!is.na(outcomes__discharge_alive)))
    results$n.ptc.complete.m30d <- with(data[data$arm == "PTC", ], sum(!is.na(outcomes__alive_after_30_days)))
    results$n.ptc.complete.hd <- with(data[data$arm == "PTC", ], sum(!is.na(outcomes__discharge_alive)))
    missing.data.summary <- get_missing_data_summary(data, variable.names = missing.variables)
    results$min.missing.data <- min(missing.data.summary$p.missing)
    results$max.missing.data <- max(missing.data.summary$p.missing)
    return(results)
}
