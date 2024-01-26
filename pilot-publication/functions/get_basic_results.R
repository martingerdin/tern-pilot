#' Get basic results
#'
#' Function to calculate basic results.
#' @param data A data.frame with the trial data. No default.
#' @export
get_basic_results <- function(data) {
    results <- list()
    arrival.dates <- data %>%
        pull(incident__date_of_arrival) %>%
        as.Date()
    results$start.date <- arrival.dates %>%
        min() %>%
        format_date()
    results$end.date <- arrival.dates %>%
        max() %>%
        format_date()
    n.no.consent <- list(
        "11542" = 40,
        "44805" = 10,
        "55356" = 43,
        "78344" = 3,
        "95846" = 9,
        "88456" = 0, # To be updated
        "10263" = 2
    )
    results$p.consent <- round(nrow(data) / (nrow(data) + sum(unlist(n.no.consent))) * 100)
    results$icc <- estimate_icc("outcomes__discharge_alive", "id__reg_hospital_id", data)
    results$n.patients <- nrow(data)
    results$n.atls.residents <- 4 + 2 # The total number of residents trained in ATLS, per ATLS centre
    results$n.ptc.residents <- 9 + 6 # The total number of residents trained in PTC, per centre
    results$n.residents <- with(results, n.atls.residents + n.ptc.residents)
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
    return(results)
}
