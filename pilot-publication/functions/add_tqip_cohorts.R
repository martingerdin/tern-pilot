#' Add Trauma Quality Improvement Program (TQIP) Cohorts to Data
#'
#' This function augments a given dataset with additional cohort information based on Trauma Quality Improvement Program (TQIP) criteria from Hornor et at. 2018, http://dx.doi.org/10.1007/s40719-018-0127-1.
#' @param data The original data containing the variables needed to define the TQIP cohorts and to which the TQIP cohorts will be added. No default.
#' @param cohorts An optional list of cohort names and their corresponding labels. Only cohorts included in this list will be added to the data. Defaults to list(blunt.multisystem.trauma = "Blunt multisystem trauma", penetrating.trauma = "Penetrating trauma", severe.tbi = "Severe traumatic brain injury", shock = "Shock", elderly = "Elderly", male = "Male", female = "Female")
#' @details
#' It identifies various trauma-related cohorts such as blunt multisystem trauma, penetrating trauma, severe traumatic brain injury, etc., and appends these as new columns to the data.
#' The original version of this function assumes that the AIS data was created from ICD codes using the package icdpicr.
#' @return Returns a data frame which is a combination of the original data and the newly defined TQIP cohorts. Each TQIP cohort is represented as a boolean column indicating whether a patient belongs to that cohort.
#'
#' @importFrom dplyr select starts_with
#' @importFrom labelled var_label
#' @importFrom assertthat assert_that
#' @export
add_tqip_cohorts <- function(data,
                             cohorts = list(
                                 "blunt.multisystem.trauma" = "Blunt multisystem trauma",
                                 "penetrating.trauma" = "Penetrating trauma",
                                 "severe.tbi" = "Severe traumatic brain injury",
                                 "shock" = "Shock",
                                 "elderly" = "Elderly",
                                 "male" = "Male",
                                 "female" = "Female"
                             )) {
    # Check arguments with assertthat
    assertthat::assert_that(is.data.frame(data))
    assertthat::assert_that(is.list(cohorts))
    assertthat::assert_that(all(sapply(cohorts, is.character)))
    assertthat::assert_that(all(names(cohorts) %in% c(
        "blunt.multisystem.trauma", "penetrating.trauma", "severe.tbi",
        "shock", "elderly", "male", "female"
    )))
    assertthat::assert_that(all(sapply(cohorts, function(cohort) {
        length(cohort) == 1
    })))

    # Define utility functions

    ## Define a function that counts the number of columns in which the AIS severity score is more than 0
    count_unique_regions <- function(ais.region.data) {
        unique.regions <- apply(ais.region.data, 1, function(row) {
            sum(row > 0, na.rm = TRUE)
        })
        return(unique.regions)
    }

    ## Define a function that counts the number of columns in which the AIS severity score is 3 or more
    count_severe_regions <- function(ais.region.data) {
        severe.regions <- apply(ais.region.data, 1, function(row) {
            sum(row >= 3, na.rm = TRUE)
        })
        return(severe.regions)
    }

    # Create list to hold cohort data
    tqip.cohorts <- list()

    # Count unique regions
    unique.regions.count <- data %>%
        select(starts_with("mxaisbr_"), -mxaisbr_General) %>%
        count_unique_regions()

    # Count severe regions
    severe.regions.count <- data %>%
        select(starts_with("mxaisbr_"), -mxaisbr_General) %>%
        count_severe_regions()

    # Define blunt multisystem trauma
    dominating.injury.type <- data$incident__dominating_injury_type
    if ("blunt.multisystem.trauma" %in% names(cohorts)) {
        tqip.cohorts$blunt.multisystem.trauma <- dominating.injury.type == "Blunt" & severe.regions.count >= 2
    }

    # Define penetrating trauma
    if ("penetrating.trauma" %in% names(cohorts)) {
        tqip.cohorts$penetrating.trauma <- dominating.injury.type == "Penetrating"
    }

    # Define shock
    if ("shock" %in% names(cohorts)) {
        tqip.cohorts$shock <- data$patvitals__ed_sbp < 90
    }

    # Define severe traumatic brain injury
    if ("severe.tbi" %in% names(cohorts)) {
        tqip.cohorts$severe.tbi <- data$patvitals__ed_gcs < 9 & severe.regions.count >= 1
    }

    # Define elderly
    if ("elderly" %in% names(cohorts)) {
        tqip.cohorts$elderly <- data$patinfo__pt_age >= 65
    }

    # Define male
    if ("male" %in% names(cohorts)) {
        tqip.cohorts$male <- data$patinfo__pt_gender == "Male"
    }

    # Define female
    if ("female" %in% names(cohorts)) {
        tqip.cohorts$female <- data$patinfo__pt_gender == "Female"
    }

    # Combine cohorts list into a single data frame
    tqip.cohorts <- as.data.frame(do.call(cbind, tqip.cohorts))

    # Merge tqip cohorts into data, making sure that column names are unique
    if (any(names(data) %in% names(tqip.cohorts))) {
        stop("Some of the cohort names are already present in the data")
    }

    # Add tqip cohorts to data
    new.data <- cbind(data, tqip.cohorts)

    # Label cohorts
    for (cohort.name in names(tqip.cohorts)) {
        labelled::var_label(new.data[[cohort.name]]) <- cohorts[[cohort.name]]
    }

    # Return data with tqip cohorts
    return(new.data)
}
