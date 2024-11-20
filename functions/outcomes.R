#' Return vector of all binary outcomes
#' See ../pilot-protocol-publication/outcomes.json for full list of outcomes
binary_outcomes <- function() {
     binary.outcomes <- list(
          list(
               name = "outcomes__alive_after_30_days",
               label = "30 day mortality",
               positive.class = "No"
          ),
          list(
               name = "outcomes__alive_24h",
               label = "24 hour mortality",
               positive.class = "No"
          ),
          list(
               name = "outcomes__discharge_alive",
               label = "In-hospital mortality",
               positive.class = "No"
          ),
          list(
               name = "outcomes__selfambulatory_at_discharge",
               label = "Self-ambulatory at discharge",
               positive.class = "Yes"
          ),
          list(
               name = "outcomes__return_to_work",
               label = "Return to work",
               positive.class = "Yes"
          ),
          list(
               name = "complications__pulmonary_complication",
               label = "Pulmonary complication",
               positive.class = "Yes"
          ),
          list(
               name = "complications__complication_septic_shock",
               label = "Septic complication",
               positive.class = "Yes"
          ),
          list(
               name = "complications__complication_renal_failure",
               label = "Renal failure",
               positive.class = "Yes"
          ),
          list(
               name = "complications__complication_coagulopathy",
               label = "Coagulopathy",
               positive.class = "Yes"
          ),
          list(
               name = "complications__need_for_reexploration_or_resurgery",
               label = "Need for reexploration or resurgery",
               positive.class = "Yes"
          ),
          list(
               name = "complications__failure_of_conservative_management",
               label = "Failure of conservative management",
               positive.class = "Yes"
          )
     )
     binary.outcomes <- lapply(binary.outcomes, function(binary.outcome) {
          binary.outcome$change.from.baseline <- TRUE
          return(binary.outcome)
     })
     names(binary.outcomes) <- sapply(binary.outcomes, function(binary.outcome) binary.outcome$name)
     return(binary.outcomes)
}

categorical_outcomes <- function() {
     categorical.outcomes <- list(
          list(
               name = "outcomes__eq5dm",
               label = "EQ-5D mobility at discharge"),
          list(name = "outcomes__eq5dsc",
               label = "EQ-5D self-care at discharge"),
          list(name = "outcomes__eq5dua",
               label = "EQ-5D usual activities at discharge"),
          list(name = "outcomes__eq5dpd", 
               label = "EQ-5D pain/discomfort at discharge"),
          list(name = "outcomes__eq5dad",
               label = "EQ-5D anxiety/depression at discharge"),
          list(name = "outcomes__eq5dm30d",
               label = "EQ-5D mobility at 30 day follow-up"),
          list(name = "outcomes__eq5dsc30d",
               label = "EQ-5D self-care at 30 day follow-up"),
          list(name = "outcomes__eq5dua30d",
               label = "EQ-5D usual activities at 30 day follow-up"),
          list(name = "outcomes__eq5dpd30d",
               label = "EQ-5D pain/discomfort at 30 day follow-up"),
          list(name = "outcomes__eq5dad30d",
               label = "EQ-5D anxiety/depression at 30 day follow-up"),
          list(name = "outcomes__patient_satisfaction",
               label = "Patient satisfaction")
     )
     categorical.outcomes <- lapply(categorical.outcomes, function(categorical.outcome) {
          categorical.outcome$change.from.baseline <- TRUE
          return(categorical.outcome)
     })
     names(categorical.outcomes) <- sapply(categorical.outcomes, function(categorical.outcome) categorical.outcome$name)
     return(categorical.outcomes)
}

## Complete later
quantitative_outcomes <- function() {
     quantitative.outcomes <- list(
          list(
               name = "complications__number_of_hospitalizations_for_this_injury",
               label = "Number of hospitalizations for this injury"
          ),
          list(
               name = "outcomes__eq5dhs",
               label = "EQ-5D health state at discharge"
          ),
          list(
               name = "outcomes__eq5dhs30d",
               label = "EQ-5D health state at 30 day follow-up"
          ),
          list(
               name = "outcomes__cost_of_treatment",
               label = "Cost of treatment"
          )
     )
     quantitative.outcomes <- lapply(quantitative.outcomes, function(quantitative.outcome) {
          quantitative.outcome$change.from.baseline <- TRUE
          return(quantitative.outcome)
     })
     names(quantitative.outcomes) <- sapply(quantitative.outcomes, function(quantitative.outcome) quantitative.outcome$name)
     return(quantitative.outcomes)
}

## Complete later
time_to_event_outcomes <- function() {
     setNames(nm = c(
          "outcomes__time_of_death",
          "interventions__date_of_surgery"
     ))
}

## Function to return all outcomes
all_outcomes <- function() {
     outcomes <- c(
          binary_outcomes(),
          categorical_outcomes(),
          quantitative_outcomes(),
          time_to_event_outcomes()
     )
     return(outcomes)
}