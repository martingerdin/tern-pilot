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
          binary.outcome$hchange.from.baseline <- TRUE
          return(binary.outcome)
     })
     names(binary.outcomes) <- sapply(binary.outcomes, function(binary.outcome) binary.outcome$name)
     return(binary.outcomes)
}

categorical_outcomes <- function() {
     setNames(nm = c(
          "outcomes__eq5dm",
          "outcomes__eq5dsc",
          "outcomes__eq5dua",
          "outcomes__eq5dpd",
          "outcomes__eq5dad",
          "outcomes__eq5dm30d",
          "outcomes__eq5dsc30d",
          "outcomes__eq5dua30d",
          "outcomes__eq5dpd30d",
          "outcomes__eq5dad30d",
          "outcomes__patient_satisfaction"
     ))
}

## Complete later
quantitative_outcomes <- function() {
     setNames(nm = c(
          "complications__number_of_hospitalizations_for_this_injury",
          "outcomes__eq5dhs",
          "outcomes__eq5dhs30d",
          "outcomes__cost_of_treatment"
     ))
}

## Complete later
time_to_event_outcomes <- function() {
     setNames(nm = c(
          "outcomes__time_of_death",
          "interventions__date_of_surgery"
     ))
}
