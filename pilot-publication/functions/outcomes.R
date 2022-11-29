#' Return vector of all binary outcomes
binary_outcomes <- function() {
    c("outcomes__alive_24h",
      "outcomes__discharge_alive",
      "outcomes__selfambulatory_at_discharge",
      "outcomes__alive_after_30_days",
      "outcomes__return_to_work",
      "complications__pulmonary_complication",
      "complications__complication_septic_shock",
      "complications__complication_renal_failure",
      "complications__complication_coagulopathy",      
      "complications__need_for_reexploration_or_resurgery",
      "complications__failure_of_conservative_management")
}

categorical_outcomes <- function() {
    c("outcomes__eq5dm",
      "outcomes__eq5dsc",
      "outcomes__eq5dua",
      "outcomes__eq5dpd", 
      "outcomes__eq5dad",
      "outcomes__eq5dm30d",
      "outcomes__eq5dsc30d", 
      "outcomes__eq5dua30d",
      "outcomes__eq5dpd30d",
      "outcomes__eq5dad30d")
}

quantitative_outcomes <- function() {
    c("complications__number_of_hospitalizations_for_this_injury",
      "outcomes__eq5dhs",
      "outcomes__eq5dhs30d",
      "outcomes__cost_of_treatment")
}
