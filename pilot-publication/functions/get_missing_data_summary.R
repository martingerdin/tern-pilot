get_missing_data_summary <- function(data, variable.names = NULL) {
    # Create default variable names
    if (is.null(variable.names)) {
        variable.names <-
            c(
                "patinfo__pt_age", "patinfo__pt_gender", "patinfo__occupation",
                "incident__referral", "incident__dominating_injury_type", "incident__moi",
                "incident__date_of_injury", "incident__arrival_by", "incident__date_of_arrival",
                "incident__date_of_ed_discharge", "patvitals__ed_rr", "patvitals__ed_sat",
                "patvitals__ed_hr", "patvitals__ed_sbp", "patvitals__ed_dbp",
                "patvitals__ed_gcs", "patvitals__ed_pupils", "patvitals__ed_temperature",
                "patvitals__ed_initial_serum_lactate", "patvitals__ed_intial_be", "who_tc__ctp", "who_tc__popf", "who_tc__lbiv",
                "who_tc__fsd", "who_tc__apf", "who_tc__aib", "who_tc__sid", "who_tc__nvs",
                "who_tc__tvg", "who_tc__ang", "who_tc__abg", "resident__res_num_serious",
                "resident__res_comfort", "cci_index__cci_age", "cci_index__cci_mi",
                "cci_index__cci_chf", "cci_index__cci_pvd", "cci_index__cci_cva",
                "cci_index__cci_dementia", "cci_index__cci_copd", "cci_index__cci_ctd",
                "cci_index__cci_pud", "cci_index__cci_liver", "cci_index__cci_dm",
                "cci_index__cci_hemiplegia", "cci_index__cci_ckd", "cci_index__cci_tumor",
                "cci_index__cci_leukemia", "cci_index__cci_lymphoma", "cci_index__cci_aids",
                "cci_index__co_morbidity_index", "interventions__intubation",
                "interventions__chest_tube", "interventions__vasopressors", "interventions__recived_fluids",
                "interventions__intervention", "interventions__admitted", "interventions__surgery_during_stay",
                "interventions__xray_done", "interventions__fast_done",
                "interventions__ct_done", "outcomes__alive_24h", "outcomes__discharge_alive",
                "outcomes__selfambulatory_at_discharge", "outcomes__patient_satisfaction",
                "outcomes__qold_performed", "outcomes__eq5dm", "outcomes__eq5dsc",
                "outcomes__eq5dua", "outcomes__eq5dpd", "outcomes__eq5dad", "outcomes__eq5dhs",
                "outcomes__alive_after_30_days", "outcomes__qol30d_performed",
                "outcomes__eq5dm30d", "outcomes__eq5dsc30d", "outcomes__eq5dua30d",
                "outcomes__eq5dpd30d", "outcomes__eq5dad30d", "outcomes__eq5dhs30d",
                "outcomes__return_to_work", "outcomes__cost_of_treatment", "complications__pulmonary_complication",
                "complications__complication_septic_shock", "complications__complication_renal_failure",
                "complications__complication_coagulopathy", "complications__number_of_hospitalizations_for_this_injury",
                "complications__need_for_reexploration_or_resurgery", "complications__failure_of_conservative_management", "incident__ref_hospital_code",
                "incident__prior_facility_interventions", "interventions__num_blood_transfusion_within_1h",
                "interventions__num_plasma_within_1h", "interventions__num_platelets_within_1h",
                "interventions__fluids_within_1h", "interventions__num_blood_transfusion_within_24h",
                "interventions__num_plasma_within_24h", "interventions__num_platelets_within_24h",
                "interventions__fluids_within_24h", "interventions__date_of_admission",
                "interventions__date_of_admission_ward", "interventions__admitted_to_icu",
                "interventions__date_of_admission_icu", "interventions__date_of_discharge_icu",
                "interventions__date_of_discharge", "interventions__dialysis_within_30_days",
                "interventions__time_fast", "interventions__time_first_ct",
                "interventions__type_first_ct", "incident__date_of_transport", "interventions__time_of_intubation",
                "interventions__time_mechanical_ventilation_started", "interventions__time_mechanical_ventilated_stopped",
                "interventions__time_of_vasopressors", "interventions__date_of_surgery",
                "interventions__type_of_initial_surgery", "interventions__sbp_at_start_of_surgery",
                "outcomes__time_of_death", "outcomes__physician_likely_cause_death",
                "outcomes__cause_of_death", "interventions__time_of_chest_tube", "incident__internal_referral"
            )
    }

    # Check arguments
    assertthat::assert_that(
        is.data.frame(data),
        is.character(variable.names)
    )
    assertthat::assert_that(
        all(variable.names %in% names(data))
    )

    # Create summary
    missing.data.summary <- data %>%
        select(all_of(variable.names)) %>%
        is.na() %>%
        colSums() %>%
        as.data.frame() %>%
        rename(n.missing = ".") %>%
        mutate(p.missing = round(n.missing / nrow(data) * 100)) %>%
        mutate(variable = rownames(.)) %>%
        select(variable, n.missing, p.missing) %>%
        arrange(desc(p.missing))

    return(missing.data.summary)
}
