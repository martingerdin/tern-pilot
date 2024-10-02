## Run from command line with R CMD BATCH main.R run.log &

## Source functions
noacsr::source_all_functions()

## Load packages
load_packages()

## Import data
data <- readr::read_csv(Sys.getenv("DATA_DIR"))

## Import codebook
codebook.arguments <- lapply(
    c("URL", "UID", "USERNAME", "PASSWORD"),
    function(x) Sys.getenv(paste0("KOBO_", x))
)
codebook <- do.call(noacsr::kobo_get_project_codebook, codebook.arguments)

## Prepare data
data <- prepare_data(data, codebook)

## Define basic results
results <- get_basic_results(data)

## Create CONSORT diagram
create_consort_diagram(results)

## Create table of sample characteristics
table.variables <- c(
    "patinfo__pt_age", "elderly",
    "patinfo__pt_gender",
    "incident__dominating_injury_type",
    "blunt.multisystem.trauma", "severe.tbi",
    "shock", "patvitals__ed_rr",
    "patvitals__ed_sat",
    "patvitals__ed_hr", "patvitals__ed_sbp",
    "patvitals__ed_gcs", "riss",
    "outcomes__discharge_alive",
    "outcomes__alive_after_30_days",
    "arm"
)
table.data <- data %>%
    select(all_of(table.variables)) %>%
    select(-arm)
overall.sample.characteristics.table <- create_descriptive_table(table.data, show.all.levels = FALSE)

## Create table of sample characteristics stratified by arm
stratified.table.data <- data %>%
    select(all_of(table.variables)) %>%
    select(-outcomes__discharge_alive, -outcomes__alive_after_30_days)
sample.characteristics.table <- create_descriptive_table(stratified.table.data,
    strata = "arm", show.all.levels = FALSE, include.overall = TRUE
) %>%
    gtsummary::modify_caption("Patient sample characteristics") %>%
    gtsummary::modify_table_styling(
        column = label,
        footnote_abbrev = "ATLS = Advanced Trauma Life Support; PTC = Prehospital Trauma Care"
    ) %>%
    add_stat_label(
        label = list(
            all_continuous() ~ "median (IQR)",
            all_categorical() ~ "n (%)"
        )
    )

## Create table of sample characteristics before training
pre.training.table.data <- data %>%
    filter(!post.training) %>%
    select(table.variables)
pre.training.characteristics.table <- create_descriptive_table(
    pre.training.table.data,
    strata = "arm",
    show.all.levels = FALSE,
    include.overall = TRUE
)

## Create table of sample characteristics after training
post.training.table.data <- data %>%
    filter(post.training) %>%
    select(table.variables)
post.training.characteristics.table <- create_descriptive_table(
    post.training.table.data,
    strata = "arm",
    show.all.levels = FALSE,
    include.overall = TRUE
)

## Combine tables
combined.sample.characteristics.table <- gtsummary::tbl_merge(
    tbls = list(
        pre.training.characteristics.table,
        post.training.characteristics.table,
        overall.sample.characteristics.table
    ),
    tab_spanner = c("Before training", "After training", "Overall")
) %>%
    gtsummary::modify_caption("Patient sample characteristics")

## Create table comparing directly observed with retrospective data
observed.vs.extracted.comparison.table <- create_observed_vs_extracted_comparison_table(data)

## Bootstrap outcome results
use.saved <- TRUE
if (use.saved) {
    outcome.results <- readRDS("outcome-results.Rds")
} else {
    unlink("error.log")
    unlink("out", recursive = TRUE)
    dir.create("out", showWarnings = FALSE)
    n.boot.samples <- 1000
    bootstrapped.outcome.results <- boot(data, try_estimate_outcome_results, R = n.boot.samples)
    saveRDS(bootstrapped.outcome.results, file = file.path("out", "bootstrapped-outcome-results.Rds"))

    ## Calculate confidence intervals
    bootstrapped.outcome.results.ci <- calculate_bootstrap_cis(bootstrapped.outcome.results)
    saveRDS(bootstrapped.outcome.results.ci, file = file.path("out", "bootstrapped-outcome-results-ci.Rds"))

    ## Combine point estimates with confidence intervals
    outcome.results <- list(
        point.estimates = bootstrapped.outcome.results$t0,
        confidence.intervals = bootstrapped.outcome.results.ci
    )
    saveRDS(outcome.results, file = file.path("outcome-results.Rds"))
}

## Create tables with outcome results
outcome.results.tables <- create_outcome_results_tables(outcome.results)

# Extract results from tables
females <- inline_text(overall.sample.characteristics.table,
    variable = patinfo__pt_gender,
    level = "Female"
)
median.age <- inline_text(overall.sample.characteristics.table,
    variable = patinfo__pt_age
)
median.iss <- inline_text(overall.sample.characteristics.table,
    variable = riss
)
m30d <- inline_text(overall.sample.characteristics.table,
    variable = outcomes__alive_after_30_days
)

## Crude 30 day mortality
m30d.standard.care <- inline_text(post.training.characteristics.table,
    variable = outcomes__alive_after_30_days,
    column = "Standard care"
)
m30d.standard.care.n <- inline_text(post.training.characteristics.table,
    variable = outcomes__alive_after_30_days,
    column = "Standard care",
    pattern = "{n}"
)
m30d.standard.care.p <- inline_text(post.training.characteristics.table,
    variable = outcomes__alive_after_30_days,
    column = "Standard care",
    pattern = "({p})%"
)
m30d.atls <- inline_text(post.training.characteristics.table,
    variable = outcomes__alive_after_30_days,
    column = "ATLS"
)
m30d.atls.n <- inline_text(post.training.characteristics.table,
    variable = outcomes__alive_after_30_days,
    column = "ATLS",
    pattern = "{n}"
)
m30d.atls.p <- inline_text(post.training.characteristics.table,
    variable = outcomes__alive_after_30_days,
    column = "ATLS",
    pattern = "({p})%"
)
m30d.ptc <- inline_text(post.training.characteristics.table,
    variable = outcomes__alive_after_30_days,
    column = "PTC"
)
m30d.ptc.n <- inline_text(post.training.characteristics.table,
    variable = outcomes__alive_after_30_days,
    column = "PTC",
    pattern = "{n}"
)
m30d.ptc.p <- inline_text(post.training.characteristics.table,
    variable = outcomes__alive_after_30_days,
    column = "PTC",
    pattern = "({p})%"
)

# Crude in-hospital mortality
inhosp.standard.care <- inline_text(post.training.characteristics.table,
    variable = outcomes__discharge_alive,
    column = "Standard care"
)
inhosp.standard.care.n <- inline_text(post.training.characteristics.table,
    variable = outcomes__discharge_alive,
    column = "Standard care",
    pattern = "{n}"
)
inhosp.standard.care.p <- inline_text(post.training.characteristics.table,
    variable = outcomes__discharge_alive,
    column = "Standard care",
    pattern = "({p})%"
)
inhosp.atls <- inline_text(post.training.characteristics.table,
    variable = outcomes__discharge_alive,
    column = "ATLS"
)
inhosp.atls.n <- inline_text(post.training.characteristics.table,
    variable = outcomes__discharge_alive,
    column = "ATLS",
    pattern = "{n}"
)
inhosp.atls.p <- inline_text(post.training.characteristics.table,
    variable = outcomes__discharge_alive,
    column = "ATLS",
    pattern = "({p})%"
)
inhosp.ptc <- inline_text(post.training.characteristics.table,
    variable = outcomes__discharge_alive,
    column = "PTC"
)
inhosp.ptc.n <- inline_text(post.training.characteristics.table,
    variable = outcomes__discharge_alive,
    column = "PTC",
    pattern = "{n}"
)
inhosp.ptc.p <- inline_text(post.training.characteristics.table,
    variable = outcomes__discharge_alive,
    column = "PTC",
    pattern = "({p})%"
)

# Create tables comparing outcomes
outcome.table.with.differences <- create_outcome_table_with_differences(data)

## Extract tables for absolute and relative differences
standard.care.vs.atls.table <- outcome.results.tables$ci.level.0.95[["Absolute and relative differences in outcomes after training, comparing standard care with ATLS"]]

standard.care.vs.ptc.table <- outcome.results.tables$ci.level.0.95[["Absolute and relative differences in outcomes after training, comparing standard care with PTC"]]

atls.vs.ptc.table <- outcome.results.tables$ci.level.0.95[["Absolute and relative differences in outcomes after training, comparing ATLS with PTC"]]

## Absolute differences
arr.standard.care.atls <- inline_outcome_text(
    standard.care.vs.atls.table,
    outcome = "30 day mortality",
    column = "Absolute difference",
    level = "Yes"
)

arr.standard.care.ptc <- inline_outcome_text(
    standard.care.vs.ptc.table,
    outcome = "30 day mortality",
    column = "Absolute difference",
    level = "Yes"
)

arr.atls.ptc <- inline_outcome_text(
    atls.vs.ptc.table,
    outcome = "30 day mortality",
    column = "Absolute difference",
    level = "Yes"
)

## Relative differences
rr.standard.care.atls <- inline_outcome_text(
    standard.care.vs.atls.table,
    outcome = "30 day mortality",
    column = "Relative difference",
    level = "Yes"
)

rr.standard.care.ptc <- inline_outcome_text(
    standard.care.vs.ptc.table,
    outcome = "30 day mortality",
    column = "Relative difference",
    level = "Yes"
)

rr.atls.ptc <- inline_outcome_text(
    atls.vs.ptc.table,
    outcome = "30 day mortality",
    column = "Relative difference",
    level = "Yes"
)

## Extract tables for change from baseline in 30 day mortality
absolute.change.from.baseline.table <- outcome.results.tables$ci.level.0.95[["Absolute change from baseline for all outcomes, comparing the period after training with the period before training, by treatment arms"]]
relative.change.from.baseline.table <- outcome.results.tables$ci.level.0.95[["Relative change from baseline for all outcomes, comparing the period after training with the period before training, by treatment arms"]]

## Absolute change from baseline
abs.change.standard.care <- inline_outcome_text(
    absolute.change.from.baseline.table,
    outcome = "30 day mortality",
    column = "Standard care",
    level = "Yes"
)

abs.change.atls <- inline_outcome_text(
    absolute.change.from.baseline.table,
    outcome = "30 day mortality",
    column = "ATLS",
    level = "Yes"
)

abs.change.ptc <- inline_outcome_text(
    absolute.change.from.baseline.table,
    outcome = "30 day mortality",
    column = "PTC",
    level = "Yes"
)

## Relative change from baseline
rel.change.standard.care <- inline_outcome_text(
    relative.change.from.baseline.table,
    outcome = "30 day mortality",
    column = "Standard care",
    level = "Yes"
)

rel.change.atls <- inline_outcome_text(
    relative.change.from.baseline.table,
    outcome = "30 day mortality",
    column = "ATLS",
    level = "Yes"
)

rel.change.ptc <- inline_outcome_text(
    relative.change.from.baseline.table,
    outcome = "30 day mortality",
    column = "PTC",
    level = "Yes"
)

## Save outcome results tables to file
# save_tables_to_file(outcome.results.tables)
