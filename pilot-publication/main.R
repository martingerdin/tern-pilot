## Run from command line with R CMD BATCH main.R run.log &

## Load packages
library(dotenv)
library(dplyr)
library(tibble)
library(lubridate)
library(gtsummary)
library(naniar)
library(icdpicr)
library(boot)

## Source functions
noacsr::source_all_functions()

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

## Create table of sample characteristics
table.variables <- c(
    "patinfo__pt_age", "patinfo__pt_gender",
    "incident__dominating_injury_type",
    "patvitals__ed_rr", "patvitals__ed_sat",
    "patvitals__ed_hr", "patvitals__ed_sbp",
    "riss", "niss", "outcomes__alive_after_30_days",
    "arm"
)
table.data <- data[, table.variables]
overall.sample.characteristics.table <- create_descriptive_table(table.data)

## Create table of sample characteristics before training
pre.training.table.data <- data %>%
    filter(!post.training) %>%
    select(table.variables)
pre.training.characteristics.table <- create_descriptive_table(
    pre.training.table.data,
    strata = "arm",
    include.overall = TRUE
)

## Create table of sample characteristics after training
post.training.table.data <- data %>%
    filter(post.training) %>%
    select(table.variables)
post.training.characteristics.table <- create_descriptive_table(
    post.training.table.data,
    strata = "arm",
    include.overall = TRUE
)

## Combine tables
sample.characteristics.table <- gtsummary::tbl_merge(
    tbls = list(
        pre.training.characteristics.table,
        post.training.characteristics.table,
        overall.sample.characteristics.table
    ),
    tab_spanner = c("Before training", "After training", "Overall")
) %>%
    gtsummary::modify_caption("Patient sample characteristics")

## Save tables to disk
saveRDS(list(
    sample.characteristics = sample.characteristics.table,
    post.training.characteristics = post.training.characteristics.table
), file = file.path("out", "sample-characteristics-tables.Rds"))

## Bootstrap outcome results
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

## Create tables with outcome results
outcome.results.tables <- create_outcome_results_tables(outcome.results)

## Save outcome results tables to file
save_tables_to_file(outcome.results.tables)
