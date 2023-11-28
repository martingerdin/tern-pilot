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
sample.characteristics.table <- create_descriptive_table(table.data,
    strata = "arm",
    caption = "Patient sample characteristics",
    include.overall = TRUE
)

## Bootstrap outcome results
unlink("error.log")
unlink("out", recursive = TRUE)
dir.create("out", showWarnings = FALSE)
n.boot.samples <- 10
bootstrapped.outcome.results <- boot(data, try_estimate_outcome_results, R = n.boot.samples)
saveRDS(bootstrapped.outcome.results, file = file.path("out", "bootstrapped-outcome-results.Rds"))

## Calculate confidence intervals
ci.levels <- c(0.75, 0.85, 0.95)
ci.type <- "basic"
bootstrapped.outcome.results.ci <- lapply(seq_along(bootstrapped.outcome.results$t0), function(index) {
    t0.value <- bootstrapped.outcome.results$t0[index]
    ci <- NA
    if (!is.na(t0.value)) {
        boot.ci.object <- boot.ci(
            boot.out = bootstrapped.outcome.results,
            index = index, type = ci.type, conf = ci.levels
        )
        ci <- boot.ci.object[[ci.type]][, c(1, 4:5)]
        colnames(ci) <- c("level", "lower", "upper")
    }
    return(ci)
})
names(bootstrapped.outcome.results.ci) <- names(bootstrapped.outcome.results$t0)
saveRDS(bootstrapped.outcome.results.ci, file = file.path("out", "bootstrapped-outcome-results-ci.Rds"))
