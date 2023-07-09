## Load packages
library(dotenv)
library(dplyr)
library(tibble)
library(lubridate)
library(gtsummary)
library(naniar)
library(icdpicr)

## Source functions
noacsr::source_all_functions()

## Import data
data <- readr::read_csv(Sys.getenv("DATA_DIR"))

## Import codebook
codebook.arguments <- lapply(c("URL", "UID", "USERNAME", "PASSWORD"),
                             function(x) Sys.getenv(paste0("KOBO_", x)))
codebook <- do.call(noacsr::kobo_get_project_codebook, codebook.arguments)

## Prepare data
data <- prepare_data(data, codebook)

## Define basic results
results <- get_basic_results(data)

## Create table of sample characteristics
table.variables <- c("patinfo__pt_age", "patinfo__pt_gender",
                     "incident__dominating_injury_type",
                     "patvitals__ed_rr", "patvitals__ed_sat",
                     "patvitals__ed_hr", "patvitals__ed_sbp",
                     "riss", "niss", "outcomes__alive_after_30_days",
                     "arm")
table.data <- data[, table.variables]
sample.characteristics.table <- create_descriptive_table(table.data,
                                                         strata = "arm",
                                                         caption = "Patient sample characteristics",
                                                         include.overall = TRUE)

## Create table comparing secondary outcomes
secondary.outcomes <- c(names(binary_outcomes()),
                        categorical_outcomes(),
                        quantitative_outcomes())
secondary.outcomes.data <- data[, c(secondary.outcomes, "arm", "post.training")]

## This creates a table with summary data for all outcomes
outcomes.data <- secondary.outcomes.data
outcomes.table.overall <- outcomes.data %>%
    select(-post.training) %>%
    tbl_summary(by = "arm",
                type = all_dichotomous() ~ "categorical",
                statistic = list(
                    all_continuous() ~ "{median}",
                    all_categorical() ~ "{p}"
                ),
                missing_text = "Missing") %>%
    as_tibble() 

## Should be possible to create exactly the same table pre post and
## then calculate the difference between the two. Then wrap all in a
## bootstrap.

secondary.outcomes.table.combined <- create_descriptive_table(secondary.outcomes.data[, -3],
                             p                                 strata = "arm",
                                                              caption = "Secondary outcomes by trial arm")
secondary.outcomes.table.post <- create_descriptive_table(secondary.outcomes.data %>% filter(post.training) %>% select(-post.training),
                                                              strata = "arm",
                                                              caption = "Secondary outcomes by trial arm")


## Calculate outcome results
n.m30d <- with(data, sum(outcomes__alive_after_30_days == "Yes", na.rm = TRUE))
p.m30d <- round(n.m30d/nrow(data) * 100)
arms.n.m30d.list <- lapply(setNames(arms.data.list, nm = paste0("m30d.", names(arms.data.list))),
                           function(arms.data)
                               with(arms.data,
                                    sum(outcomes__alive_after_30_days == "Yes", na.rm = TRUE)))
arms.p.m30d.list <- mapply(arms.n.m30d.list, arms.data.list,
                           FUN = function(count, arms.data) list(round(count/nrow(arms.data) * 100)))
names(arms.n.m30d.list) <- paste0("n.", names(arms.n.m30d.list))
names(arms.p.m30d.list) <- paste0("p.", names(arms.p.m30d.list))
attach(arms.n.m30d.list)
attach(arms.p.m30d.list)
arr.atls.ptc <- p.m30d.atls - p.m30d.ptc
arr.atls.control <- p.m30d.atls - p.m30d.control
arr.ptc.control <- p.m30d.ptc - p.m30d.control
rr.atls.ptc <- round(p.m30d.atls/p.m30d.ptc, 2)
rr.atls.control <- round(p.m30d.atls/p.m30d.control, 2)
rr.ptc.control <- round(p.m30d.ptc/p.m30d.control, 2)
p.missing.in.hospital.mortality <- round(sum(is.na(data$outcomes__discharge_alive))/n.patients * 100)





## Estimate composite outcome
in.hospital.mortality <- data$outcomes__discharge_alive == "Yes"
data$in.hospital.mortality <- in.hospital.mortality
icc.in.hospital.mortality <- estimate_icc("in.hospital.mortality", "id__reg_hospital_id", data)
labelled::var_label(data$in.hospital.mortality) <- "In-hospital mortality"
confined.to.bed <- data$outcomes__eq5dm == "I am confined to bed"
data$confined.to.bed <- confined.to.bed
labelled::var_label(data$confined.to.bed) <- "Confined to bed"
extreme.pain.discomfort <- data$outcomes__eq5dpd == "I have extreme pain or discomfort"
data$extreme.pain.discomfort <- extreme.pain.discomfort
labelled::var_label(data$extreme.pain.discomfort) <- "Extreme pain or discomfort"
unable.bath.dress <- data$outcomes__eq5dsc == "I am unable to bathe or dress myself"
data$unable.bath.dress <- unable.bath.dress
labelled::var_label(data$unable.bath.dress) <- "Unable to bathe or dress oneself"
unable.usual.activities <- data$outcomes__eq5dua == "I am unable to perform my usual activities"
data$unable.usual.activities <- unable.usual.activities
labelled::var_label(data$unable.usual.activities) <- "Unable to perform usual activities"
composite.outcome <- in.hospital.mortality |
    confined.to.bed |
    extreme.pain.discomfort |
    unable.bath.dress |
    unable.usual.activities
data$composite.outcome <- composite.outcome
labelled::var_label(data$composite.outcome) <- "Composite endpoint"
n.composite.outcome <- sum(composite.outcome, na.rm = TRUE)
p.composite.outcome <- round(n.composite.outcome/nrow(data) * 100)
icc.composite.outcome <- estimate_icc("composite.outcome", "id__reg_hospital_id", data)

## Resident outcomes
median.confidence <- median(as.numeric(data$resident__res_comfort), na.rm = TRUE)
iqr.confidence <- quantile(as.numeric(data$resident__res_comfort), probs = c(0.25, 0.75), na.rm = TRUE) %>%
    paste0(collapse = "-")

## Feasibility outcomes

## List of potentially eligible patient participants per site, replace
## IDs with actual IDs once known and replace figures with actual
## figures once known
n.potentially.eligible = list(n.potentially.eligible.centre.1 = 999,
                              n.potentially.eligible.centre.2 = 999,
                              n.potentially.eligible.centre.3 = 999,
                              n.potentially.eligible.centre.4 = 999,
                              n.potentially.eligible.centre.5 = 999,
                              n.potentially.eligible.centre.6 = 999,
                              n.potentially.eligible.centre.7 = 999)
recruitment.rate.patients <- round(n.patients/do.call(sum, n.potentially.eligible) * 100)
n.eligible.residents <- 21
recruitment.rate.residents <- round(n.residents/n.eligible.residents * 100)
n.lost.to.follow.up <- sum(is.na(data$outcomes__alive_after_30_days))
rate.lost.to.follow.up <- round(n.lost.to.follow.up/n.patients * 100)
n.atls.residents.passed.first.attempt <- 2 + 2 # The number of residents who had passed ATLS after the first attempt, per centre
n.atls.residents.passed.second.attempt <- 4 + 2 # The number of residents who had passed ATLS after the second attempt, per centre
rate.pass <- round((n.atls.residents.passed.second.attempt + n.ptc.residents)/n.residents * 100)
rates.missing.data <- unlist(lapply(data, function(column) round(sum(is.na(column)/length(column) * 100))))
min.missing.data <- min(rates.missing.data)
max.missing.data <- max(rates.missing.data)

## Calculate varying cluster sizes
cluster.sizes <- sapply(centre.ids, function(centre.id) nrow(data[data$id__reg_hospital_id == centre.id, ]))
sd.cluster.size <- sd(cluster.sizes)
mean.cluster.size <- mean(cluster.sizes)
ratio.sd.mean.cluster.size <- sd.cluster.size/mean.cluster.size
