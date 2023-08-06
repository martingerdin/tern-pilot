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

## Create tables comparing outcomes
outcomes <- c(names(binary_outcomes()),
              categorical_outcomes(),
              quantitative_outcomes())
outcomes.data <- data[, c(outcomes, "arm", "post.training")]
outcomes.data.list <- list(
    overall = outcomes.data %>% select(-post.training),
    pre.training = outcomes.data %>% filter(!post.training) %>% select(-post.training),
    post.training = outcomes.data %>% filter(post.training) %>% select(-post.training))
outcomes.tables <- lapply(outcomes.data.list, create_outcomes_table)
outcomes.row.names <- outcomes.tables$overall[, 1]

create_outcomes_table <- function(data) {
    outcomes.table <- data %>%
        tbl_summary(by = "arm",
                    type = all_dichotomous() ~ "categorical",
                    statistic = list(
                        all_continuous() ~ "{median}",
                        all_categorical() ~ "{p}"
                    ),
                    missing_text = "Missing") %>%
        as_tibble()
    return (outcomes.table)
}

## Convert all outcome data in tables to numeric
outcomes.tables <- lapply(outcomes.tables, convert_table_data_to_numeric)

convert_table_data_to_numeric <- function(table.object) {
    ## This function converts the three last columns of the table to numeric
    ## and returns the table
    table.object[, 2:4] <- lapply(table.object[, 2:4], function(column)
        as.numeric(gsub(",", "", column)))
    return (table.object)
}

## Calculate the absolute difference between the pre and post training outcomes in the same trial arms using only the three columns with the arms data and not the characteristics column
outcomes.tables$absolute.difference <- outcomes.tables$post.training[2:4] - outcomes.tables$pre.training[2:4] 
outcomes.tables$absolute.difference <- outcomes.tables$absolute.difference %>%
    as_tibble() %>%
    add_column(outcomes.row.names, .before = 1) 

## And now calculate the relative difference, still in the same trial arms
outcomes.tables$relative.difference <- outcomes.tables$post.training[2:4]/outcomes.tables$pre.training[2:4]
outcomes.tables$relative.difference <- outcomes.tables$relative.difference %>%
    as_tibble() %>%
    add_column(outcomes.row.names, .before = 1)

## Now compare post training outcomes between trial arms. First identify all possible combinations of arms
arm.combinations <- combn(unique(data$arm), 2, simplify = FALSE)
arm.combination.names <- lapply(arm.combinations, paste, collapse = " vs. ")

## Now compare post training outcomes between arms
post.outcomes.compared.between.arms <- setNames(lapply(arm.combinations, compare_outcomes_between_arms, table.name = "post.training", outcomes.tables = outcomes.tables), paste0("Post training outcome ", arm.combination.names))

## And finally compare change from baseline between arms
change.from.baseline.compared.between.arms <- setNames(lapply(arm.combinations, compare_outcomes_between_arms, table.name = "absolute.difference", outcomes.tables = outcomes.tables), paste0("Change from baseline ", arm.combination.names))

compare_outcomes_between_arms <- function(arm.combination, table.name, outcomes.tables) {
    assertthat::assert_that(is.list(outcomes.tables))
    assertthat::assert_that(is.data.frame(outcomes.tables[[table.name]]))
    outcomes.table <- outcomes.tables[[table.name]] %>%
        select(contains(arm.combination[1]), contains(arm.combination[2]))
    ## Calculate the difference between the two arms
    outcomes.comparison <- data.frame(
        absolute.difference = outcomes.table[, 1] - outcomes.table[, 2],
        relative.difference = outcomes.table[, 1]/outcomes.table[, 2]
    )
    colnames(outcomes.comparison) <- c("Absolute difference", "Relative difference")
    ## Convert outcomes comparison to tibble
    outcomes.comparison <- as_tibble(outcomes.comparison) %>%
        add_column(outcomes.row.names, .before = 1)
    ## Return the table
    return (outcomes.comparison)
}

## Compile all outcome comparisons in a single list
outcome.comparisons.list <- c(
    outcomes.tables,
    post.outcomes.compared.between.arms,
    change.from.baseline.compared.between.arms
)
names(outcome.comparisons.list) <- paste0("||table::", names(outcome.comparisons.list), "||")


## Convert all tables to named vectors
outcome.comparisons.vectors <- lapply(outcome.comparisons.list, convert_tibble_to_named_vector)
outcome.comparisons.vector <- unlist(outcome.comparisons.vectors)

convert_tibble_to_named_vector <- function(tibble) {
    listed.tibble <- as.list(tibble)
    row.names <- paste0("||row::index:", 1:nrow(tibble), "::", listed.tibble[[1]], "||")
    tibble.data.columns <- listed.tibble[-1]
    names(tibble.data.columns) <- paste0("||column::", names(tibble.data.columns), "||")
    unlisted.tibble.data.columns <- lapply(tibble.data.columns, unlist)
    named.tibble.data.columns <- lapply(unlisted.tibble.data.columns, setNames, nm = row.names)
    vector <- unlist(named.tibble.data.columns)
    return (vector)
}

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
