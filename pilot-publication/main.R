## Load packages
library(dotenv)
library(dplyr)
library(lubridate)

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
arrival.dates <- data %>% pull(incident__date_of_arrival) %>% sort()
format_date <- function(date) paste0(month(date[1], label = TRUE, abbr = FALSE), " ", year(date[1]))
start.date <- format_date(arrival.dates[1])
end.date <- format_date(rev(arrival.dates)[1])
n.no.consent <-  list("11542" = 40,
                      "44805" = 10,
                      "55356" = 43,
                      "78344" = 3,
                      "95846" = 9, 
                      "88456" = 0, # To be updated
                      "10263" = 2) 
pre.post.break.points <- list("11542" = "2022-04-24", 
                              "44805" = c("2022-05-30", "2022-06-20"), 
                              "55356" = "2022-09-02", 
                              "78344" = "2022-06-03", 
                              "95846" = "2022-09-01", 
                              "88456" = NA, # To be updated
                              "10263" = NA) # To be updated
pre.post.break.points <- lapply(pre.post.break.points, as.Date)
n.patients <- nrow(data)
n.atls.residents <- 4 + 2 # The total number of residents trained in ATLS, per ATLS centre
n.ptc.residents <- 9 + 6 # The total number of residents trained in PTC, per centre
n.residents <- n.atls.residents + n.ptc.residents
centre.ids <- data %>% pull(id__reg_hospital_id) %>% unique() 
n.centres <-  centre.ids %>% length()
atls.centres <- c("44805", "78344")
ptc.centres <- c("55356", "95846")
control.centres <- c("11542", "88456", "10263")
centre.data <- data %>% split(data$id__reg_hospital_id)
atls.data <- bind_rows(centre.data[as.character(atls.centres)]) %>% labelled::copy_labels_from(data)
atls.data$arm <-"ATLS"
ptc.data <- bind_rows(centre.data[as.character(ptc.centres)]) %>% labelled::copy_labels_from(data)
ptc.data$arm <- "PTC"
control.data <- bind_rows(centre.data[as.character(control.centres)]) %>% labelled::copy_labels_from(data)
control.data$arm <- "Standard care"
arms.data.list <- list(atls = atls.data, ptc = ptc.data, control = control.data)
data <- bind_rows(arms.data.list) %>% labelled::copy_labels_from(data)
n.atls <- nrow(atls.data)
n.ptc <- nrow(ptc.data)
n.control <- nrow(control.data)
n.females <- with(data, sum(patinfo__pt_gender == "Female"))
p.females <- round(n.females/nrow(data) * 100)
median.age <- median(data$patinfo__pt_age, na.rm = TRUE)
iqr.age <- quantile(data$patinfo__pt_age, probs = c(0.25, 0.75), na.rm = TRUE) %>%
    paste0(collapse = "-")

## Patient participant outcomes

## Create table of sample characteristics
table.variables <- c("patinfo__pt_age", "patinfo__pt_gender",
                     "incident__dominating_injury_type",
                     "patvitals__ed_rr", "patvitals__ed_sat",
                     "patvitals__ed_hr", "patvitals__ed_sbp",
                     "outcomes__alive_after_30_days",
                     "arm")
table.data <- data[, table.variables]
sample.characteristics.table <- create_descriptive_table(table.data,
                                                         strata = "arm",
                                                         caption = "Patient sample characteristics")
sample.characteristics.data <- as.data.frame(sample.characteristics.table)

## Create table comparing secondary outcomes
secondary.outcomes <- c(names(binary_outcomes()),
                        categorical_outcomes(),
                        quantitative_outcomes())
secondary.outcomes.data <- data[, c(secondary.outcomes, "arm")]
secondary.outcomes.table <- create_descriptive_table(secondary.outcomes.data,
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


