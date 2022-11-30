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
                      "44805" = 0, # To be updated
                      "55356" = 43,
                      "78344" = 3,
                      "95846" = 0, # To be updated
                      "88456" = 0, # To be updated
                      "10263" = 0) # To be updated
pre.post.break.points <- list("11542" = "2022-10-01", # To be updated
                              "44805" = "2022-10-01", # To be updated
                              "55356" = "2022-10-01", # To be updated
                              "78344" = "2022-10-01", # To be updated
                              "95846" = "2022-10-01", # To be updated
                              "88456" = "2022-10-01", # To be updated
                              "10263" = "2022-10-01") # To be updated
n.patients <- nrow(data)
n.atls.residents <- 4 + 2 # The total number of residents trained in ATLS, per ATLS centre
n.atls.residents.passed.first.attempt <- 3 + 2 # The number of residents who had passed ATLS after the first attempt, per centre
n.atls.residents.passed.first.attempt <- 4 + 2 # The number of residents who had passed ATLS after the second attempt, per centre
n.ptc.residents <- 9 + 6 # The total number of residents trained in PTC, per centre
n.residents <- n.atls.residents + n.ptc.residents
centre.ids <- data %>% pull(id__reg_hospital_id) %>% unique() 
n.centres <-  centre.ids %>% length()
shuffled.centres <- centre.ids %>% sample(n.centres)
atls.centres <- shuffled.centres[1:2]
ptc.centres <- shuffled.centres[3:4]
control.centres <- shuffled.centres[5:n.centres]
centre.data <- data %>% split(data$id__reg_hospital_id)
atls.data <- bind_rows(centre.data[as.character(atls.centres)]) %>% labelled::copy_labels_from(data)
ptc.data <- bind_rows(centre.data[as.character(ptc.centres)]) %>% labelled::copy_labels_from(data)
control.data <- bind_rows(centre.data[as.character(control.centres)])  %>% labelled::copy_labels_from(data)
arms.data.list <- list(atls = atls.data, ptc = ptc.data, control = control.data)
n.atls <- nrow(atls.data)
n.ptc <- nrow(ptc.data)
n.control <- nrow(control.data)
n.females <- with(data, sum(patinfo__pt_gender == "Female"))
p.females <- round(n.females/nrow(data) * 100)
median.age <- median(data$patinfo__pt_age, na.rm = TRUE)
iqr.age <- quantile(data$patinfo__pt_age, probs = c(0.25, 0.75), na.rm = TRUE) %>% paste0(collapse = "-")

## Patient participant outcomes
n.outcomes <- lapply(data[, grep("outcomes__", names(data))])
n.m30d <- with(data, sum(outcomes__alive_after_30_days == "No", na.rm = TRUE))
p.m30d <- round(n.m30d/nrow(data) * 100)
arms.n.m30d.list <- lapply(setNames(arms.data.list, nm = paste0("m30d.", names(arms.data.list))),
                           function(arms.data)
                               with(arms.data,
                                    sum(outcomes__alive_after_30_days == "No", na.rm = TRUE)))
arms.p.m30d.list <- mapply(arms.n.m30d.list, arms.data.list,
                           FUN = function(count, arms.data) round(count/nrow(arms.data) * 100))
names(arms.n.m30d.list) <- paste0("n.", names(arms.n.m30d.list))
names(arms.p.m30d.list) <- paste0("p.", names(arms.p.m30d.list))
attach(arms.n.m30d.list)
attach(arms.p.m30d.list)



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

## Create table of sample characteristics
table.variables <- c("patinfo/pt_age", "patinfo/pt_gender",
                     "incident/dominating_injury_type",
                     "patvitals/ed_rr", "patvitals/ed_sat",
                     "patvitals/ed_hr", "patvitals/ed_sbp")
table.data <- data[, table.variables]

## Label variables
