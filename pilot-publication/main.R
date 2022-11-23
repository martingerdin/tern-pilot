## Load packages
library(dotenv)
library(dplyr)
library(lubridate)

## Import data
data <- readr::read_csv(Sys.getenv("DATA_DIR"))

## Import codebook
codebook.arguments <- lapply(c("URL", "UID", "USERNAME", "PASSWORD"),
                             function(x) Sys.getenv(paste0("KOBO_", x)))
codebook <- do.call(noacsr::kobo_get_project_codebook, codebook.arguments)

## Define basic results
arrival.dates <- data %>% pull("incident/date_of_arrival") %>% sort()
format_date <- function(date) paste0(month(date[1], label = TRUE, abbr = FALSE), " ", year(date[1]))
start.date <- format_date(arrival.dates[1])
end.date <- format_date(rev(arrival.dates)[1])
n.patients <- nrow(data)
n.atls.residents <- 4 + 2 # The total number of residents trained in ATLS, per ATLS centre
n.atls.residents.passed.first.attempt <- 3 + 2 # The number of residents who had passed ATLS after the first attempt, per centre
n.atls.residents.passed.first.attempt <- 4 + 2 # The number of residents who had passed ATLS after the second attempt, per centre
n.ptc.residents <- 9 + 6 # The total number of residents trained in PTC, per centre
n.residents <- n.atls.residents + n.ptc.residents
centre.ids <- data %>% pull("id/reg_hospital_id") %>% unique() 
n.centres <-  centre.ids %>% length()
shuffled.centres <- centre.ids %>% sample(n.centres)
atls.centres <- shuffled.centres[1:2]
ptc.centres <- shuffled.centres[3:4]
control.centres <- shuffled.centres[5:n.centres]
centre.data <- data %>% split(data$`id/reg_hospital_id`)
atls.data <- do.call(rbind, centre.data[as.character(atls.centres)])
ptc.data <- do.call(rbind, centre.data[as.character(ptc.centres)])
control.data <- do.call(rbind, centre.data[as.character(control.centres)])
n.atls <- nrow(atls.data)
n.ptc <- nrow(ptc.data)
n.control <- nrow(control.data)
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
recruitment.rate.patients <- 

## Label variables


