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

## Label variables


