library(pbapply)
library(openssl)
library(dotenv)
# library(DataExplorer)

load_packages <- function(packages = c(
                              "dotenv",
                              "dplyr",
                              "tibble",
                              "lubridate",
                              "gtsummary",
                              "naniar",
                              "icdpicr",
                              "boot",
                              "pbapply",
                              "openssl",
                              "lme4"
                              # "DataExplorer"
                          )) {
    for (package in packages) library(package, character.only = TRUE)
}
