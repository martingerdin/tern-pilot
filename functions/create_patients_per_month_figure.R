create_patients_per_month_figure <- function(prepared.data) {
  library(ggplot2)

  # Create a data frame with the number of patients per month per cluster
  prepared.data$arrival.year.month <- prepared.data$incident__date_of_arrival %>%
    format("%Y-%m")
  patients.per.month <- prepared.data %>%
    group_by(id__reg_hospital_id, arrival.year.month) %>%
    summarise(n = n()) %>%
    ungroup()

  # Rename the hospital id variable to cluster
  patients.per.month <- patients.per.month %>%
    rename(cluster = id__reg_hospital_id)

  # Format arrival.year.month as a date column
  patients.per.month$arrival.year.month.label <- patients.per.month$arrival.year.month
  patients.per.month$arrival.year.month <- patients.per.month$arrival.year.month %>%
    paste0("-01") %>%
    as.Date() %>%
    factor(levels = as.numeric(patients.per.month$arrival.year.month), labels = patients.per.month$arrival.year.month.label)


  # Create a barchart with the number of patients per month per cluster
  ggplot(patients.per.month, aes(x = arrival.year.month, y = n)) +
    geom_bar(stat = "identity", width = 3) +
    labs(x = "Year-month", y = "Number of patients", fill = "Cluster") +
    facet_grid(cluster ~ .) +
    theme_minimal()

  # Save the figure
  ggsave("patients_per_month.png", width = 10, height = 6)
}

prepared.data |>
  filter(id__reg_hospital_id == "95846") |>
  select(incident__date_of_arrival)
