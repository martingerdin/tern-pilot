create_patients_per_month_figure <- function(data) {
  library(ggplot2)

  # Create a data frame with the number of patients per month per cluster
  data$arrival.year.month <- data$incident__date_of_arrival %>%
    format("%Y-%m")
  patients.per.month <- data %>%
    group_by(id__reg_hospital_id, arrival.year.month) %>%
    summarise(n = n()) %>%
    ungroup()

  # Rename the hospital id variable to cluster
  patients.per.month <- patients.per.month %>%
    rename(cluster = id__reg_hospital_id)

  # Give each cluster a random id from 1 to the number of clusters
  patients.per.month$cluster <- patients.per.month$cluster %>%
    as.factor() %>%
    as.numeric() %>%
    paste0("Cluster ", .)

  # Format arrival.year.month as a date column
  patients.per.month$arrival.year.month.label <- patients.per.month$arrival.year.month
  patients.per.month$arrival.year.month <- patients.per.month$arrival.year.month %>%
    paste0("-01") %>%
    as.Date()

  patients.per.month$arrival.year.month <- factor(patients.per.month$arrival.year.month, labels = unique(patients.per.month$arrival.year.month.label))

  # Create a barchart with the number of patients per month per cluster
  patients.per.month.figure <- ggplot(patients.per.month, aes(x = arrival.year.month, y = n, fill = cluster)) +
    geom_bar(stat = "identity", color = "black", width = 0.65) +
    labs(x = "Year-month", y = "Number of patients", fill = "Cluster") +
    ggsci::scale_fill_bmj() +
    facet_grid(cluster ~ .) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
    scale_y_continuous(breaks = c(0, 15, 30))

  # Save the figure
  ggsave(patients.per.month.figure, filename = "patients-per-month.png", width = 85, height = 130, units = "mm")
}
