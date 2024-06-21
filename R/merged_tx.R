#' Filter Active Patients
#'
#' Filters the patients based on their current status and date conditions.
#'
#' @param linelist A data frame containing the patient data.
#' @param status_column The name of the column indicating the patient's current status.
#' @param start_date The start date for the filtering period.
#' @param end_date The end date for the filtering period.
#' @return A filtered data frame with active patients.
#' @export
filter_active_patients <- function(linelist, status_column, start_date, end_date) {
  linelist %>%
    mutate(patient_stopped_treatment_date = as.Date(patient_stopped_treatment_date),
           transferred_out_date = as.Date(transferred_out_date),
           patient_deceased_date = as.Date(patient_deceased_date)) %>%
    filter(.data[[status_column]] == 'Active' &
             (is.na(patient_stopped_treatment_date) |
                !(patient_stopped_treatment_date >= as.Date(start_date) &
                    patient_stopped_treatment_date <= as.Date(end_date))) &
             (is.na(transferred_out_date) |
                !(transferred_out_date >= as.Date(start_date) &
                    transferred_out_date <= as.Date(end_date))) &
             (is.na(patient_deceased_date) |
                !(patient_deceased_date >= as.Date(start_date) &
                    patient_deceased_date <= as.Date(end_date))))
}

#' Filter TX_NEW Patients
#'
#' Filters the patients based on their ART start date and transfer status.
#'
#' @param linelist A data frame containing the patient data.
#' @param start_date The start date for the filtering period.
#' @param end_date The end date for the filtering period.
#' @param transfer_column The name of the column indicating if the patient was transferred in.
#' @return A filtered data frame with new patients (TX_NEW).
#' @export
filter_tx_new <- function(linelist, start_date, end_date, transfer_column) {
  linelist %>%
    mutate(art_start_date = as.Date(art_start_date)) %>%
    filter(art_start_date >= as.Date(start_date) &
             art_start_date <= as.Date(end_date) &
             .data[[transfer_column]] != 1)
}

#' Aggregate by Facility
#'
#' Aggregates the filtered data by IP, state name, and facility name.
#'
#' @param filtered_data A data frame containing the filtered patient data.
#' @return A data frame with the aggregated counts.
#' @export
aggregate_by_facility <- function(filtered_data) {
  filtered_data %>%
    group_by(ip, state_name, facility_name) %>%
    summarize(count = n(), .groups = 'drop')
}

# Function to write data to CSV
write_data_to_csv <- function(data, filename) {
  write.csv(data, filename, row.names = FALSE)
}

# Example usage
library(dplyr)
library(ndrtxnumbers)

# Sample data frame
linelist <- data.frame(
  currentStatus_28_Q2 = c('Active', 'Inactive', 'Active'),
  patient_stopped_treatment_date = c('2024-01-10', '2024-02-15', '2024-04-01'),
  transferred_out_date = c(NA, '2024-03-20', NA),
  patient_deceased_date = c(NA, NA, '2024-01-25'),
  art_start_date = c('2024-01-10', '2024-02-15', '2024-04-01'),
  patient_transferred_in = c(0, 1, 0),
  ip = c('IP1', 'IP2', 'IP3'),
  state_name = c('State1', 'State2', 'State3'),
  facility_name = c('Facility1', 'Facility2', 'Facility3')
)

# User-specified parameters for TX_CURR
status_column <- 'currentStatus_28_Q2'
start_date <- '2024-01-01'
end_date <- '2024-03-31'

# Filter and aggregate TX_CURR data
filtered_txcurr <- filter_active_patients(linelist, status_column, start_date, end_date)
aggregated_txcurr <- aggregate_by_facility(filtered_txcurr)

# Write TX_CURR data to CSV
write_data_to_csv(aggregated_txcurr, "aggregated_txcurr.csv")

# User-specified parameters for TX_NEW
transfer_column <- 'patient_transferred_in'

# Filter and aggregate TX_NEW data
filtered_txnew <- filter_tx_new(linelist, start_date, end_date, transfer_column)
aggregated_txnew <- aggregate_by_facility(filtered_txnew)

# Write TX_NEW data to CSV
write_data_to_csv(aggregated_txnew, "aggregated_txnew.csv")
