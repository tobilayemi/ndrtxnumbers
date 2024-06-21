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
