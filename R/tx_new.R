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
