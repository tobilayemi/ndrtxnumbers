#' Load Data
#'
#' This function loads data from a CSV file and returns it as a dataframe.
#'
#' @param filepath The path to the CSV file.
#' @return A dataframe containing the loaded data.
#' @examples
#' linelist <- load_data("path/to/tll_6_6_2024.csv")
#' @import dplyr
#' @import tidyr
#' @import lubridate
#' @export
load_data <- function(filepath) {
  # Ensure the file exists
  if (!file.exists(filepath)) {
    stop("File does not exist: ", filepath)
  }

  # Read the CSV file into a dataframe
  data <- read.csv(filepath)

  # Return the dataframe
  return(data)
}
