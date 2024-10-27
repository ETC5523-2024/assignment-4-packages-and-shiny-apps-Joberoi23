#' Calculate Literacy Growth (2001-2011)
#'
#' @description This function calculates the percentage growth in literacy rates
#' from 2001 to 2011 for each state or union territory in the dataset.
#'
#' @param data A data frame containing literacy rates from the GOI dataset.
#' @return A data frame with the original data and an additional column, `literacy_growth`, showing the growth rate.
#' @examples
#' literacy_growth(data = GOI_data)
#' @export
literacy_growth <- function(data) {
  # Check if the necessary columns are present in the dataset
  if (!all(c("literacy_rate_persons_total_2001", "literacy_rate_persons_total_2011") %in% colnames(data))) {
    stop("Data must contain 'literacy_rate_persons_total_2001' and 'literacy_rate_persons_total_2011' columns.")
  }

  # Calculate literacy growth
  data$literacy_growth <- (data$literacy_rate_persons_total_2011 - data$literacy_rate_persons_total_2001) /
    data$literacy_rate_persons_total_2001 * 100
  data
}





