#' Calculate Literacy Growth (2001-2011)
#'
#' @description Calculates the growth in literacy rates from 2001 to 2011.
#' @param data A data frame with literacy rates for 2001 and 2011.
#' @return The input data frame with an additional `literacy_growth` column.
#' @export
literacy_growth <- function(data) {
  if (!all(c("literacy_rate_persons_total_2001", "literacy_rate_persons_total_2011") %in% colnames(data))) {
    stop("Data must contain 'literacy_rate_persons_total_2001' and 'literacy_rate_persons_total_2011' columns.")
  }

  data$literacy_growth <- (data$literacy_rate_persons_total_2011 - data$literacy_rate_persons_total_2001) /
    data$literacy_rate_persons_total_2001 * 100
  data
}





