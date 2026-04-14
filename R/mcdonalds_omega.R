# McDonald's Omega
# ==============================================================================


#' @title mcdonalds_omega
#' @description function for calculating McDonald's Omega for likert survey data
#'
#' @param data a dataframe where each column is a likert survey question (item)
#'     and each row is a response.
#' @param loadings vector of factor loadings
#' @param std logical indicating if the factor loadings are standardized or not.
#'     If no value is provided, defaults to TRUE.
#' @param empirical logical indicating whether to use Empirical (Observed)
#'     calculation method or Model-Implied calculation method. If no value is
#'     provided, defaults to TRUE.
#'
#' @returns numeric representing McDonald's Omega for the given data and
#'     loadings
mcdonalds_omega <- function(data, loadings, std = TRUE, empirical = TRUE) {

  # ADD SUPPORT FOR MULTIFACTOR MODELS ?

  numerator <- sum(loadings)^2

  if (empirical) {
    if (std) {
      denominator <- sum(cor(data, use = "complete.obs"))
    } else {
      denominator <- sum(cov(data, use = "complete.obs"))
    }
  } else {
    if (std) {
      residuals <- 1 - (loadings^2)
    } else {
      item_vars <- apply(data, 2, var, na.rm = TRUE)
      residuals <- item_vars - (loadings^2)
    }
    denominator <- sum(loadings)^2 + sum(residuals)
  }

  numerator / denominator
}
