# McDonald's Omega
# ==============================================================================


#' ADD DOCUMENTATION :)
mcdonalds_omega <- function(data, loadings, empirical = TRUE) {

  # ADD SUPPORT FOR MULTIFACTOR MODELS ?

  numerator <- sum(loadings)^2

  if (empirical) {
    denominator <- sum(cov(data))
    return(numerator / denominator)
  }

  item_vars <- apply(data, 2, var)
  residuals <- item_vars - (loadings^2)
  denominator <- sum(loadings)^2 + sum(residuals)

  numerator / denominator
}
