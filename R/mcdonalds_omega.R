# McDonald's Omega
# ==============================================================================


#' ADD DOCUMENTATION :)
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
