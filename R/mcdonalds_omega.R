# McDonald's Omega
# ==============================================================================


#' @title mcdonalds_omega
#' @description function for calculating McDonald's Omega for likert survey data
#'
#' DOCUMENTATION :)
#'
mcdonalds_omega <- function(data, flip, plot, nfactors) {

  psych_omega <- psych::omega(m = data,
                              nfactors = nfactors,
                              flip = flip,
                              plot = plot)

  list(
    omega_h = psych_omega$omega_h,
    omega_t = psych_omega$omega.tot
  )
}
