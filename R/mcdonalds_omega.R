# McDonald's Omega
# ==============================================================================


#' @title mcdonalds_omega
#' @description function for calculating McDonald's Omega for likert survey data
#'
#' DOCUMENTATION :)
#'
mcdonalds_omega <- function(data, fm, flip, plot, rotate) {

  psych_omega <- psych::omega(m = data,
                              fm = fm,
                              flip = flip,
                              plot = plot,
                              rotate = rotate)

  list(
    omega_h = psych_omega$omega_h,
    omega_t = psych_omega$omega.tot
  )
}
