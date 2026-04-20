# McDonald's Omega
# ==============================================================================


#' @title mcdonalds_omega
#' @description function for calculating McDonald's Omega for likert survey data
#'
#' @param data a dataframe where each column is a likert survey question (item)
#'     and each row is a response.
#' @param n_fact Number of factors believed to be group factors
#' @param flip If flip is TRUE, then items are automatically flipped to have
#'     positive correlations on the general factor (recommended to do this
#'     manually before)
#' @param plot Whether or not to call omega.diagram
#'
#' @importFrom psych omega
#'
#' @returns list containing omega hierarchical and omega total values
mcdonalds_omega <- function(data, n_fact, flip, plot) {

  psych_omega <- psych::omega(m = data,
                              nfactors = n_fact,
                              flip = flip,
                              plot = plot)

  list(
    omega_h = psych_omega$omega_h,
    omega_t = psych_omega$omega.tot
  )
}
