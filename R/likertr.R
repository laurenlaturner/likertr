#' Likertr Wrapper Function
#' 
#' @description
#' 
#' @param data
#' @param max_val
#' @param na_decision
#' @param ipsatize_decision
#' @param small_n_decision
#' @param groups
#' @param std
#' @param empirical
#' 
#' @example
#' 
#' @export

likertr <- function(data, max_val, na_decision = "drop", ipsatize_decision = FALSE, small_n_decision = "nothing", 
  groups = numeric(0), std = TRUE, empirical = TRUE) {
  # Preparation and Cleaning
  clean_data <- preparation(data, na_decision, ipsatize_decision, small_n_decision)

  # EFA
  efa <- efa(clean_data)


  # Reliability and Structure
  alpha <- cronbachs_alpha(clean_data, groups)
  omega <- mcdonalds_omega(clean_data, loadings, std, empirical)
  rii <- rii(clean_data, max_val)

  # Inference and Reporting
  inference <- inference(clean_data)

  new_likertr(
    data = clean_data,
    alpha = alpha,
    omega = omega,
    rii = rii,
    polychoric = polychoric,
    sphericity = sphericity,
    kmo = kmo,
    nonparam = nonparam,
    effect_size = effect_size
  )
}
