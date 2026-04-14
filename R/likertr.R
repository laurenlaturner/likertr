# This is the overall wrapper function that will run everything

likertr <- function(data) {
  # Preparation and Cleaning
  clean_data <- preparation(data)

  # Reliability and Structure
  alpha <- cronbachs_alpha(clean_data)
  omega <- mcdonalds_omega(clean_data)
  rii <- rii(clean_data)

  # EFA
  efa <- efa(clean_data)

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