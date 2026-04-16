# For the "likertr" class

# "likertr" needs to following properties to be called in summary/plot

new_likertr <- function(data, alpha, omega, rii, polychoric,
                        pre_efa_diagnostics, efa_results, nonparam, effect_size) {
  structure(
    data,
    class = "likertr",
    alpha = alpha,
    omega = omega,
    rii = rii,
    pre_efa_diagnostics = pre_efa_diagnostics,
    efa_results = efa_results,
    nonparam = nonparam,
    effect_size = effect_size
  )
}
