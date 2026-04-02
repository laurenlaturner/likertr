# For the "likertr" class

# "likertr" needs to following properties to be called in summary/plot

new_likertr <- function(data, alpha, omega, rii, polychoric, 
  sphericity, kmo, nonparam, effect_size) {
  structure(
    class = "likertr",
    data = data,
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