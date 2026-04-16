#' Likertr Wrapper Function
#'
#' @description
#'
#' @param data a dataframe where each column is a likert survey question (item)
#'     and each row is a response.
#' @param max_val a numeric vector with length equal to the number of items in
#'   the dataframe. Specifies the point scale of each question by providing
#'   the max value answerable (eg. if a question is on a five point scale
#'   the value provided in max_val should be five).
#' @param na_decision Character. Strategy for handling `NA` values; if "neutral",
#'   missing values are replaced with the scale midpoint.
#' @param ipsatize_decision Logical. If `TRUE`, returns a version of the data
#'   centered by respondent (person-mean centering).
#' @param small_n_decision Character. If not "nothing", questions/groups with
#'   fewer than 20 responses are dropped.
#' @param groups a numeric vector specifying groups of questions.
#'   Cronbach's alpha information will be calculated separately for each
#'   group. The vector must be the same length as the number of items
#'   (columns) in the dataframe. Ex. c(1, 1, 2, 2) i.e questions 1-2 are in
#'   a group and questions 2-4 are in a group. Groups should be numbered
#'   1, 2, 3, ... , n.
#' @param std
#' @param empirical
#'
#' @example
#'
#' @export

likertr <- function(
    data,
    max_val,
    na_decision = "drop",
    ipsatize_decision = FALSE,
    small_n_decision = "nothing",
    groups = numeric(0), fm = "minres",
    flip = FALSE,
    plot = FALSE,
    rotate = "oblimin"
  ) {

  # Preparation and Cleaning
  clean_data <- preparation(data, na_decision, ipsatize_decision, small_n_decision)

  # EFA
  efa <- efa(clean_data)


  # Reliability and Structure
  alpha <- cronbachs_alpha(clean_data, groups)
  omega <- mcdonalds_omega(clean_data, fm, flip, plot, rotate)
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
