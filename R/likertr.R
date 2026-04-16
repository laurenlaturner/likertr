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
#' @param na_drop Character. Strategy for handling `NA` values; if "neutral",
#'   missing values are replaced with the scale midpoint.
#' @param ipsatize_decision Logical. If `TRUE`, returns a version of the data
#'   centered by respondent (person-mean centering).
#' @param small_n_drop Character. If not "nothing", questions/groups with
#'   fewer than 20 responses are dropped.
#' @param groups a numeric vector specifying groups of questions.
#'   Cronbach's alpha information will be calculated separately for each
#'   group. The vector must be the same length as the number of items
#'   (columns) in the dataframe. Ex. c(1, 1, 2, 2) i.e questions 1-2 are in
#'   a group and questions 2-4 are in a group. Groups should be numbered
#'   1, 2, 3, ... , n.
#' @param factor_inference column index referencing a factor variable to split
#'   data on for inference.
#' @param inference_vars A vector of column indices to perform inference on
#' @param inference_vars2 A vector of column indices to compare
#'   to inference_vars.
#' @param std
#' @param empirical
#'
#' @example
#'
#' @export

likertr <- function(
    data,
    max_val,
    na_drop = FALSE,
    n_fact,
    ipsatize_decision = FALSE,
    small_n_drop = FALSE,
    groups = numeric(0),
    factor_inference = NA,
    inference_vars = NA,
    inference_vars2 = NA,
    flip = FALSE,
    plot = FALSE
  ) {

  # Preparation and Cleaning
  clean_data <- preparation(data, na_drop, ipsatize_decision, small_n_drop)

  # EFA
  if (missing(n_fact)) {
    efa <- efa(clean_data)
  } else {
    efa <- efa(clean_data, n_fact)
  }


  # Reliability and Structure
  alpha <- cronbachs_alpha(clean_data, groups)
  omega <- mcdonalds_omega(clean_data, efa$efa_results$n_fact, flip, plot)
  rii <- rii(clean_data, max_val)

  # Inference and Reporting
  inference <- inference(clean_data, factor_inference, inference_vars,
                         inference_vars2)

  new_likertr(
    data = clean_data,
    alpha = alpha,
    omega = omega,
    rii = rii,
    pre_efa_diagnostics = efa$pre_efa_diagnostics,
    efa_results = efa$efa_results,
    nonparam = nonparam,
    effect_size = effect_size
  )
}
