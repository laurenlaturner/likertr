#' Likertr Wrapper Function
#'
#' @description
#'
#' @param data a dataframe where each column is a likert survey question (item)
#'     and each row is a response.
#' @param na_drop Character. Strategy for handling `NA` values; if "neutral",
#'   missing values are replaced with the scale midpoint.
#' @param n_fact Integer. Optional argument for the number of factors to be used
#'   in the Exploratory Factor Analysis (EFA). If not given, this argument will
#'   be decided using parallel analysis.
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
#' @param factor_inference integer corresponding to a column index referencing a factor variable to split
#'   data on for inference.
#' @ param inference_vars vector of integers corresponding to column indicies to use for inferential analysis
#' @example
#'
#' @export

likertr <- function(
    data,
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
  clean_data_list <- preparation(data, na_drop, ipsatize_decision, small_n_drop)
  clean_data <- clean_data_list[[1]]
  max_val <- clean_data_list[[7]]

  # EFA
  if (missing(n_fact)) {
    efa <- efa(clean_data)
  } else {
    efa <- efa(clean_data, n_fact)
  }

  pre_efa_diagnostics = efa$pre_efa_diagnostics
  efa_results = efa$efa_results


  # Reliability and Structure
  alpha <- cronbachs_alpha(clean_data, groups)
  omega <- mcdonalds_omega(clean_data, efa$efa_results$n_fact, flip, plot)
  rii <- rii(clean_data, max_val)

  # Inference and Reporting
  inference <- inference(clean_data, factor_inference, inference_vars,
                         inference_vars2)

  test <- inference$test
  effect_size <- inference$effect_size

  new_likertr(
    data = clean_data_list,
    alpha = alpha,
    omega = omega,
    rii = rii,
    pre_efa_diagnostics = pre_efa_diagnostics,
    efa_results = efa_results,
    test = test, 
    effect_size = effect_size
  )
}
