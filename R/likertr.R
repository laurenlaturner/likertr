#' Likertr Wrapper Function
#'
#' @description A comprehensive wrapper for psychometric analysis of
#'   Likert-scale survey data. This function automates the pipeline
#'   from data cleaning (including optional ipsatization and NA handling)
#'   to Exploratory Factor Analysis (EFA), reliability testing
#'   (Cronbach's alpha and McDonald's omega), Relative Importance Index
#'   (RII) calculation, and inferential testing.
#'
#' @param data a dataframe where each column is a likert survey question (item)
#'     and each row is a response.
#' @param n_fact Integer. Optional argument for the number of factors to be used
#'   in the Exploratory Factor Analysis (EFA). If not given, this argument will
#'   be decided using parallel analysis.
#' @param na_drop Logical. Strategy for handling `NA` values; if `FALSE`,
#'   missing values are replaced with the scale midpoint.
#' @param ipsatize_decision Logical. If `TRUE`, returns a version of the data
#'   centered by respondent (person-mean centering).
#' @param small_n_drop Logical. If TRUE, questions/groups with
#'   fewer than 20 responses are dropped.
#' @param groups a numeric vector specifying groups of questions.
#'   Cronbach's alpha information will be calculated separately for each
#'   group. The vector must be the same length as the number of items
#'   (columns) in the dataframe. Ex. c(1, 1, 2, 2) i.e questions 1-2 are in
#'   a group and questions 2-4 are in a group. Groups should be numbered
#'   1, 2, 3, ... , n.
#' @param category integer corresponding to a column index
#'   referencing a factor variable to split data on for inference.
#' @param flip If flip is TRUE, then items are automatically flipped to have
#'   positive correlations on the general factor (recommended to do this
#'   manually before). Defaults to FALSE.
#' @param plot Whether or not to call omega.diagram. Defaults to FALSE.
#'
#' @examples
#' \dontrun{
#' load("data/data.rda")
#' # Run the wrapper with 2-group reliability check
#' # Group 1: Items 1-2, Group 2: Items 3-5
#' results <- likertr(
#'   data = data,
#'   n_fact = 1,
#'   na_drop = TRUE,
#'   groups = c(1, 1, 2, 2, 2),
#'   ipsatize_decision = TRUE
#' )
#' }
#'
#' @export

likertr <- function(
  data,
  n_fact,
  na_drop = FALSE,
  ipsatize_decision = FALSE,
  small_n_drop = FALSE,
  groups = numeric(0),
  category = NA,
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

  pre_efa_diagnostics <- efa$pre_efa_diagnostics
  efa_results <- efa$efa_results


  # Reliability and Structure
  alpha <- cronbachs_alpha(clean_data, groups)
  omega <- mcdonalds_omega(clean_data, efa$efa_results$n_fact, flip, plot)
  rii <- rii(clean_data, max_val)

  # Inference and Reporting
  inference <- inference(
    clean_data, category, groups
  )

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
