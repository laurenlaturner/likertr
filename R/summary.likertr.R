# For generic usage of summary() with "likertr" class

#' Summarize the results of a likertr object
#'
#' `summary.likertr()` is a generic summary function for objects of the
#'     likertr class. The function generates a summary report with EFA
#'     diagnostics and results, reliability measures, and inferential
#'     statistics.
#'
#' @param object A likertr object for which to generate a summary report
#' @param data_summary Logical. Determines whether or not a data summary
#'   is included in the [summary()] output.
#' @param efa_summary Logical. Determines whether or not an exploratory
#'   factor analysis summary is included in the [summary()] output.
#' @param reliability_summary Logical. Determines whether or not a
#'   reliability summary is included in the [summary()] output.
#' @param inference_summary Logical. Determines whether or not an inference
#'   summary is included in the [summary()] output.
#' @param ... All other arguments to be passed to this function.
#'
#' @export
summary.likertr <- function(
  object, data_summary = TRUE, efa_summary = TRUE,
  reliability_summary = TRUE, inference_summary = TRUE, ...
) {
  cat("================================================\n")
  cat("LIKERTR OBJECT SUMMARY REPORT\n")
  cat("================================================\n\n")

  if (data_summary) {
    data_summary(object)
  }
  if (efa_summary) {
    efa_summary(object)
  }
  if (reliability_summary) {
    reliability_summary(object)
  }
  if (inference_summary) {
    inference_summary(object)
  }
}
