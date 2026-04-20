# Relative Importance Index (RII)
# ==============================================================================


#' @title rii
#'
#' @description used to calculate the relative importance index for each item
#'     (question) in a likert survey.
#'
#' @param data a dataframe where each column is a likert survey question (item)
#'     and each row is a response.
#' @param max_val a numeric vector with length equal to the number of items in
#'     the dataframe. Specifies the point scale of each question by providing
#'     the max value answerable (eg. if a question is on a five point scale
#'     the value provided in max_val should be five).
#'
#' @returns a numeric vector with the relative importance index for each
#'     question.
rii <- function(data, max_val) {
  data.frame(
    item = names(data),
    rii = colSums(data) / (max_val * nrow(data))
  )
}
