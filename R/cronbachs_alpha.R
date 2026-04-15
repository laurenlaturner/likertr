# Cronbach's Alpha
# ==============================================================================


#' @title cronbachs_alpha
#'
#' @description used to find Cronbach's alpha information for a dataset.
#'
#' @param data a dataframe where each column is a likert survey question (item)
#'     and each row is a response.
#' @param groups a numeric vector specifying groups of questions.
#'     Cronbach's alpha information will be calculated separately for each
#'     group. The vector must be the same length as the number of items
#'     (columns) in the dataframe. Ex. c(1, 1, 2, 2) i.e questions 1-2 are in
#'     a group and questions 2-4 are in a group. Groups should be numbered
#'     1, 2, 3, ... , n.
#'
#' @returns a list containing lists of Cronbach's alpha information for each
#'     group. Each group sub-list contains the Cronbach's alpha for the group
#'     as well as the Cronbach's alpha when each item (question) is left out.
cronbachs_alpha <- function(data, groups) {
  if (length(groups) == 0) {
    groups <- rep(1, ncol(data))
    message(
      "No item grouping specified. Cronbach's Alpha calculated assuming all items in same group."
    )
  }

  # ADD MORE ERROR HANDLING :)

  num_groups <- max(groups)
  alpha_info <- vector("list", num_groups)

  # Calculate Cronbach's Alpha information for each group
  for (i in seq_len(num_groups)) {
    group_data <- data[, groups == i]
    group_alpha <- c_alpha_calc(group_data)

    # Calculate leaving out each item in the group
    num_group_items <- ncol(group_data)
    sub_group_alphas <- numeric(num_group_items)
    item_names <- character(num_group_items)

    for (j in seq_len(num_group_items)) {
      sub_group_data <- sub_group_data <- group_data[, -j, drop = FALSE]
      sub_group_alphas[j] <- c_alpha_calc(sub_group_data)
      item_names[j] <- names(group_data)[j]
    }

    sub_group_alphas_df <- data.frame(
      Item = item_names,
      Alpha = sub_group_alphas
    )

    alpha_info[[i]] <- list(group_alpha, sub_group_alphas_df)
  }

  alpha_info
}



#' @title c_alpha_calc
#'
#' @description function for calculating cronbach's alpha for a dataset
#'
#' @param data a dataframe where each column is a survey question (item)
#'     and each row is a response.
#'
#' @details this function handles the math for a prepared and filtered dataset.
#'     See [cronbachs_alpha()] for the process of the analysis.
#'
#' @returns a numeric value. cronbach's alpha for the provided data.
c_alpha_calc <- function(data) {
  # Number of items (questions) in the dataset
  num_items <- ncol(data)

  # Create total column and calculate variance
  tot_col <- rowSums(data)
  tot_var <- var(tot_col)

  # Calculate the sum of variances of each item (question)
  sum_item_var <- sum(sapply(data, var))

  # Calculate and return Cronbach's Alpha
  (num_items / (num_items - 1)) * ((tot_var - sum_item_var) / tot_var)
}

