#' Pipeline for Cleaning and Preparing Likert Data
#' 
#' @description This is the primary entry point for processing raw survey data. It performs 
#'   a sequence of cleaning steps including type conversion, handle-missing-data 
#'   strategies, bias removal (fence-sitting/straight-lining), ipsatization, 
#'   and sample size validation.
#' 
#' @param data a dataframe where each column is a likert survey question (item)
#'     and each row is a response.
#' @param na_decision Character. Strategy for handling `NA` values; if "neutral", 
#'   missing values are replaced with the scale midpoint.
#' @param ipsatize_decision Logical. If `TRUE`, returns a version of the data 
#'   centered by respondent (person-mean centering).
#' @param small_n_decision Character. If not "nothing", questions/groups with 
#'   fewer than 20 responses are dropped.
#' 
#' @return A list containing:
#' \itemize{
#'   \item \strong{cleanest_data}: The final numeric data frame after all filters.
#'   \item \strong{questions}: Original column names (question text).
#'   \item \strong{num_questions}: Total count of Likert questions identified.
#'   \item \strong{num_people}: Vector of response counts per question.
#'   \item \strong{ipsatize}: The ipsatized data frame (or NULL if not requested).
#'   \item \strong{perc_by_question}: List of percentage distributions for plotting.
#' }
#' 
#' @export
preparation <- function(data, na_decision, ipsatize_decision, small_n_decision) {
    data <- general_cleaning(data)
    clean_data <- data[[1]]
    questions <- data[[2]]

    col_mins <- apply(clean_data, 2, min, na.rm = TRUE)
    col_maxs <- apply(clean_data, 2, max, na.rm = TRUE)
    neutrals <- (col_mins + col_maxs) / 2

    num_questions <- ncol(clean_data)

    cleaner_data <- adjust_nas(clean_data, na_decision, neutrals) |>
        bias_handling(neutrals, col_mins, col_maxs)

    ipsatize <- ipsatize(cleaner_data, ipsatize_decision)

    num_people <- colSums(!is.na(cleaner_data))
    cleanest_data <- noting_small_n(cleaner_data, num_people, small_n_decision)

    perc_by_question <- split_question(cleanest_data)
    
    return(list(cleanest_data, questions, num_questions, num_people, ipsatize, perc_by_question))
}

#'Initial Data Filtering and Question Extraction
#' 
#' @description Identifies numeric columns that follow a Likert format (integers 
#'   between 1 and 11) and renames them for internal processing.
#' 
#' @param data the raw data
general_cleaning <- function(data) {
    numeric_data <- data[, sapply(data, is.numeric), drop = FALSE]
    
    is_likert <- sapply(numeric_data, function(x) {
        vals <- x[!is.na(x)]
        if (length(vals) == 0) return(FALSE)
        all(vals >= 1 & vals <= 11) && all(vals %% 1 == 0)
    })
    numeric_data <- numeric_data[, is_likert, drop = FALSE]

    questions <- colnames(numeric_data)

    rownames(numeric_data) <- NULL
    colnames(numeric_data) <- paste0("Q", 1:ncol(numeric_data))

    return(list(numeric_data, questions))
}

#' Handle Response Bias and Low-Quality Entries
#' 
#' @description Filters out "low-effort" respondents, including:
#' \itemize{
#'   \item \strong{Fence-sitters}: Respondents who only chose the neutral midpoint.
#'   \item \strong{Straight-liners}: Respondents who only chose the scale minimum or maximum.
#'   \item \strong{Identical responses}: Respondents who gave the same value for every question.
#' }
#' @param data the raw data
#' @param neutrals the average integer value between the maximum and the minimum
#'   input values in each question
#' @param col_mins the lowest answered value in each question
#' @param col_maxs the largest answered value in each question
bias_handling <- function(data, neutrals, col_mins, col_maxs) {
    # Fence sitting drops
    is_fence_sitter <- rowSums(sweep(data, 2, neutrals, "-")  != 0, na.rm = TRUE) == 0
    
    # Straight lining drops
    is_straight_min <- rowSums(sweep(data, 2, col_mins, "-") != 0, na.rm = TRUE) == 0

    is_straight_max <- rowSums(sweep(data, 2, col_maxs, "-") != 0, na.rm = TRUE) == 0

    # Answering all the same drops
    is_all_same <- rowSums(data != data[, 1], na.rm = TRUE) == 0

    to_drop <- is_fence_sitter | is_straight_min | is_straight_max | is_all_same

    if (ncol(data) < 2) stop("More data is needed for effect bias handling.")

    return(data[!to_drop, , drop = FALSE])
}

#' Person-Mean Centering (Ipsatization)
#' 
#' @description Subtracts the respondent's average score from each of their individual 
#'   responses to control for individual response styles (e.g., general tendency to 
#'   agree or disagree).
#' 
#' @param data the raw data
#' @param ipsatize_decision a determination by the user if this data is wanted
ipsatize <- function(data, ipsatize_decision) {
    if (ipsatize_decision) {
        person_means <- rowMeans(data, na.rm = TRUE)
        ipsatized_data <- sweep(data, 1, person_means, "-")

        return(ipsatized_data)
    } else {
        return(NULL)
    }
}

#' Impute Missing Values
#' 
#' @description Handles `NA` values based on user preference, specifically 
#'   allowing replacement with the calculated scale midpoint.
#' 
#' @param data the raw data
#' @param na_decision a determination by the user if NAs should be 
#'   removed or replaced
#' @param neutrals the average integer value between the maximum and the minimum
#'   input values in each question
adjust_nas <- function(data, na_decision, neutrals) {
    if (na_decision == "neutral") {
        nas <- is.na(data)
        neutral_matrix <- matrix(neutrals, nrow = nrow(data), ncol = ncol(data), byrow = TRUE)
        data[nas] <- neutral_matrix[nas]
    }
    return(data)
}

#' Sample Size Validation
#' 
#' @description Checks if questions meet a minimum threshold (n=20) and 
#'   issues warnings or removes data accordingly.
#' 
#' @param data the raw data
#' @param num_people the number of people's responses per question
#' @param small_n_decision a determination by the user if questions with
#'   low numbers of responses should be dropped.
noting_small_n <-function(data, num_people, small_n_decision) {
    if (any(num_people < 20)) {
        message("Warning: Some groups have N < 20. Results may be unstable or non-representative.")
    }

    if (small_n_decision != "nothing") {
        data <- data[num_people >= 20, ]
    }
    return(data)
}

#' Convert Raw Data to Percentages
#' 
#' @description Takes the cleaned data frame and splits it into a list of 
#'   percentage distributions, one for each question, suitable 
#'   for Likert visualization. Calls the helper function [converting_to_percentage()].
#' 
#' @param the raw data
split_question <- function(data) {
    results <- lapply(data, converting_to_percentage)
    return(results)
}

#' Frequency to Percentage Calculation
#' 
#' @description Calculates the frequency of each response level within a 
#'   specific column, relative to the observed scale range, and returns 
#'   rounded percentages.
#' 
#' @param col the responses for each question
converting_to_percentage <- function(col) {
    clean_vals <- col[!is.na(col)]
    if (length(clean_vals) == 0) return(NULL)
    
    scale_range <- min(clean_vals):max(clean_vals)
    counts <- table(factor(col, levels = scale_range))
    
    return(round((counts / length(clean_vals)) * 100))
}