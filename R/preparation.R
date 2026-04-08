#' Preparation and Cleaning
#' 
#' @param data
#' @param na_decision
#' @param ipsatize_decision
#' @param small_n_decision
#' 
#' @export
preparation <- function(data, na_decision = "drop", ipsatize_decision = FALSE, small_n_decision = "nothing") {
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
    
    return(list(cleanest_data, questions, num_questions, num_people, ipsatize))
}

general_cleaning <- function(data) {
    numeric_data <- data[, sapply(data, is.numeric), drop = FALSE]
    questions <- colnames(numeric_data)

    rownames(numeric_data) <- NULL
    colnames(numeric_data) <- paste0("Q", 1:ncol(numeric_data))

    return(list(numeric_data, questions))
}

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

ipsatize <- function(data, ipsatize_decision) {
    if (ipsatize_decision) {
        person_means <- rowMeans(data, na.rm = TRUE)
        ipsatized_data <- sweep(data, 1, person_means, "-")

        return(ipsatized_data)
    } else {
        return(NULL)
    }
}

adjust_nas <- function(data, na_decision, neutrals) {
    if (na_decision == "neutral") {
        nas <- is.na(data)
        neutral_matrix <- matrix(neutrals, nrow = nrow(data), ncol = ncol(data), byrow = TRUE)
        data[nas] <- neutral_matrix[nas]
    }
    return(data)
}

noting_small_n <-function(data, num_people, small_n_decision) {
    if (any(num_people < 20)) {
        message("Warning: Some groups have N < 20. Results may be unstable or non-representative.")
    }

    if (small_n_decision != "nothing") {
        data <- data[num_people >= 20, ]
    }
    return(data)
}