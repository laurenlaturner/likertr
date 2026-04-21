reliability_summary <- function(object) {
  cat("================================================\n")
  cat("Cronbach's Alpha\n")
  cat("================================================\n\n")

  # Extract alpha information from likertr object
  alpha <- attributes(object)$alpha
  groups <- length(alpha)

  for (i in seq_len(groups)) {
    # If more than one group of questions, list each group
    if (groups != 1) {
      cat("--------------- Group:", i, "---------------\n")
    }

    # Print overall alpha value
    cat("Alpha =", alpha[[i]][[1]], "\n\n")

    # Print exclusion alphas
    items <- nrow(alpha[[i]][[2]])

    cat("Alpha after removing: \n")

    for (j in seq_len(items)) {
      if (alpha[[i]][[2]][j, 2] > alpha[[i]][[1]]) {
        cat(
          alpha[[i]][[2]][j, 1],
          ":     ",
          alpha[[i]][[2]][j, 2],
          "(*)\n"
        )
      } else {
        cat(
          alpha[[i]][[2]][j, 1],
          ":     ",
          alpha[[i]][[2]][j, 2],
          "\n"
        )
      }
    }
    cat("\n")
  }

  cat("================================================\n")
  cat("Relative Importance Index (RII)\n")
  cat("================================================\n\n")

  # Extract RII information from likertr object
  rii <- attributes(object)$rii

  # Print
  rows <- nrow(rii)

  for (i in seq_len(rows)) {
    cat(rii[i, 1], ":     ", rii[i, 2], "\n")
  }

  cat("\n")

  cat("================================================\n")
  cat("McDonald's Omega\n")
  cat("================================================\n\n")

  # Extract Omega information from likertr object
  omega <- attributes(object)$omega

  # Print
  cat("Omega Hierarchical:     ", omega$omega_h, "\n")
  cat("Omega Total:            ", omega$omega_t, "\n\n")
}
