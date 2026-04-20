# For generic usage of summary() with "likertr" class

#' Summarize the results of a likertr object
#'
#' `summary.likertr()` is a generic summary function for objects of the
#'     likertr class. The function generates a summary report with EFA
#'     diagnostics and results, reliability measures, and inferential
#'     statistics.
#'
#' @param x A likertr object for which to generate a summary report
#'
#' @export
summary.likertr <- function(x, ...) {
  data <- attr(x, "data")
  n_q <- data[[3]]
  n_obs <- max(data[[4]])

  cat("================================================\n")
  cat("LIKERTR OBJECT SUMMARY REPORT\n")
  cat("================================================\n")
  cat(paste0("This dataset contains ", n_q, " questions with ", n_obs, " total observations.\n"))

  cat("================================================\n")
  cat("Exploratory Factor Analysis (EFA)\n")
  cat("================================================\n\n")

  cat("================================================\n")
  cat("Pre-EFA Diagnostics\n")
  cat("================================================\n\n")

  sph_p_val <- x$pre_efa_diagnostics$sphericity$p_value
  MSAi <- x$pre_efa_diagnostics$kmo$MSAi
  user_n_fact <- x$efa_results$user_n_fact
  n_fact <- x$efa_results$n_fact

  invalid_kmo <- MSAi < 0.6

  if (sph_p_val > 0.05) {
    cat(paste0("Bartlett's sphericity test resulted in a non-significant ",
              "(p<0.05) p-value of ",
              sph_p_val,
              ", variables may not be correlated enough for EFA\n\n")
        )
  } else {
    cat(paste("Bartlett's sphericity test does not show problematic results",
              "\n\n")
        )
  }


  if (any(invalid_kmo)) {
    invalid_kmo <- MSAi[!kmo_validity]
    cat(paste("At least one MSAi value from the KMO test is less than 0.6 and",
              "possibly problematic, the following feature or features share",
              "little variance with the rest of the data:\n")
        )
    print(invalid_kmo)
    cat("\n")
  } else {
    cat(paste("The KMO test does not show problematic results",
              "\n\n")
        )
  }


  cat(paste("Polychoric correlation matrix results can be viewed using the",
            "'plot' function\n - Lots of very low coefficients in the matrix",
            "indicate low factorability\n - Lots of very high loadings",
            "indicate redundant variables\n\n"))


  cat("================================================\n")
  cat("EFA Results\n")
  cat("================================================\n\n")

  loadings <- round(x$efa_results$loadings, 4)
  var_exp <- round(x$efa_results$var_exp, 4)
  communality <- round(x$efa_results$communality, 4)
  fc_matrix <- round(x$efa_results$fc_matrix, 4)
  RMSEA <- round(x$efa_results$RMSEA, 4)
  TLI <- round(x$efa_results$TLI, 4)
  CFI <- round(x$efa_results$CFI, 4)


  if (user_n_fact) {
    cat(paste0("User-supplied number of factors (",
               n_fact,
               ") was used for EFA\n\n"))
  } else {
    cat(paste0("No 'n' argument was given and number of factors (",
               n_fact,
               ") used in EFA was determined using parallel analysis\n\n",
               "Check parallel analysis Scree plot using 'plot' function for ",
               "more details\n\n")
    )
  }


  # EFA Loadings

  cat("EFA Loadings (Standardized):\n")
  print(loadings)
  cat("\n\n")


  # Variance Explained

  cat("Variance Explained:\n")
  print(var_exp)
  cat("\n\n")


  # Communality

  # Recommend to get rid of variables with a communality < 0.2

  cat("Communality:\n")
  print(communality)
  cat("\n\n")

  low_communality <- communality < 0.2

  if (any(low_communality)) {
    lc_variables <- communality[low_communality]
    cat(paste("The following variables have communality values less than 0.2,",
              "which means that very little of their variance is explained by",
              "the common factors and they should be considered for removal:\n")
    )
    print(lc_variables)
    cat("\n")
  } else {
    cat(paste("There are no variables with a communality less than 0.2,",
              "but it is reccommended to review the communality values that",
              "show how much of the variance of each variable is explained by",
              "the common factors"))
  }

  # We want variables with a high communality that contribute strongly to the
  # common factors


  cat("Factor Correlation Matrix:\n")
  print(fc_matrix)
  cat("\n\n")

  cat("Measures of Fit:\n")
  cat(paste("RMSEA:", RMSEA, "\n"))
  if (RMSEA < 0.05) {
    cat("This RMSEA value indicates a good model fit")
  } else if (RMSEA < 0.08) {
    cat("This RMSEA value indicates an acceptable model fit")
  } else if (RMSEA < 0.1) {
    cat("This RMSEA value indicates a marginal model fit")
  } else {
    cat("This RMSEA value indicates a poor model fit")
  }
  cat("\n\n")

  cat(paste("TLI:", TLI, "\n"))
  if (TLI > 0.95) {
    cat("This TLI value indicates a good model fit")
  } else if (TLI > 0.9) {
    cat("This TLI value indicates an acceptable model fit")
  } else {
    cat("This TLI value indicates a poor model fit")
  }
  cat("\n\n")

  cat(paste("CFI:", CFI, "\n"))
  if (CFI > 0.95) {
    cat("This CFI value indicates a good model fit")
  } else if (CFI > 0.9) {
    cat("This CFI value indicates an acceptable model fit")
  } else {
    cat("This CFI value indicates a poor model fit")
  }
  cat("\n\n")


  cat(paste("Keep in mind that the interpretation of many of these statistics",
            "will depend on the context of your analysis\n\n"))

  cat("================================================\n")
  cat("Cronbach's Alpha\n")
  cat("================================================\n\n")

  # Extract alpha information from likertr object
  alpha <- x$alpha
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
  rii <- x$rii

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
  omega <- x$omega

  # Print
  cat("Omega Hierarchical:     ", omega$omega_h, "\n")
  cat("Omega Total:            ", omega$omega_t, "\n\n")
}
