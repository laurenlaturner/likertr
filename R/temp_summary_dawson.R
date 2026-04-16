

test_efa_summary <- function(obj) {
  cat("================================================\n")
  cat("Exploratory Factor Analysis (EFA)\n")
  cat("================================================\n\n")

  cat("================================================\n")
  cat("Pre-EFA Diagnostics\n")
  cat("================================================\n\n")

  sph_p_val <- obj$pre_efa_diagnostics$sphericity$p_value
  MSAi <- obj$pre_efa_diagnostics$kmo$MSAi
  user_n_fact <- obj$efa_results$user_n_fact
  n_fact <- obj$efa_results$n_fact

  kmo_validity <- MSAi |>
    unname() |>
    lapply(function(x) x > 0.6) |>
    unlist()

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


  if (all(kmo_validity) != TRUE) {
    # invalid_kmo <- list(attributes(MSAi)$names['FALSE'])
    invalid_kmo <- MSAi[!kmo_validity]
    # row.names(invalid_kmo) <- c("Feature", "MSAi")
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

  loadings <- obj$efa_results$loadings
  var_exp <- obj$efa_results$var_exp
  communality <- obj$efa_results$communality


  if (user_n_fact) {
    cat(paste0("User-supplied number of factors (",
               n_fact,
               ") was used for EFA\n\n"))
  } else {
    cat(paste0("No 'n' argument was given and number of factors (",
               n_fact,
               ") used in EFA was determined using parallel analysis\n\n",
               "Check parallel analysis Skree plot using 'plot' function for more ",
               "details\n\n")
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

  low_communality <- communality < 0.2

  if (any(low_communality)) {
    lc_variables <- communality[low_communality]
    cat(paste("The following variables have communality values less than 0.2",
              "and should be considered for removal:\n")
    )
    print(lc_variables)
    cat("\n")
  }

  # Maybe add something saying the communality values are fine, but you cna look
  # at them more carefully?

  # We want variables with a high communality that contribute strongly to the
  # common factors

  # Do we want to add other stuff from the EFA output?



}
