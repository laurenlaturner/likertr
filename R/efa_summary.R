efa_summary <- function(object) {
  cat("================================================\n")
  cat("Pre-EFA (Exploratory Factor Analysis) Diagnostics\n")
  cat("================================================\n\n")

  sph_p_val <- attributes(object)$pre_efa_diagnostics$sphericity$p_value
  msai <- attributes(object)$pre_efa_diagnostics$kmo$MSAi
  user_n_fact <- attributes(object)$efa_results$user_n_fact
  n_fact <- attributes(object)$efa_results$n_fact

  invalid_kmo <- msai < 0.6

  cat(paste(
    "Bartlett's Test of Sphericity P-value:",
    sph_p_val,
    "\n"
  ))
  if (sph_p_val > 0.05) {
    cat(paste(
      "Non-significant (p<0.05) p-value indicates that variables may not be",
      "correlated enough to justify EFA\n\n"
    ))
  } else {
    cat(paste(
      "Significant (p<0.05) p-value does not show problematic results\n\n"
    ))
  }


  if (any(invalid_kmo)) {
    invalid_kmo <- msai[invalid_kmo]
    cat(paste(
      "At least one MSAi value from the KMO test is less than 0.6 and",
      "possibly problematic for EFA, the following feature or features share",
      "little variance with the rest of the data:\n"
    ))
    print(invalid_kmo)
    cat("\n")
  } else {
    cat(paste(
      "All MSAi values from the KMO test are greater than 0.6, so each",
      "variable shares sufficient variance with the rest of the data for EFA",
      "\n\n"
    ))
  }


  cat(paste(
    "Polychoric correlation matrix results can be viewed using the",
    "'plot' function\n - Lots of very low coefficients in the matrix",
    "indicate low factorability\n - Lots of very high loadings",
    "indicate redundant variables\n\n"
  ))


  cat("================================================\n")
  cat("EFA Results\n")
  cat("================================================\n\n")

  loadings <- round(attributes(object)$efa_results$loadings, 4)
  var_exp <- round(attributes(object)$efa_results$var_exp, 4)
  communality <- round(attributes(object)$efa_results$communality, 4)
  fc_matrix <- round(attributes(object)$efa_results$fc_matrix, 4)
  rmsea <- round(attributes(object)$efa_results$RMSEA, 4)
  tli <- round(attributes(object)$efa_results$TLI, 4)
  cfi <- round(attributes(object)$efa_results$CFI, 4)


  if (user_n_fact) {
    cat(paste0(
      "User-supplied number of factors (",
      n_fact,
      ") was used for EFA\n\n"
    ))
  } else {
    cat(paste0(
      "No 'n' argument was given and number of factors (",
      n_fact,
      ") used in EFA was determined\n using parallel analysis\n\n",
      "Check parallel analysis Scree plot using 'plot' function for ",
      "more details\n\n\n"
    ))
  }


  # EFA Loadings

  cat("Factor Loadings (Standardized):\n")
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
  cat("\n")

  low_communality <- communality < 0.2

  if (any(low_communality)) {
    lc_variables <- communality[low_communality]
    cat(paste(
      "The following variables have communality values less than 0.2,",
      "which means that very little of their variance is explained by",
      "the common factors and they should be considered for removal:\n"
    ))
    print(lc_variables)
    cat("\n\n")
  } else {
    cat(paste(
      "There are no variables with a communality less than 0.2,",
      "but it is reccommended to review the communality values that",
      "show how much of the variance of each variable is explained by",
      "the common factors\n\n\n"
    ))
  }

  # We want variables with a high communality that contribute strongly to the
  # common factors


  cat("Factor Correlation Matrix:\n")
  print(fc_matrix)
  cat("\n\n")

  cat("Measures of Fit:\n")
  cat(paste("RMSEA:", rmsea, "\n"))
  if (rmsea < 0.05) {
    cat("This RMSEA value indicates a good model fit")
  } else if (rmsea < 0.08) {
    cat("This RMSEA value indicates an acceptable model fit")
  } else if (rmsea < 0.1) {
    cat("This RMSEA value indicates a marginal model fit")
  } else {
    cat("This RMSEA value indicates a poor model fit")
  }
  cat("\n\n")

  cat(paste("TLI:", tli, "\n"))
  if (tli > 0.95) {
    cat("This TLI value indicates a good model fit")
  } else if (tli > 0.9) {
    cat("This TLI value indicates an acceptable model fit")
  } else {
    cat("This TLI value indicates a poor model fit")
  }
  cat("\n\n")

  cat(paste("CFI:", cfi, "\n"))
  if (cfi > 0.95) {
    cat("This CFI value indicates a good model fit")
  } else if (cfi > 0.9) {
    cat("This CFI value indicates an acceptable model fit")
  } else {
    cat("This CFI value indicates a poor model fit")
  }
  cat("\n\n")


  cat(paste(
    "Keep in mind that the interpretation of many of these statistics",
    "will depend on the context of your analysis\n\n"
  ))
}
