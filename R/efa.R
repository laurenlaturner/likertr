# EFA
  # Polychoric Correlation Matrix
  # Bartlett’s Test of Sphericity
  # Kaiser-Meyer-Olkin (KMO)

  # Functions from other packages:
  # psych::KMO()
  # psych::cortest.bartlett

# Diagnostics
# - Sphericity
# - KMO

# Diagnostic Plots
# - Polychoric Correlation Matrix
# - Skree plot

# Number of factors
# - We'll have an option to have this decided automatically,
# but also have an option to manually decide the number of factors

# Output
# - Spit out diagnostics values
# - Do the EFA
# - Spit out loadings and also spit out variance explained (like fa function)

########################### Main function ######################################

efa <- function(data, n) {
  sphericity_results <- sphericity(data)
  kmo_results <- kmo(data)
  pm_results <- polychoric_matrix(data)
  pa_results <- pa(data)

  # Variable that represents whether the user gave a number of factors or not
  user_n_fact <- !missing(n)

  # If n is missing, we use an estimate of which one's the best

  if (missing(n)) {
    n = pa_results$rec_n_fact
  }


  pre_efa_diagnostics <- list(sphericity = sphericity_results,
                              kmo = kmo_results,
                              pa = pa_results,
                              pc_matrix = pm_results)


  efa_results <- run_efa(data, n, user_n_fact)


  total_results <- list(pre_efa_diagnostics = pre_efa_diagnostics,
                        efa_results = efa_results)

  # Values that will be needed for plotting later
  # plotting <- list(pa_results$fa_real,
  #                  pa_results$fa_sim,
  #                  pa_results$fa_resamp
  #                  pc_matrix)

  total_results
}


################# Smaller helper functions ############################################


sphericity <- function(data) {
  # What form will the data be in?
  # Probably a dataframe

  n <- nrow(data)
  data <- cor(data, use = "pairwise")
  p <- dim(data)[2]


  # if(diag) ?
  diag(data) <- 1   #this will make tests of factor residuals correct
  det <- det(data)
  statistic  <- -log(det) *(n -1 - (2*p + 5)/6)
  df <- p * (p-1)/2
  pval <- pchisq(statistic,df,lower.tail=FALSE)

  bartlett <- list(chi_squared = statistic, p_value = pval, df = df)
  bartlett

}

kmo <- function(data) {
  # Checks sampling adequacy
  # Summary gives a warning if it is below 0.6 (default acceptable minimum value)

  data <- cor(data,use="pairwise")
  Q <- try(solve(data))
  if(inherits(Q,  as.character("try-error")))  {message("matrix is not invertible, image not found")
    Q <- data}
  #from the original paper
  S2  <- diag(1/diag(Q))
  S <- sqrt(S2)
  IC <- S %*% Q %*% S   #from the kaiser paper

  Q <- Image <-  cov2cor(Q) #ANOTHER WAY OF FINDING Q
  diag(Q) <- 0
  diag(data) <- 0
  sumQ2 <- sum(Q^2)
  sumr2 <- sum(data^2)
  MSA <- sumr2/(sumr2 + sumQ2)
  MSAi <- colSums(data^2)/(colSums(data^2) + colSums(Q^2))
  results <- list(MSA = MSA, MSAi = MSAi) # , Image=Image,ImCov = IC,Call=cl)
  results
}


# instead of correlations of responses, correlations of the underlying continuous
# variables (true anxiety, true stress. etc.)

polychoric_matrix <- function(data) {
  matrix <- psych::polychoric(data)

  # list(polychoric_matrix = matrix$rho)
  matrix$rho
}



pa <- function(data) {
  # Supress plot, output, and warnings
  pdf(file = tempfile())
  invisible(
    capture.output(
      pa <- suppressWarnings(
        psych::fa.parallel(data, fm = "minres", fa = "fa")
        )
    )
  )
  dev.off()

  list(rec_n_fact = pa$nfact,
       fa_real = pa$fa.values,
       fa_sim = pa$fa.sim,
       fa_resamp = pa$fa.simr)

  # Don't show warnings, but give them a way to look at them??
  # Actually, pa warnings don't really matter a ton
  # A suppress warning option on the overall likertr function is also a good idea
}


run_efa <- function(data, n_fact, user_n_fact) {
  # Do we want to suppress warnings on this??


  efa <- suppressWarnings(
    psych::fa(before, nfactors = n_fact, rotate = "oblimin", fm= "minres")
  )

  # Maybe give a cutoff option for the loadings?
  # What is a good default?

  loadings <- efa$loadings[,]
  # When we print these, we'll only show above a certain level
  # Should we only use above a certain level?
  # For McDonald's Omega as well?

  if (n_fact == 1) {
    var_exp <- efa$Vaccounted[2,]
  } else {
    var_exp <- efa$Vaccounted[2:3,]
  }


  fc_matrix <- efa$Phi


  # Recommend to get rid of variables with a communality < 0.2
  # in future runs of the likertr workflow

  # We want variables with a high communality that contribute strongly to the
  # common factors
  communality <- efa$communality

  # Check for Ultra Heywood case (communality above 1)

  ultra_heywood <- communality > 1
  heywood <- communality >= 1

  if (any(ultra_heywood)) {
    warning(paste("Communality values above 1 indicate an Ultra-Heywood case,",
              "which means that the EFA generated mathematically invalid",
              "results\n\nSome causes may be:\n",
              "- Too many factors\n",
              "- Low sample size\n",
              "- Very high multicollinearity"),
            call. = FALSE)
  } else if (any(heywood)) {
    warning(paste("Communality values above 1 indicate a Heywood case,",
                  "which means that the EFA generated mathematically invalid",
                  "results\n\nSome causes may be:\n",
                  "- Too many factors\n",
                  "- Low sample size\n",
                  "- Very high multicollinearity"),
            call. = FALSE)
  }

  # Measures of fit
  RMSEA <- efa$RMSEA[1]
  TLI <- efa$TLI
  CFI <- efa$CFI


  list(loadings = loadings,
       var_exp = var_exp,
       communality  = communality,
       n_fact = n_fact,
       user_n_fact = user_n_fact,
       fc_matrix = fc_matrix,
       RMSEA = RMSEA,
       TLI = TLI,
       CFI = CFI)
}

