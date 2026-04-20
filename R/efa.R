########################### Main function ######################################

#' Carry out exploratory factor analysis
#'
#' @description `efa()` carries out exploratory factor analysis on a dataset,
#'     returning pre-EFA diagnostics as well as various EFA results
#'
#' @param data A dataset in the form of a data.frame that will be used for the
#'     analysis
#' @param n An optional integer argument indicating how many factors should be
#'     used in the analysis
#'
#' @returns A list containing smaller lists of pre-EFA diagnostics and EFA
#'     results, which both contain several measures
efa <- function(data, n) {

  sphericity_results <- sphericity(data)
  kmo_results <- kmo(data)
  pcm_results <- polychoric_matrix(data)
  pa_results <- pa(data)

  # Variable that represents whether the user gave a number of factors or not
  user_n_fact <- !missing(n)

  # If n is missing, we use an estimate of which one's the best

  if (missing(n)) {
    n <- pa_results$rec_n_fact
  }


  pre_efa_diagnostics <- list(
    sphericity = sphericity_results,
    kmo = kmo_results,
    pa = pa_results,
    pc_matrix = pcm_results
  )


  efa_results <- run_efa(data, n, user_n_fact)


  total_results <- list(
    pre_efa_diagnostics = pre_efa_diagnostics,
    efa_results = efa_results
  )

  # Values that will be needed for plotting later
  # plotting <- list(pa_results$fa_real,
  #                  pa_results$fa_sim,
  #                  pa_results$fa_resamp
  #                  pc_matrix)

  total_results
}


################# Smaller helper functions #####################################


sphericity <- function(data) {
  # If this test results in a non-significant value, it indicates that the
  # variables are not correlated enough for an EFA (indicated by summary
  # function)



  n <- nrow(data)
  data <- cor(data, use = "pairwise")
  p <- dim(data)[2]


  diag(data) <- 1 # this will make tests of factor residuals correct
  det <- det(data)
  statistic <- -log(det) * (n - 1 - (2 * p + 5) / 6)
  df <- p * (p - 1) / 2
  pval <- pchisq(statistic, df, lower.tail = FALSE)

  bartlett <- list(chi_squared = statistic, p_value = pval, df = df)
  bartlett
}

kmo <- function(data) {
  # Checks sampling adequacy
  # Summary gives a warning if any variable's value is below 0.6
  # (default acceptable minimum value)

  data <- cor(data, use = "pairwise")
  q <- try(solve(data))
  if (inherits(q, as.character("try-error"))) {
    message(paste(
      "Matrix is not",
      "invertible, image not found"
    ))
    q <- data
  }

  q <- cov2cor(q)
  diag(q) <- 0
  diag(data) <- 0
  sum_q2 <- sum(q^2)
  sumr2 <- sum(data^2)
  msa <- sumr2 / (sumr2 + sum_q2)
  msai <- colSums(data^2) / (colSums(data^2) + colSums(q^2))
  results <- list(MSA = msa, MSAi = msai)
  results
}


#' @importFrom psych polychoric
polychoric_matrix <- function(data) {
  # Instead of correlations of exact responses, this function returns
  # correlations of the underlying continuous variables (true anxiety, true
  # stress. etc.)

  matrix <- psych::polychoric(data)

  matrix$rho
}


#' @importFrom psych fa.parallel
pa <- function(data) {
  # Parallel analysis will give us a recommended number of factors to use for
  # EFA, as well as the necessary values for a scree plot that users can look
  # at further

  # Suppress plot, output, and warnings from fa.parallel function
  pdf(file = tempfile())
  invisible(
    capture.output(
      pa <- suppressWarnings(
        psych::fa.parallel(data, fm = "minres", fa = "fa")
      )
    )
  )
  dev.off()

  list(
    rec_n_fact = pa$nfact,
    fa_real = pa$fa.values,
    fa_sim = pa$fa.sim,
    fa_resamp = pa$fa.simr
  )
}


#' @importFrom psych fa
run_efa <- function(data, n_fact, user_n_fact) {
  efa <- suppressWarnings(
    psych::fa(data, nfactors = n_fact, rotate = "oblimin", fm = "minres")
  )

  # Factor loadings
  loadings <- efa$loadings[, ]

  # Variance explained
  if (n_fact == 1) {
    var_exp <- efa$Vaccounted[2, ]
  } else {
    var_exp <- efa$Vaccounted[2:3, ]
  }


  # Factor correlation matrix
  fc_matrix <- efa$Phi


  # Summary function recommends to get rid of variables with communality < 0.2

  # We want variables with a high communality that contribute strongly to the
  # common factors.
  communality <- efa$communality

  # Check for Ultra Heywood case (communality above 1)

  ultra_heywood <- communality > 1
  heywood <- communality >= 1

  if (any(ultra_heywood)) {
    warning(
      paste(
        "Communality values above 1 indicate an Ultra-Heywood case,",
        "which means that the EFA generated mathematically invalid",
        "results\n\nSome causes may be:\n",
        "- Too many factors\n",
        "- Low sample size\n",
        "- Very high multicollinearity"
      ),
      call. = FALSE
    )
  } else if (any(heywood)) {
    warning(
      paste(
        "Communality values above 1 indicate a Heywood case,",
        "which means that the EFA generated mathematically invalid",
        "results\n\nSome causes may be:\n",
        "- Too many factors\n",
        "- Low sample size\n",
        "- Very high multicollinearity"
      ),
      call. = FALSE
    )
  }

  # Measures of fit
  rmsea <- efa$RMSEA[1]
  attributes(rmsea) <- NULL
  tli <- efa$TLI
  cfi <- efa$CFI


  list(
    loadings = loadings,
    var_exp = var_exp,
    communality = communality,
    n_fact = n_fact,
    user_n_fact = user_n_fact,
    fc_matrix = fc_matrix,
    RMSEA = rmsea,
    TLI = tli,
    CFI = cfi
  )
}
