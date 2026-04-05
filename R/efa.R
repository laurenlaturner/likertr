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

efa <- function(data) {
  sphericity_results <- sphericity(data)
  kmo_results <- kmo(data)
}


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

  bartlett <- list(chisq = statistic, p.value =pval, df =df)
  bartlett

}

kmo <- function(data) {
  # Checks sampling adequacy
  # Give a warning if it is below 0.6 (default acceptable minimum value)

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
  results <- list(MSA =MSA,MSAi = MSAi) # , Image=Image,ImCov = IC,Call=cl)
  results
}

# sphericity(before)
# kmo(before)

