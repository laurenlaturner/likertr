# Inference and Reporting
  # Nonparametric testing with Mann-Whitney U or Kruskal-Wallis
  # Effect size calculation (Cliff’s Delta or r)


inference <- function(data, variable_group, factor_var = NA, variable_group2 = NA) {
  if (is.na(factor_var) == FALSE) {
    if (nlevels(factor_var) == 2) {
      test <- "wilcox"
      wilcox <- test_wilcox(data, factor_var, variable_group)
      cat("Mann Whitney U-Test for Two Independent Samples",
          paste("Factor Variable:", colnames(data[factor_var])),
          paste("Likert Variable:", colnames(data[variable_group])),
          paste("p-value:", wilcox$p.value),
          sep = "\n")
      print(cat("p-value:", as.character(wilcox$p.value)))
    } else if (nlevels(factor_var) > 2) {
      test <- "Kruskal Wallis"
      kruskal <- test_kruskal(data, factor_var, variable_group)
      cat("Kruskal-Wallis Rank Sum Test",
          paste("Factor Variable:", colnames(data[factor_var])),
          paste("Likert Variable:", colnames(data[variable_group])),
          paste("p-value:", kruskal$p.value),
          sep = "\n")
    }
  }

}


test_wilcox <- function(data, factor_var, likert_var) {
  # Create dataframe with factor variable and sum
  wilcox_data <- data.frame(fact = data[[factor_var]],
                            likert = rowSums(data[, likert_var]))
  wilcox_data$fact <- as.factor(wilcox_data$fact)

  wilcox <- wilcox.test(likert ~ fact,
              data = wilcox_data,
              exact = TRUE)
}

test_kruskal <- function(data, factor_var, likert_var) {
  kruskal_data <- data.frame(fact = data[[factor_var]],
                             likert = rowSums(data[, likert_var]))
  kruskal_data$fact <- as.factor(kruskal_data$fact)

  kruskal <- kruskal.test(likert ~ fact,
                          data = kruskal_data)
}


