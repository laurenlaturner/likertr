# Inference and Reporting
  # Nonparametric testing with Mann-Whitney U or Kruskal-Wallis
  # Effect size calculation (Cliff’s Delta or r)

inference <- function(data, variable_group, factor_var = NA, variable_group2 = NA) {
  if (nlevels(factor_variable) == 2) {
    test <- "wilcox"
    wilcox <- wilcox_sum(data, factor_var, variable_group)
    cat("Mann Whitney U-Test for Two Independent Samples",
        paste("Factor Variable:", colnames(data[factor_var])),
        paste("Likert Variable:", colnames(data[variable_group])),
        paste("p-value:", wilcox$p.value),
        sep = "\n")
    print(cat("p-value:", as.character(wilcox$p.value)))
  }
}

wilcox_sum <- function(data, factor_var, likert_var) {
  # Create dataframe with factor variable and sum
  wilcox_data <- data.frame(fact = data[[factor_var]],
                            likert = rowSums(data[, likert_var]))
  wilcox_data$fact <- as.factor(wilcox_data$fact)

  wilcox <- wilcox.test(likert ~ fact,
              data = wilcox_data,
              exact = TRUE)
}
