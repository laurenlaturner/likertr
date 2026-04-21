# Inference and Reporting
# Nonparametric testing with Mann-Whitney U or Kruskal-Wallis
# Effect size calculation (Cliff’s Delta or r)


inference <- function(data, category, groups) {
  test <- "None"
  effect_size <- "None"

  if (is.na(category) == FALSE) {
    if (nlevels(as.factor(data[[category]])) == 2) {
      test <- "wilcox"
      wilcox <- test_wilcox(data, category, which(groups == 1))
      effect_size <- wilcox$p.value
    } else if (nlevels(as.factor(data[[category]])) > 2) {
      test <- "Kruskal Wallis"
      kruskal <- test_kruskal(data, category, which(groups == 1))
      effect_size <- kruskal$p.value
    }
  }
  list("test" = test, "effect_size" = effect_size)
}


test_wilcox <- function(data, factor_var, inference_variables) {
  # Create dataframe with factor variable and sum
  wilcox_data <- data.frame(
    fact = data[[factor_var]],
    likert = rowSums(data[, inference_variables])
  )
  wilcox_data$fact <- as.factor(wilcox_data$fact)

  stats::wilcox.test(likert ~ fact, data = wilcox_data, exact = FALSE)
}

test_kruskal <- function(data, factor_var, inference_variables) {
  kruskal_data <- data.frame(
    fact = data[[factor_var]],
    likert = rowSums(data[, inference_variables])
  )
  kruskal_data$fact <- as.factor(kruskal_data$fact)

  stats::kruskal.test(likert ~ fact, data = kruskal_data)
}
