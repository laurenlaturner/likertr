# Inference and Reporting
# Nonparametric testing with Mann-Whitney U or Kruskal-Wallis
# Effect size calculation (Cliff’s Delta or r)


inference <- function(data,
                      inference_variables,
                      factor_var = NA,
                      variable_group2 = NA) {
  if (is.na(factor_var) == FALSE) {
    if (nlevels(factor_var) == 2) {
      test <- "wilcox"
      wilcox <- test_wilcox(data, factor_var, inference_variables)
      effect_size <- wilcox$p.value
      cat("Mann Whitney U-Test for Two Independent Samples",
        paste("Factor Variable:", colnames(data[factor_var])),
        paste("Likert Variable:", colnames(data[inference_variables])),
        paste("p-value:", wilcox$p.value),
        sep = "\n"
      )
      print(cat("p-value:", as.character(wilcox$p.value)))
    } else if (nlevels(factor_var) > 2) {
      test <- "Kruskal Wallis"
      kruskal <- test_kruskal(data, factor_var, inference_variables)
      cat("Kruskal-Wallis Rank Sum Test",
        paste("Factor Variable:", colnames(data[factor_var])),
        paste("Likert Variable:", colnames(data[inference_variables])),
        paste("p-value:", kruskal$p.value),
        sep = "\n"
      )
    } else {
      test <- "None"
      effect_size <- "None"
    }
  }
  list("test" = test, "effect_size" = effect_size)
}


test_wilcox <- function(data, factor_var, inference_variables) {
  # Create dataframe with factor variable and sum
  wilcox_data <- data.frame(fact = data[[factor_var]],
                            likert = rowSums(data[, inference_variables]))
  wilcox_data$fact <- as.factor(wilcox_data$fact)

  wilcox.test(likert ~ fact, data = wilcox_data, exact = FALSE)
}

test_kruskal <- function(data, factor_var, inference_variables) {
  kruskal_data <- data.frame(
    fact = data[[factor_var]],
    likert = rowSums(data[, inference_variables])
  )
  kruskal_data$fact <- as.factor(kruskal_data$fact)

  kruskal.test(likert ~ fact, data = kruskal_data)
}

inference(data, factor_var = 1, inference_variables = 3:25)
