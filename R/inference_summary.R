inference_summary <- function(object) {
  cat("================================================\n")
  cat("Inference\n")
  cat("================================================\n\n")

  test <- attributes(object)$test
  effect_size <- attributes(object)$effect_size

  if (test == "wilcox") {
    cat(
      "Mann Whitney U-Test for Two Independent Samples\n",
      paste("p-value:", effect_size),
    )
  } else if (test == "Kruskal Wallis") {
    cat(
      "Kruskal-Wallis Rank Sum Test\n",
      paste("p-value:", effect_size)
    )
  } else {
    cat("No Variables were provided for inference")
  }
}
