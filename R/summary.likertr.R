# For generic usage of summary() with "likertr" class

summary.likertr <- function(likertr, ...) {
  data <- attr(likertr, "data")
  n_q <- data[[3]]
  n_obs <- max(data[[4]])

  cat("================================================\n")
  cat("LIKERTR OBJECT SUMMARY REPORT\n")
  cat("================================================\n")
  cat(paste0("This dataset contains ", n_q, " questions with ", n_obs, " total observations.\n"))

  cat("\n\n")
  cat("================================================\n")
  cat("Exploratory Factor Analysis\n")
  cat("================================================\n")

}