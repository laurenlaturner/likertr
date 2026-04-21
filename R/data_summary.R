data_summary <- function(object) {
  data <- object
  n_q <- data[[3]]
  n_obs <- max(data[[4]])

  cat(paste0(
    "This dataset contains ", n_q, " questions with ", n_obs,
    " total observations.\n\n"
  ))
}
