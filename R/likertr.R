# This is the overall wrapper function that will run everything

likertr <- function(data) {
  # Preparation and Cleaning
  clean_data <- preparation(data)

  # Reliability and Structure
  # reliability(clean_data)

  # EFA
  # efa(data)

  # Inference and Reporting
  # inference(clean_data)

  new_likertr(clean_data)
}