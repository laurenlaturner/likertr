# For generic usage of plot() with "likertr" class

# Diverging stacked bar charts
# Confidence Intervals on bar charts to show how "extreme responders" affect mean
# Heat maps for correlations
# Comparison plots (group to group)
# Ridge Plots to show density of responses
# EFA Skree plot
# EFA Polychloric correlation matrix

plot.likertr <- function(likertr, ...) {
  data <- attr(likertr, "data")
  
  stacked_bar(data[[6]], data[[2]])
  diverging_bar(data[[6]], data[[2]])
}

align_likert_data <- function(perc_by_question) {
  widths <- sapply(perc_by_question, length)
  max_w  <- max(widths)
  mid_idx <- ceiling(max_w / 2)
  
  # Process each question
  processed <- lapply(perc_by_question, function(x) {
    len <- length(x)
    left_pad <- (max_w - len) / 2
    
    # Create the padded vector
    combined <- rep(0, max_w)
    combined[(left_pad + 1):(left_pad + len)] <- x
    
    # Calculate offset (for diverging plots)
    # sum of everything left of neutral + half of neutral
    offset <- sum(combined[1:(mid_idx - 1)]) + (combined[mid_idx] / 2)
    
    return(list(vec = combined, offset = offset))
  })
  
  # Simplify into a matrix and a vector of offsets
  matrix_data <- do.call(cbind, lapply(processed, `[[`, "vec"))
  offsets <- sapply(processed, `[[`, "offset")
  
  return(list(
    matrix = matrix_data, 
    offsets = offsets, 
    max_w = max_w,
    mid_idx = mid_idx
  ))
}

stacked_bar <- function(perc_by_question, questions) {
  prep <- align_likert_data(perc_by_question)
  
  # Generate colors
  cols <- colorRampPalette(c("#D7191C", "#FFFFBF", "#2C7BB6"))(prep$max_w)
  
  par(mar = c(5, 15, 5, 2), xpd = TRUE)
  barplot(prep$matrix, horiz = TRUE, col = cols, names.arg = questions, 
          las = 1, xlab = "Percentage (%)")
}

diverging_bar <- function(perc_by_question, questions) {
  prep <- align_likert_data(perc_by_question)
  
  cols <- colorRampPalette(c("#D7191C", "#FFFFBF", "#2C7BB6"))(prep$max_w)
  
  par(mar = c(5, 15, 5, 2), xpd = TRUE)
  barplot(prep$matrix, 
          horiz = TRUE, 
          col = cols, 
          names.arg = questions, 
          las = 1, 
          offset = -prep$offsets,
          xlim = c(-100, 100),
          xlab = "Negative <--- Neutral ---> Positive")
  
  abline(v = 0, lty = 2, col = "gray40")
}