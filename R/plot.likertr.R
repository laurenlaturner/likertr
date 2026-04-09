# For generic usage of plot() with "likertr" class

# Diverging stacked bar charts
# Confidence Intervals on bar charts to show how "extreme responders" affect mean
# Heat maps for correlations
# Comparison plots (group to group)
# Ridge Plots to show density of responses
# EFA Skree plot
# EFA Polychloric correlation matrix

plot.likertr <- function(likertr, ...) {

}

stacked_bar <- function(perc_by_question, questions) {
  widths <- sapply(perc_by_question, length)
  max_w  <- max(widths)
  
  aligned_data <- sapply(perc_by_question, function(x) {
    len <- length(x)
    if (len == max_w) return(as.numeric(x))
    
    left_pad <- (max_w - len) / 2
    combined <- rep(0, max_w)
    
    combined[(left_pad + 1):(left_pad + len)] <- x
    return(combined)
  })

  base_colors <- c("#D7191C", "#FDAE61", "#FFFFBF", "#ABD9E9", "#2C7BB6")
  dynamic_cols <- colorRampPalette(base_colors)(max_w)

  par(xpd = TRUE)

  barplot(aligned_data,
          horiz = TRUE,
          las = 1,
          beside = FALSE,
          xlab = "Percentage (%)", 
          names.arg = questions,
          col = dynamic_cols,
          border = "white")
  
  par(xpd = FALSE)
}
