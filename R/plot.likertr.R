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

regular_bar <- function(perc_by_question, questions) {
  # Needs work because of possible 1-7 and 1-5 likert
  barplot(perc_by_question,
          horiz = TRUE,
          las = 1,
          beside = FALSE,
          xlab = "Percentage (%)", 
          names.arg = questions,
          col = c("#D7191C", "#FDAE61", "#FFFFBF", "#ABD9E9", "#2C7BB6"),
  )
}
