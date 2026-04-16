# For generic usage of plot() with "likertr" class

# Diverging stacked bar charts
# Confidence Intervals on bar charts to show how "extreme responders" affect mean
# EFA - Heat map for polychloric correlations
# Comparison plots (group to group)
# Ridge Plots to show density of responses
# EFA Skree plot

#' Plot Method for Likertr Objects
#' 
#' @description A S3 method for objects of class \code{likertr}. 
#'   This function automatically generates a set of visualizations.
#' 
#' @param likertr An object of class \code{likertr}, typically the output from 
#'   the \code{preparation} function
#' @param ... Additional arguments passed to the underlying plotting functions.
#' 
#' @export
plot.likertr <- function(likertr, ...) {
  data <- attr(likertr, "data")

  stacked_bar(data[[6]], data[[2]])
  diverging_bar(data[[6]], data[[2]])
  ridge_plot(data[[1]], data[[2]])
}

#' Align the data for graphing
#' @description Processes a list of Likert scale percentages to ensure they are centered and 
#'   aligned for visualization. The function pads varying response lengths to a 
#'   uniform width and calculates the necessary horizontal offset for diverging 
#'   stacked bar charts. This ensures that the "neutral" category (or the center 
#'   of the scale) is positioned at a consistent zero-point across different questions.
#' 
#' @param perc_by_question A list of numeric vectors, where each vector represents 
#'   the percentage distribution of responses for a single question.
#' 
#' @return A list containing:
#' \itemize{
#'   \item \strong{matrix}: A numeric matrix where each column is a padded question vector.
#'   \item \strong{offsets}: A numeric vector of calculated offsets used to shift the bars.
#'   \item \strong{max_w}: The maximum number of response categories found across all questions.
#'   \item \strong{mid_idx}: The index identified as the midpoint/neutral category.
#' }
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

#' Standard Stacked Bar Chart for Likert Data
#' 
#' @description 
#' Generates a horizontal stacked bar chart where each bar represents a question 
#' and the segments represent the percentage distribution of responses. This 
#' visualization uses a color gradient to distinguish between response levels 
#' and places a legend on the left side of the plot.
#' 
#' @param perc_by_question A list of numeric vectors containing percentage 
#' distributions for each question.
#' @param questions A character vector of labels/questions to display on the y-axis.
stacked_bar <- function(perc_by_question, questions) {
  prep <- align_likert_data(perc_by_question)

  # Generate colors
  cols <- colorRampPalette(c("#D7191C", "#FFFFBF", "#2C7BB6"))(prep$max_w)

  par(mar = c(5, 15, 5, 2), xpd = TRUE)
  barplot(prep$matrix, horiz = TRUE, col = cols, names.arg = questions,
          las = 1, xlab = "Percentage (%)")

  legend("left",
         legend = 1:prep$max_w,
         fill = cols,
         horiz = FALSE,
         inset = c(-0.45, 0),
         bty = "n",
         title = "Response Scale")
}

#' Diverging Bar Chart for Likert Data
#' @description Generates a centered Likert scale visualization. Unlike a standard stacked 
#'   bar, this function uses calculated offsets to align the neutral midpoint of 
#'   each question at zero. This makes it easier to compare the "lean" of 
#'   responses toward the positive or negative ends of the scale.
#' 
#' @param perc_by_question A list of numeric vectors containing percentage 
#'   distributions for each question.
#' @param questions A character vector of labels/questions to display on the y-axis.
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

  legend("right",
         legend = 1:prep$max_w,
         fill = cols,
         horiz = FALSE,
         inset = c(-0.15, 0),
         bty = "n",
         title = "Response Scale")
}

#' A Ridgeline Density Plot for Likert Data
#' 
#' @description Visualizes the distribution of Likert responses using overlapping density 
#'   plots. This is particularly useful for seeing the "shape" of 
#'   the data and identifying whether responses are unimodal, bimodal, or 
#'   skewed across different questions.
#' 
#' @param clean_data A data frame or list containing the raw numeric response values.
#' @param questions A character vector of labels/questions to display on the y-axis.
#'
ridge_plot <- function(clean_data, questions) {
  n <- ncol(clean_data)
  overlap <- 1.6

  global_min <- min(clean_data, na.rm = TRUE)
  global_max <- max(clean_data, na.rm = TRUE)

  cols <- colorRampPalette(c("#D7191C", "#FFFFBF", "#2C7BB6"))(n)

  densities <- lapply(1:n, function(i) {
    density(clean_data[[i]], from = global_min, to = global_max, na.rm = TRUE, bw = 0.4)
  })

  max_h <- max(sapply(densities, function(d) max(d$y)))

  par(mar = c(5, 12, 4, 2) + 0.1)

  plot(NULL,
       xlim = c(global_min, global_max),
       ylim = c(1, n + overlap),
       type = "n", yaxt = "n", bty = "n",
       xlab = "Response Value", ylab = "",
       main = "Response Density by Question")

  axis(2, at = 1:n, labels = questions, las = 1, cex.axis = 0.8, tick = FALSE)

  for (i in n:1) {
    d <- densities[[i]]

    y_vals <- (d$y / max_h) * overlap + i

    polygon(d$x, y_vals,
            col = adjustcolor(cols[i], alpha.f = 0.7),
            border = "white", lwd = 0.5)

    lines(d$x, y_vals, col = "black", lwd = 1)

    abline(h = i, col = "gray90", lwd = 0.5)
  }
}
