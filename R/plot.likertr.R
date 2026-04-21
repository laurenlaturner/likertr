# For generic usage of plot() with "likertr" class

# Diverging stacked bar charts
# Confidence Intervals on bar charts to show how
#     "extreme responders" affect mean
# EFA - Heat map for polychoric correlations
# Comparison plots (group to group)
# Ridge Plots to show density of responses
# EFA Skree plot

#' Plot Method for Likertr Objects
#'
#' @description A S3 method for objects of class \code{likertr}.
#'   This function automatically generates a set of visualizations.
#'
#' @param x An object of class \code{likertr}, typically the output from
#'   the \code{likertr} function
#' @param ... Additional arguments passed to the underlying plotting functions.
#'
#' @importFrom grDevices adjustcolor colorRampPalette dev.off pdf
#' @importFrom graphics abline axis barplot image legend lines par polygon text title
#' @importFrom stats cor cov2cor density kruskal.test pchisq wilcox.test
#' @importFrom utils capture.output
#' @export
plot.likertr <- function(x, ...) {
  clean_data_list <- x
  pc <- attr(x, "pre_efa_diagnostics")[[4]]
  pa <- attr(x, "pre_efa_diagnostics")[["pa"]]
  fa_real <- pa[["fa_real"]]
  fa_sim <- pa[["fa_sim"]]
  fa_resamp <- pa[["fa_resamp"]]


  stacked_bar(clean_data_list[[6]], clean_data_list[[2]])
  diverging_bar(clean_data_list[[6]], clean_data_list[[2]])
  ridge_plot(clean_data_list[[1]], clean_data_list[[2]])
  efa_heat_map(pc)
  pa_skree_plot(fa_real, fa_sim, fa_resamp)
}

#' Align the data for graphing
#' @description Processes a list of Likert scale percentages to ensure they are
#'   centered and aligned for visualization. The function pads varying response
#'   lengths to a uniform width and calculates the necessary horizontal offset
#'   for diverging stacked bar charts. This ensures that the "neutral" category
#'   (or the center of the scale) is positioned at a consistent zero-point
#'   across different questions.
#'
#' @param perc_by_question A list of numeric vectors, where each vector
#'   represents the percentage distribution of responses for a single question.
#'
#' @return A list containing:
#' \itemize{
#'   \item \strong{matrix}: A numeric matrix where each column is a padded
#'       question vector.
#'   \item \strong{offsets}: A numeric vector of calculated offsets used to
#'       shift the bars.
#'   \item \strong{max_w}: The maximum number of response categories found
#'       across all questions.
#'   \item \strong{mid_idx}: The index identified as the midpoint/neutral
#'       category.
#' }
align_likert_data <- function(perc_by_question) {
  widths <- sapply(perc_by_question, length)
  max_w <- max(widths)
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

    list(vec = combined, offset = offset)
  })

  # Simplify into a matrix and a vector of offsets
  matrix_data <- do.call(cbind, lapply(processed, `[[`, "vec"))
  offsets <- sapply(processed, `[[`, "offset")

  list(
    matrix = matrix_data,
    offsets = offsets,
    max_w = max_w,
    mid_idx = mid_idx
  )
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
#'     distributions for each question.
#' @param questions A character vector of labels/questions to display on the
#'     y-axis.
stacked_bar <- function(perc_by_question, questions) {
  prep <- align_likert_data(perc_by_question)

  # Generate colors
  cols <- colorRampPalette(c("#D7191C", "#FFFFBF", "#2C7BB6"))(prep$max_w)

  par(mar = c(5, 16, 5, 2), xpd = TRUE)
  barplot(prep$matrix,
    horiz = TRUE, col = cols, names.arg = questions,
    las = 1, xlab = "Percentage (%)",
    cex.names = 0.58
  )

  legend("topright",
    legend = 1:prep$max_w,
    fill = cols,
    horiz = FALSE,
    inset = c(-0.08, -0.31),
    bty = "n",
    title = "Response Scale",
    cex = 0.8,
    xpd = TRUE
  )
}

#' Diverging Bar Chart for Likert Data
#' @description Generates a centered Likert scale visualization. Unlike a
#'   standard stacked bar, this function uses calculated offsets to align the
#'   neutral midpoint of each question at zero. This makes it easier to compare
#'   the "lean" of responses toward the positive or negative ends of the scale.
#'
#' @param perc_by_question A list of numeric vectors containing percentage
#'   distributions for each question.
#' @param questions A character vector of labels/questions to display on the
#'   y-axis.
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
    xlab = "Negative <--- Neutral ---> Positive",
    cex.names = 0.58
  )

  abline(v = 0, lty = 2, col = "gray40")

  legend("right",
    legend = 1:prep$max_w,
    fill = cols,
    horiz = FALSE,
    inset = c(-0.2, 0),
    bty = "n",
    title = "Response Scale"
  )
}

#' A Ridgeline Density Plot for Likert Data
#'
#' @description Visualizes the distribution of Likert responses using
#'   overlapping density plots. This is particularly useful for seeing the
#'   "shape" of the data and identifying whether responses are unimodal,
#'   bimodal, or skewed across different questions.
#'
#' @param clean_data A data frame or list containing the raw numeric response
#'   values.
#' @param questions A character vector of labels/questions to display on the
#'   y-axis.
#'
ridge_plot <- function(clean_data, questions) {
  n <- ncol(clean_data)
  overlap <- 1.6

  global_min <- min(clean_data, na.rm = TRUE)
  global_max <- max(clean_data, na.rm = TRUE)

  cols <- colorRampPalette(c("#D7191C", "#FFFFBF", "#2C7BB6"))(n)

  densities <- lapply(1:n, function(i) {
    density(
            clean_data[[i]],
            from = global_min,
            to = global_max,
            na.rm = TRUE,
            bw = 0.4)
  })

  max_h <- max(sapply(densities, function(d) max(d$y)))

  par(mar = c(5, 14, 4, 2) + 0.1)

  plot(NULL,
    xlim = c(global_min, global_max),
    ylim = c(1, n + overlap),
    type = "n", yaxt = "n", bty = "n",
    xlab = "Response Value", ylab = "",
    main = "Response Density by Question"
    )


  for (i in n:1) {
    d <- densities[[i]]

    y_vals <- (d$y / max_h) * overlap*0.9 + i
    polygon(d$x, y_vals,
      col = adjustcolor(cols[i], alpha.f = 0.7),
      border = "white", lwd = 0.5
    )

    lines(d$x, y_vals, col = "black", lwd = 1)

    #abline(h = i, col = "gray90", lwd = 0.5)
  }
  axis(2, at = 1:n +1, labels = questions, cex.axis = 0.5, las = 1, tick = FALSE)

}

#' Heat Map for the EFA Polychoric Correlation Matrix
#'
#' @description Generates a color-coded visual representation of a polychoric
#'   correlation matrix using base R graphics. The function maps correlation
#'   values to a red-white-blue color palette, where red indicates negative
#'   correlations, white indicates zero, and blue indicates positive
#'   correlations. It also overlays the numerical correlation coefficients on
#'   the plot.
#'
#' @param pc A square, symmetric numeric matrix of polychoric correlations.
#'   The matrix must have row and column names for axis labeling.
#'
#' @param pc A square matrix of polychoric correlations.
efa_heat_map <- function(pc) {
  col_palette <- colorRampPalette(c("red", "white", "blue"))(100)
  par(mar = c(2, 4, 4, 2) + 0.1)
  # Transpose and reverse the matrix
  # (image() plots columns as rows and starts from the bottom-left)
  plot_data <- t(pc[rev(seq_len(nrow(pc))), ])

  image(seq_len(ncol(pc)), seq_len(nrow(pc)), plot_data,
    col = col_palette,
    breaks = seq(-1, 1, length.out = 101),
    axes = FALSE,
    xlab = "", ylab = ""
  )

  axis(
    3,
    at = seq_len(ncol(pc)),
    labels = colnames(pc),
    las = 2,
    cex.axis = 0.8
  )
  axis(
    2,
    at = seq_len(nrow(pc)),
    labels = rev(rownames(pc)),
    las = 1,
    cex.axis = 0.8
  )
  title("Polychoric Correlation Matrix", line = 3)

  for (x in seq_len(ncol(pc))) {
    for (y in seq_len(nrow(pc))) {
      val <- pc[nrow(pc) - y + 1, x]
      text(x, y, round(val, 2), cex = 0.7)
    }
  }
}


pa_skree_plot <- function(fa_real, fa_sim, fa_resamp) {
  idx <- seq_len(length(fa_real))
  par(mar = c(5, 4, 4, 2) + 0.1)
  plot(idx, fa_real,
    type = "o",
    col = "blue",
    lwd = 2,
    pch = 16,
    main = "Parallel Analysis Scree Plot",
    xlab = "Number of Factors",
    ylab = "Eigenvalues"
    )

  lines(idx, fa_sim,
    lwd = 2,
    col = "red"
  )

  lines(idx, fa_resamp,
    lwd = 2,
    col = "#00DE64"
  )

  legend("topright",
    legend = c("Actual Data", "Simulated Data", "Resampled Data"),
    col = c("blue", "red", "#00DE64"),
    pch = c(16, NA, NA),
    lwd = 2,
    inset = 0.05,
    bty = "n"
  )
}
