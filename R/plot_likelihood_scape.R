#' Speciation scape
#'
#' This is a function to visualize the speciation probability landscape in a heatmap across parameter space.
#'
#' @param data A data frame containing trajectories in speciation cube for all simulations (in rows).
#' @param paramspace The names of the two parameters to plot against.
#' @param dim The variable to use as a criterion for complete speciation.
#' @param threshold The threshold above which the value of the criterion variable means complete speciation
#' @param n How many of the last time points to average?
#' @param color_gradient Optional pair of colors for the two extremes of the gradient.
#' @export

# Function to plot speciation probability across parameter space
plot_likelihood_scape <- function(data, paramspace, dim, threshold, n = 1, color_gradient) {

  if(length(paramspace) != 2) stop("I need two parameters to make a heatmap across parameter space")

  library(ggplot2)

  # Make a vector of yes/no for speciation completion according to the specified criterion
  speciation_columns <- colnames(data)[grep(dim, colnames(data))]
  speciation_columns <- speciation_columns[(length(speciation_columns) - n + 1):length(speciation_columns)]
  speciation_criterion <- rowMeans(cbind(data[, speciation_columns]))
  is_speciation <- speciation_criterion > threshold

  # Count the proportion of successful speciation events for each parameter combination
  parameter_sets <- factor(apply(data[, paramspace], 1, paste, collapse = "_"))
  prob <- tapply(is_speciation, parameter_sets, function(outcomes_curr_set) mean(as.numeric(outcomes_curr_set)))
  parameter_space <- do.call("rbind", lapply(strsplit(names(prob), "_"), as.numeric))
  colnames(parameter_space) <- paste0("parameter_", seq_along(paramspace))

  # Make a summary table for plotting
  prob_data <- data.frame(parameter_space, prob)

  # Plot the resulting table
  p <- ggplot(data = prob_data, aes(x = parameter_1, y = parameter_2)) +
    geom_tile(aes(fill = prob)) +
    xlab(paramspace[1]) +
    ylab(paramspace[2])
  if(!missing(color_gradient)) p <- p + scale_fill_gradient(low = color_gradient[1], high = color_gradient[2])

  return(p)

}
