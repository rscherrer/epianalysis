#' Multiple speciation scapes
#'
#' This is a function to produce multiple heatmaps from a single dataset, by levels of a factor.
#'
#' @param data A data frame containing trajectories in speciation cube for all simulations (in rows).
#' @param paramspace The names of the two parameters to plot against.
#' @param dim The variable to use as a criterion for complete speciation.
#' @param threshold The threshold above which the value of the criterion variable means complete speciation
#' @param n How many of the last time points to average?
#' @param factor The factor used to split the plots
#' @param ... Extra arguments to be passed to \code{plot_likelihood_scape}.
#' @export

# Function to produce multiple heatmaps from a single dataset, by levels of a factor
plot_multiple_likelihood_scapes <- function(data, paramspace, dim, threshold, n, factor, ...) {

  factor_levels <- levels(as.factor(data[,factor]))

  p <- lapply(factor_levels, function(curr_level) {

    # Make a single plot
    data <- data[data[factor] == curr_level,]
    p <- plot_likelihood_scape(data = data, paramspace = paramspace, dim = dim, threshold = threshold, n = n, ...)
    return(p)

  })

  return(p)

}
