#' Subset completed events
#'
#' @param speciation_cube_data A data frame containing trajectories in speciation cube for all simulations (in rows).
#' @param paramspace The names of the two parameters to plot against.
#' @param dim The variable to use as a criterion for complete speciation.
#' @param threshold The threshold above which the value of the criterion variable means complete speciation
#' @param n How many of the last time points to average?
#'
#' @return A subset of \code{speciation_cube_data}.
#' @export

# Function to isolate a subset of the data
subset_completed <- function(speciation_cube_data, dim, threshold, n) {

  # Make a vector of yes/no for speciation completion according to the specified criterion
  speciation_columns <- colnames(speciation_cube_data)[grep(dim, colnames(speciation_cube_data))]
  speciation_columns <- speciation_columns[(length(speciation_columns) - n + 1):length(speciation_columns)]
  speciation_criterion <- rowMeans(cbind(speciation_cube_data[, speciation_columns]))
  is_speciation <- speciation_criterion > threshold

  speciation_cube_data <- speciation_cube_data[is_speciation,]
  return(speciation_cube_data)

}
