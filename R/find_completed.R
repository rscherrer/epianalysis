#' Find completed events
#'
#' Function to find sets of simulations reaching a certain threshold in certain dimensions.
#'
#' @param speciation_cube_data A data frame containing trajectories in speciation cube for all simulations (in rows).
#' @param dim The variable to use as a criterion for complete speciation.
#' @param threshold The threshold above which the value of the criterion variable means complete speciation
#' @param n How many of the last time points to average?
#'
#' @return A subset of \code{speciation_cube_data}.
#' @export

# Find sets of simulations reaching a certain threshold in a certain dimensions
find_completed <- function(speciation_cube_data, dim, threshold, n) {

  # Make a vector of yes/no for speciation completion according to the specified criterion
  speciation_columns <- colnames(speciation_cube_data)[grep(dim, colnames(speciation_cube_data))]
  speciation_columns <- speciation_columns[(length(speciation_columns) - n + 1):length(speciation_columns)]
  speciation_criterion <- rowMeans(cbind(speciation_cube_data[, speciation_columns]))
  is_speciation <- speciation_criterion > threshold
  return(is_speciation)

}
