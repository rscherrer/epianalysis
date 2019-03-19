#' Subset completed events
#'
#' @param speciation_cube_data A data frame containing trajectories in speciation cube for all simulations (in rows).
#' @param dim The variable to use as a criterion for complete speciation.
#' @param threshold The threshold above which the value of the criterion variable means complete speciation
#' @param n How many of the last time points to average?
#'
#' @return A subset of \code{speciation_cube_data}.
#' @export

# Function to isolate a subset of the data
subset_completed <- function(speciation_cube_data, dim, threshold, n) {

  is_speciation <- find_completed(speciation_cube_data, dim, threshold, n)

  speciation_cube_data <- speciation_cube_data[is_speciation,]
  return(speciation_cube_data)

}
