#' Find simulations that have reached a threshold along one dimension
#'
#' @param trajectories A data frame containing the speciation cube coordinates through time of the simulations.
#' @param dim Along what dimension to check? Either of "spatial", "ecological" or "mating".
#' @param th Threshold value for the coordinate at the final time step. Above this value, returns a TRUE, otherwise a FALSE.
#' @return A boolean vector indicating which simulations did reach at least the specified threshold value along the specified dimension of the speciation cube.

# Function to tell what simulations have reached isolation in some dimension of the cube
is_isolated <- function(trajectories, dim, th) {

  # Remove the time column of there is one
  timecol <- grep("time", colnames(trajectories))
  if(length(timecol) > 0) trajectories <- trajectories[,-timecol]

  # Count the number of simulations to draw
  if(ncol(trajectories) %% 3 != 0) stop("the number of columns is not a multiple of three")
  nsimul <- ncol(trajectories) / 3

  # Identify the columns to check
  firstcol <- which(c("spatial", "ecological", "mating") == dim)
  if(length(firstcol) == 0) stop("incorrect cube dimension")
  cols <- seq(firstcol, nsimul, 3)

  # Compare the final value of each target column to the threshold
  out <- trajectories[nrow(trajectories),cols] > th
  return(out)

}
