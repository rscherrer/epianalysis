#' Smoothen a trajectory
#'
#' @param df A data frame with numeric columns.
#' @param nbins Integer. The number of bins to calculate averages for when smoothing.
#' @return A matrix with the same number of columns as `df` and `nbins` columns.
#' @export

# Function to smoothen a data frame by taking bin-averages
smoothen_bins <- function(df, nbins) {

  i <- 1
  n <- nrow(df)
  step <- floor(n / nbins)
  j <- step
  bins <- matrix(0, nrow = nbins, ncol = ncol(df))
  bin.id <- 1
  while(j < n) {

    # When last bin is reached, take all remaining rows
    if(bin.id == nbins) j <- n

    # Append new bin
    bins[bin.id,] <- colMeans(df[i:j,])

    # Move on to the next bin
    i <- j + 1
    j <- j + step
    bin.id <- bin.id + 1

  }
  return(bins)

}
