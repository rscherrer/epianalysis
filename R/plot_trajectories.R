#' Plot trajectories in speciation cube
#'
#' @param trajectories A list of data frames. Each data frame has three columns, one for each dimension of the cube. Typically the output of `extract_trajectories`.
#' @param smoothing Logical. `TRUE` if you want to average part of the trajectories into bins of given sizes.
#' @param nbins Integer. If `smoothing = TRUE`, the number of bins to calculate averages for when smoothing.
#' @param ri.threshold Numeric. If `smoothing = TRUE`, this argument specifies the value of mating isolation over which determines the part of the trajectory that has to be smoothed. A trajectory is smoothed onwards from the time at which it irreversibly goes above `ri.threshold` in the course of the simulation. This makes the plot lighter while keeping the most informative part of each trajectory intact (i.e. the way towards reproductive isolation).
#'
#' @export

# Function to plot trajectories in speciation cube
# dir = where the folders are
# ri.threshold = threshold for complete speciation
# will plot only those trajectories that have completed speciation
plot_trajectories <- function(trajectories, smoothing = T, nbins = 30, ri.threshold = 0.9) {

  if(smoothing) {

    message("Smoothing...")
    # For all completed events, return a lighter dataset
    smoothed <- lapply(trajectories, function(simulation) {

      # Find the time step t at which MI irreversibly goes above the threshold ri.threshold
      t <- NULL
      i <- 1
      vec <- simulation$speciation.cube.mating.isolation
      n <- length(vec)
      while(is.null(t)) {
        if(all(vec[i:n] > ri.threshold)) t <- i
        i <- i + 1
      }

      # Make a new dataset with the original data up to that point
      smoothdata1 <- simulation[1:t,]

      # But smoothen the data into bins after that point
      smoothdata2 <- smoothen_bins(simulation[(t+1):n,], nbins = nbins)
      colnames(smoothdata2) <- colnames(smoothdata1)

      # Merge the two
      smoothdata <- rbind(smoothdata1, smoothdata2)

      return(smoothdata)

    })

    trajectories <- smoothed

  }

  message("Plotting...")
  library(plot3D)
  library(extrafont)
  loadfonts()
  pdf("../../plots/speciation_cube.pdf", family = "Garamond", width = 5, height = 5)

  for(i in 1:length(trajectories)) {

    message(paste0(round(i / length(trajectories) * 100), "% done"))

    dataset <- trajectories[[i]]
    if(i == 1) add <- F else add <- T

    scatter3D(dataset[,1], dataset[,2], dataset[,3], type = "l", phi = 0, bty = "g", ticktype = "detailed", clim = c(0,1), xlim = c(0,1), ylim = c(0,1), zlim = c(0,1), main = "Speciation cube", xlab = "Spatial isolation", ylab = "Ecological isolation", zlab = "Mating isolation", add = add, colkey = F)

  }

  dev.off()

  setwd(homedir)

}
