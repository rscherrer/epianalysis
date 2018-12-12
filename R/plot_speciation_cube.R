#' Plot simulations in speciation cube
#'
#' @param trajectories A data frame containing x, y and z coordinates for each simulation, so the number of columns (time column excluded) should be a multiple of 3. A time column may be present and entitled "time", but another name will yield a bug. The time column is not needed.
#' @param saveto Full path, including file name, to where to save the pdf figure. If not specified, the figure will be returned in R.
#' @param font Font to be used in the figure. Defaults to Helvetica. See those available with the package extrafont.
#' @param rarify Whether to rarify the data after the simulations have converged, in order to alleviate the amount of data. If TRUE, then dim and th have to be specified.
#' @param dim The dimension along which to look for convergence. Either of "spatial", "ecological" or "mating".
#' @param th The threshold value used to determine convergence, e.g. mating isolation above 0.9.
#'
#' @return A speciation cube plot.
#' @export

# Function to plot trajectories in speciation cube
plot_speciation_cube <- function(trajectories, saveto, font, rarify = F, dim, th) {

  library(plot3D)

  # Remove the time column of there is one
  timecol <- grep("time", colnames(trajectories))
  if(length(timecol) > 0) trajectories <- trajectories[,-timecol]

  # Count the number of simulations to draw
  if(ncol(trajectories) %% 3 != 0) stop("the number of columns is not a multiple of three")
  nsimul <- ncol(trajectories) / 3

  if(!missing(saveto)) pdf(saveto, family = ifelse(missing(font), "Helvetica", font))

  message("Plotting speciation cube...")

  # Loop through simulations
  for(i in seq_len(nsimul)) {

    message(paste("Plotting simulation", i, "out of", nsimul, "..."))

    idx <- 3*(i-1) + 1
    idy <- idx + 1
    idz <- idy + 1

    # For the current simulation
    # Find when it is virtually done
    # Rarify after this point

    if(rarify) {

      if(missing(dim)) stop("dim must be provided to rarify")
      if(missing(th)) stop("th must be provided to rarify")

      # Find the reference column to find from where on to rarify
      idim <- which(c("spatial", "ecological", "mating") == dim)
      idim <- c(idx, idy, idz)[idim]
      refcol <- trajectories[,idim]

      # From where does that column goes above the threshold?
      # This is the virtual end of the simulation
      tend <- min(which(refcol > th))

      # Rarify the data after this point because not much happens anyway
      chosenones <- sort(sample(tend:length(refcol), size = 30, replace = F))
      chosenones<- c(1:(tend-1), chosenones)

      # Take that subset of the trajectory
      trajx <- trajectories[chosenones, idx]
      trajy <- trajectories[chosenones, idy]
      trajz <- trajectories[chosenones, idz]

    }

    # Plot the line inside the speciation cube
    scatter3D(x = trajx,
              y = trajy,
              z = trajz,
              bty = "g",
              phi = 0,
              ticktype = "detailed",
              type = "l",
              main = "Speciation cube",
              xlab = "Spatial isolation",
              ylab = "Ecological isolation",
              zlab = "Mating isolation",
              colkey = F,
              xlim = c(0,1),
              ylim = c(0,1),
              zlim = c(0,1),
              add = ifelse(i == 1, FALSE, TRUE))

  }

  if(!missing(saveto)) dev.off()

}
