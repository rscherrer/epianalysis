#' Plot simulations in speciation cube
#'
#' @param trajectories A data frame containing x, y and z coordinates for each simulation, so the number of columns (time column excluded) should be a multiple of 3. A time column may be present and entitled "time", but another name will yield a bug. The time column is not needed.
#' @param saveto Full path, including file name, to where to save the pdf figure. If not specified, the figure will be returned in R.
#' @param font Font to be used in the figure. Defaults to Helvetica. See those available with the package extrafont.
#' @return A speciation cube plot.
#' @export

# Function to plot trajectories in speciation cube
plot_speciation_cube <- function(trajectories, saveto, font) {

  library(plot3D)

  # Remove the time column of there is one
  timecol <- grep("time", colnames(trajectories))
  if(length(timecol) > 0) trajectories <- trajectories[,-timecol]

  # Count the number of simulations to draw
  if(ncol(trajectories) %% 3 != 0) stop("the number of columns is not a multiple of three")
  nsimul <- ncol(trajectories) / 3

  if(!missing(saveto)) pdf(saveto, family = ifelse(missing(font), "Helvetica", font))

  # Loop through simulations
  for(i in seq_len(nsimul)) {

    idx <- 3*(i-1) + 1
    idy <- idx + 1
    idz <- idy + 1

    # Plot the line inside the speciation cube
    scatter3D(x = trajectories[,idx],
              y = trajectories[,idy],
              z = trajectories[,idz],
              bty = "g",
              phi = 0,
              ticktype = "detailed",
              type = "l",
              main = "Speciation cube",
              xlab = "Spatial isolation",
              ylab = "Ecological isolation",
              zlab = "Reproductive isolation",
              colkey = F,
              xlim = c(0,1),
              ylim = c(0,1),
              zlim = c(0,1),
              add = ifelse(i == 1, FALSE, TRUE))

  }

  if(!missing(saveto)) dev.off()

}
