#' Speciation cube
#'
#' This functions plots the simulations in speciation cube. Avoid using it to visualize directly in RStudio, takes too long. Instead, prefer calling a pdf device before running the function, a view the PDF. Much faster.
#'
#' @param speciation_cube_data A data frame containing coordinates along the three isolation axes (spatial, ecological and mating) through time. Individual simulations are in rows.
#' @export

# Function to plot a set of trajectories in speciation cube
plot_speciation_cube <- function(speciation_cube_data) {

  library(plot3D)

  xcol <- grep("speciation.cube.spatial.isolation", colnames(speciation_cube_data))
  ycol <- grep("speciation.cube.ecological.isolation", colnames(speciation_cube_data))
  zcol <- grep("speciation.cube.mating.isolation", colnames(speciation_cube_data))

  for(i in seq_len(nrow(speciation_cube_data))) {

    message(paste(round(i / nrow(speciation_cube_data), 2) * 100), "% done")

    curr_simulation <- unlist(speciation_cube_data[i,])

    if(i == 1) {

      scatter3D(
        x = curr_simulation[xcol],
        y = curr_simulation[ycol],
        z = curr_simulation[zcol],
        bty = "g",
        phi = 0,
        ticktype = "detailed",
        type = "l",
        main = "Speciation cube",
        xlab = "Spatial isolation",
        ylab = "Ecological isolation",
        zlab = "Mating isolation",
        xlim = c(0, 1),
        ylim = c(0, 1),
        zlim = c(0, 1),
        colkey = TRUE
      )

    } else {

      scatter3D(
        x = curr_simulation[xcol],
        y = curr_simulation[ycol],
        z = curr_simulation[zcol],
        type = "l",
        colkey = FALSE,
        add = TRUE
      )

    }
  }
}
