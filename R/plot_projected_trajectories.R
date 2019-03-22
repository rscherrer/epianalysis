#' Plot simulations
#'
#' This functions plots the simulations in 2D, along chosen dimensions.
#'
#' @param speciation_cube_data A data frame containing coordinates along the three isolation axes (spatial, ecological and mating) through time. Individual simulations are in rows.
#' @param vars The two variables along which to plot the simulations.
#' @param colvar Optional. The name of the variable to use to set colors according to.
#' @param show_legend Optional. Whether to show the legend.
#' @param xlim,ylim Ranges of values to plot.
#' @export


# Function to plot trajectories in 2D
plot_projected_trajectories <- function(speciation_cube_data, vars, colvar, show_legend = T, xlim, ylim) {

  if(!missing(colvar)) {

    shades_of_grey <- colorRampPalette(c("grey0", "grey80"))
    ncolors <- length(levels(as.factor(speciation_cube_data[, colvar])))
    colors <- shades_of_grey(ncolors)
    color_labels <- as.factor(speciation_cube_data[, colvar])
    levels(color_labels) <- colors

  } else {

    color_labels <- rep("black", nrow(speciation_cube_data))

  }

  # For each variable create a table with one column per line to draw, and as many rows as time points
  coordinates_per_variable <- lapply(vars, function(curr_variable) {

    # If the variable is time, all columns are the same
    if(curr_variable == "time") {

      coordinates <- colnames(speciation_cube_data)[grep(vars[vars != "time"], colnames(speciation_cube_data))]
      coordinates <- as.numeric(gsub("^.*_", "", coordinates))
      coordinates <- sapply(seq_len(nrow(speciation_cube_data)), function(curr_simulation) coordinates)

    } else {

      coordinates <- t(speciation_cube_data[,colnames(speciation_cube_data)[grep(curr_variable, colnames(speciation_cube_data))]])

    }

    return(coordinates)

  })

  if(missing(xlim)) xlim <- c(min(coordinates_per_variable[[1]]), max(coordinates_per_variable[[1]]))

  if(missing(ylim)) ylim <- c(min(coordinates_per_variable[[2]]), max(coordinates_per_variable[[2]]))

  # Plot the lines
  for(i in seq_len(nrow(speciation_cube_data))) {

    if(i == 1) {

      plot(
        x = coordinates_per_variable[[1]][,i],
        y = coordinates_per_variable[[2]][,i],
        type = "l",
        xlab = vars[1],
        ylab = vars[2],
        las = 1,
        xlim = xlim,
        ylim = ylim,
        col = as.character(color_labels[i])
      )

    } else {

      lines(
        x = coordinates_per_variable[[1]][,i],
        y = coordinates_per_variable[[2]][,i],
        col = as.character(color_labels[i])
      )

    }
  }

  if(!missing(colvar) & show_legend) {

    legend("bottomright", legend = levels(as.factor(speciation_cube_data[, colvar])), col = colors, pch = 16, title = colvar)

  }

}
