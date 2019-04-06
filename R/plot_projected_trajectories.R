#' Plot simulations
#'
#' This functions plots the simulations in 2D, along chosen dimensions.
#'
#' @param speciation_cube_data A data frame containing coordinates along the three isolation axes (spatial, ecological and mating) through time. Individual simulations are in rows.
#' @param vars The two variables along which to plot the simulations.
#' @param colvar Optional. The name of the variable to use to set colors according to.
#' @param show_legend Optional. Whether to show the legend.
#' @param xlim,ylim Ranges of values to plot.
#' @param confint Confidence interval probabilities. If specified, the confidence interval is plotted instead of every single line. To plot mean and standard error instead of a confidence interval, provide this argument with an object of length different than 2 (which is what the function expects as boundaries of the confidence interval).
#' @param col Color to plot the lines or the confidence interval. If specified, overrides the coloration per parameter value, if any.
#' @param add Whether to append to a previous plot. Defaults to FALSE.
#' @export


# Function to plot trajectories in 2D
plot_projected_trajectories <- function(speciation_cube_data, vars, colvar, show_legend = T, xlim, ylim, confint, col, add = F) {

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

  if(!missing(confint)) {

    # If a confidence interval of the Y-variable is to be plotted
    # Calculate the median and confidence interval
    if(length(confint) == 2) {

      mode_y <- apply(coordinates_per_variable[[2]], 1, median)
      confidence_interval_y <- t(apply(coordinates_per_variable[[2]], 1, quantile, probs = confint))

    } else {

      # If confidence interval argument does not have 2 elements,plot  mean and standard error instead
      mode_y <- apply(coordinates_per_variable[[2]], 1, mean)
      standard_error_y <- sqrt(apply(coordinates_per_variable[[2]], 1, var) / ncol(coordinates_per_variable[[2]]))
      confidence_interval_y <- cbind(mode_y - standard_error_y, mode_y + standard_error_y)

    }

    if(!add) {
      plot(
        x = coordinates_per_variable[[1]][,1],
        y = mode_y,
        type = "n",
        xlab = vars[1],
        ylab = vars[2],
        las = 1,
        xlim = xlim,
        ylim = ylim
      )
    }

    poly_x <- c(coordinates_per_variable[[1]][,1], rev(coordinates_per_variable[[1]][,1]))
    poly_y <- c(confidence_interval_y[,1], rev(confidence_interval_y[,2]))
    polygon(
      x = poly_x,
      y = poly_y,
      col = ifelse(missing(col), "lightgray", col),
      border = NA
    )

    linecol <- c(col2rgb(col))/255/2
    linecol <- rgb(linecol[1], linecol[2], linecol[3])

    lines(
      x = coordinates_per_variable[[1]][,1],
      y = mode_y,
      col = linecol
    )

  } else {

    # Plot the lines
    for(i in seq_len(nrow(speciation_cube_data))) {

      if(missing(col)) col <- as.character(color_labels[i])

      if(i == 1 & !add) {

        plot(
          x = coordinates_per_variable[[1]][,i],
          y = coordinates_per_variable[[2]][,i],
          type = "l",
          xlab = vars[1],
          ylab = vars[2],
          las = 1,
          xlim = xlim,
          ylim = ylim,
          col = col
        )

      } else {

        lines(
          x = coordinates_per_variable[[1]][,i],
          y = coordinates_per_variable[[2]][,i],
          col = col
        )

      }
    }

    if(!missing(colvar) & show_legend) {

      legend("bottomright", legend = levels(as.factor(speciation_cube_data[, colvar])), col = colors, pch = 16, title = colvar)

    }

  }

}
