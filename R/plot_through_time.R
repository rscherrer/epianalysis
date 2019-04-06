#' Plot simulation metric through time
#'
#' This functions makes plots to track the advancement of the simulation through time.
#'
#' @param data A data frame containing coordinates along the three isolation axes (spatial, ecological and mating), or even other variables, through time. Individual simulations are in rows.
#' @param var The variable of interest.
#' @param type Either 1 -- shows lines for all simulations, 2 -- shows median and specified quantiles, 3 -- shows mean and standard error.
#' @param colvar Optional. The variable used to assign different colors to different groups of simulations.
#' @param xlim,ylim Plot limits (optional).
#' @param confint The quantiles to use as borders of shaded areas (only if type == 2).
#' @param cols A vector of colors to use for each group, only if colvar is specified.
#' @param show_legend Whether to display the legend. Defaults to FALSE.
#' @export


# Function to plot simulations through time
plot_through_time <- function(data, var, type = 1, colvar, xlim, ylim, confint, cols, show_legend = F) {

  # Extract timepoints
  timepoints <- as.numeric(sapply(strsplit(colnames(data)[grep(var, colnames(data))], "_"), "[", 2))

  # Is there a color variable?
  if(!missing(colvar)) {

    color_factor <- as.factor(data[,colvar])
    colors <- color_factor

    # Set default colors if colors not properly specified
    if(missing(cols) | length(cols) != nlevels(colors)) {

      cols <- gray(seq(0, 0.5, length.out = nlevels(colors)))

    }

    levels(colors) <- cols
    colors <- as.character(colors)

  } else cols <- colors <- "black"


  # Reduce to variable of interest and rearrange
  data <- t(data[,grep(var, colnames(data))])

  # Specify default plot limits
  if(missing(xlim)) xlim <- c(min(timepoints), max(timepoints))
  if(missing(ylim)) ylim <- c(min(data), max(data))

  # Set the plotting window
  plot(timepoints,
       data[,1],
       type = "n",
       xlim = xlim,
       ylim = ylim,
       las = 1,
       xlab = "Time (generations)",
       ylab = var)

  # Plot each line?
  if(type == 1) {

    sapply(seq_len(ncol(data)), function(i) {

      curr_simul <- data[,i]
      curr_color <- colors[i]
      lines(timepoints, curr_simul, col = curr_color)

    })

  } else if(type == 2) {

    # Plot quantiles?
    # Go through factor levels if necessary
    sapply(seq_along(unique(colors)), function(i) {

      curr_color <- cols[i]

      # Subset the data to the current factor level only if a grouping factor is specified
      if(exists("colvar")) {

        curr_level <- levels(color_factor)[i]
        data <- data[, color_factor == curr_level]

      }

      lowerbounds <- apply(data, 1, quantile, confint[1])
      upperbounds <- apply(data, 1, quantile, confint[2])
      poly_x <- c(timepoints, rev(timepoints))
      poly_y <- c(lowerbounds, rev(upperbounds))
      curr_rgb <- c(col2rgb(curr_color))/255
      shape_col <- rgb(curr_rgb[1], curr_rgb[2], curr_rgb[3], alpha = 0.5)
      # Semi transparent polygon
      polygon(x = poly_x,
              y = poly_y,
              border = NA,
              col = shape_col)
      medians <- apply(data, 1, median)
      # Central line is darker
      line_col <- rgb(curr_rgb[1]/2, curr_rgb[2]/2, curr_rgb[3]/2)
      lines(timepoints, medians, col = line_col)

    })

  } else if(type == 3) {

    sapply(seq_along(unique(colors)), function(i) {

      curr_color <- cols[i]

      # Subset the data to the current factor level only if a grouping factor is specified
      if(exists("colvar")) {

        curr_level <- levels(color_factor)[i]
        data <- data[, color_factor == curr_level]

      }

      # Plot mean and standard error?
      stderrors <- apply(data, 1, function(x) sqrt(var(x) / length(x)))
      means <- apply(data, 1, mean)
      lowerbounds <- means - stderrors
      upperbounds <- means + stderrors
      poly_x <- c(timepoints, rev(timepoints))
      poly_y <- c(lowerbounds, rev(upperbounds))
      curr_rgb <- c(col2rgb(curr_color))/255
      shape_col <- rgb(curr_rgb[1], curr_rgb[2], curr_rgb[3], alpha = 0.5)
      polygon(x = poly_x,
              y = poly_y,
              border = NA,
              col = shape_col)
      # Central line is darker
      line_col <- rgb(curr_rgb[1]/2, curr_rgb[2]/2, curr_rgb[3]/2)
      lines(timepoints, means, col = line_col)

    })
  }

  if(show_legend & !missing(colvar)) {
    legend("bottomright", legend = levels(color_factor), col = cols, pch = 16, title = colvar)
  }

}
