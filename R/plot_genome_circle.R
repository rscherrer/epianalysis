#' Plot circular genome scan
#'
#' This functions plots variables across the genome in a circular plot.
#'
#' @param path Where the data for the given simulation are to be found.
#' @param vars The variables to plot along the genome.
#' @param time What time point?
#' @param tracks A vector of integer indices indicating what track (i.e. line from the outside inwards) each variable should appear on. Should be of the same length as \code{vars}. To overlay variables on the same track, assign to them the same index.
#' @param cols Colors. Should be as long as \code{vars}. If not specified, default palette colors are taken.
#' @export

# Function to plot variables across the genome
plot_genome_circle <- function(path = ".", vars = c("Fst", "varP"), time = 500000, tracks = seq_along(vars), cols = palette()[seq_along(vars)]) {

  library(circlize)

  # Check
  if(length(tracks) != length(vars)) stop("there should be as many track indices as variables")
  if(length(cols) != length(vars)) stop("there should be as many colors as variables")

  homedir <- getwd()
  setwd(path)

  # Load genome characteristics
  file <- list.files()[grep("nodes", list.files())]
  isTime <- as.numeric(do.call("rbind", strsplit(file, '_'))[,3]) == time
  if(!any(isTime)) stop("time not available")
  if(length(which(isTime)) > 1) stop("there should be one file per time, please check the folder")
  file <- file[isTime]
  genome <- read.csv(file)

  setwd(homedir)

  # Make circle plot
  dim(genome)
  colnames(genome)

  layout(rbind(c(1, 1, 1, 1, 1, 2)))
  for(i in seq_along(vars)) {

    if(i == 1) circos.initialize(factors = genome$linkage.group, x = genome$location)

    curr_var <- vars[i]
    curr_track <- tracks[i]

    circos.track(
      factors = genome$linkage.group,
      x = genome$location,
      y = genome[,curr_var],
      track.index = curr_track,
      panel.fun = function(x, y) {
        if(i == 1) circos.axis(labels.cex = 0.6)
        circos.lines(x, y, col = ifelse(!exists("cols"), "black", cols[i]))
      }
    )
  }

  plot.new()
  legend("left", legend = vars, col = cols, pch = 16)

}
