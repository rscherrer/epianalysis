#' Plot genome scan
#'
#' This functions plots a scan of a given variable along the genome.
#'
#' @param path Where the data for the given simulation are to be found.
#' @param var The variable to plot along the genome.
#' @param time What time point?
#' @export

# Show a genome Fst scan
plot_genome_scan <- function(path = ".", var, time = 500000) {

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

  plot(Fst ~ location, data = genome, type = "h", xlab = "Genome location", las = 1, mgp = c(2.2, 1, 0))
  # Add chromosomes on top of the chart
  y <- max(genome[,var]) + 0.1 * (max(genome[,var]) - min(genome[,var]))
  for(i in unique(genome$linkage.group)) {
    loffset <- ifelse(i == min(unique(genome$linkage.group)), 0, 0.01)
    uoffset <- ifelse(i == max(unique(genome$linkage.group)), 0, 0.01)
    lbound <- genome$location[min(which(genome$linkage.group == i))] + loffset
    ubound <- genome$location[max(which(genome$linkage.group == i))] - uoffset
    segments(x0 = lbound, x1 = ubound, y0 = y, y1 = y, lwd = 5)
  }

}
