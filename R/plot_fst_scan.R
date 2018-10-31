#' Is a speciation event complete?
#'
#' @param folder A string. The path to the folder of a given simulation.
#' @param time Integer. The time step at which to plot the Fst scan.

# Plot Fst throughout the genome at a given time
plot_fst_scan <- function(folder, time = 500000) {

  homedir <- getwd()
  setwd(folder)

  # Load the genome at the end of the simulation
  file <- list.files()[grep("nodes", list.files())]
  isTime <- as.numeric(do.call("rbind", strsplit(file, '_'))[,3]) == time
  if(!any(isTime)) stop("time not available")
  if(length(which(isTime)) > 1) stop("there should be one file per time, please check the folder")
  file <- file[isTime]
  genome <- read.csv(file)

  setwd(homedir)

  # Plot the Fst scan
  library(extrafont)
  loadfonts()
  plotname <- paste0("../../plots/", paste("fstscan", folder, time, sep = "_"), ".pdf")
  pdf(plotname, family = "Garamond", width = 5, height = 3)
  par(xpd = NA) # to draw outside the plot
  plot(Fst ~ location, data = genome, type = "h", xlab = "Genome location", las = 1, mgp = c(2.2, 1, 0))
  # Add chromosomes on top of the chart
  y <- max(genome$Fst) + 0.1 * (max(genome$Fst) - min(genome$Fst))
  for(i in unique(genome$linkage.group)) {
    loffset <- ifelse(i == min(unique(genome$linkage.group)), 0, 0.01)
    uoffset <- ifelse(i == max(unique(genome$linkage.group)), 0, 0.01)
    lbound <- genome$location[min(which(genome$linkage.group == i))] + loffset
    ubound <- genome$location[max(which(genome$linkage.group == i))] - uoffset
    segments(x0 = lbound, x1 = ubound, y0 = y, y1 = y, lwd = 5)
  }

  dev.off()

}
