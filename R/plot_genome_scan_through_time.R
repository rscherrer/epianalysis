#' Plot genome scan through time
#'
#' This functions plots a heatmap of a given variable along the genome, through time.
#'
#' @param path Where the data for the given simulation are to be found.
#' @param var The variable to plot along the genome.
#' @export

plot_genome_scan_through_time <- function(path = ".", var) {

  library(ggplot2)

  homedir <- getwd()
  setwd(path)

  files <- list.files()[grep("nodes", list.files())]

  # Loop through time points and extract Fst across genome positions
  message("Extracting data through time...")
  genomes <- lapply(files, function(curr.file) {

    message(curr.file)
    genome <- read.csv(curr.file)
    genome <- genome[,c("id", "location", "linkage.group", var)]
    time <- as.numeric(unlist(strsplit(curr.file, "_"))[3])
    genome$time <- time
    return(genome)

  })
  genomes <- do.call("rbind", genomes)
  genomes$id <- genomes$id / max(genomes$id)

  # Plot the heatmap
  message("Plotting...")

  genomes$X <- genomes[,var]
  genome_scan <- ggplot(data = genomes, mapping = aes(x = time, y = id, fill = X)) +
    geom_tile() +
    xlab(label = "Time") +
    ylab(label = "Genome position") +
    scale_fill_gradient(name = var) +
    theme_bw()

  return(genome_scan)

}
