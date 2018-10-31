#' Fst scan through time
#'
#' @param folder A string. The path to the folder of a given simulation.

# Function to map Fst along the genome through time
plot_fst_map <- function(folder) {

  # Go to a folder
  homedir <- getwd()
  setwd(folder)

  files <- list.files()[grep("nodes", list.files())]

  # Loop through time points and extract Fst across genome positions
  message("Extracting Fst through time...")
  genomes <- lapply(files, function(curr.file) {

    message(curr.file)
    genome <- read.csv(curr.file)
    genome <- genome[,c("id", "location", "linkage.group", "Fst")]
    time <- as.numeric(unlist(strsplit(curr.file, "_"))[3])
    genome$time <- time
    return(genome)

  })
  genomes <- do.call("rbind", genomes)
  genomes$id <- genomes$id / max(genomes$id)

  setwd(homedir)

  # Plot the heatmap
  message("Plotting...")
  library(ggplot2)
  library(extrafont)
  fstMap <- ggplot(data = genomes, mapping = aes(x = time, y = id, fill = Fst)) +
    geom_tile() +
    xlab(label = "Time") +
    ylab(label = "Genome position") +
    scale_fill_gradient(name = "Fst") +
    theme_bw() +
    theme(text = element_text(family = "Garamond", size = 11))
  plotname <- paste0("../../plots/fstmap_", folder, ".pdf")
  ggsave(plotname, fstMap, width = 7, height = 4)

}
