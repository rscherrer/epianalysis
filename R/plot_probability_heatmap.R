#' Speciation likelihood heatmap
#'
#' @param dir A string. The path to the folders containing the simulations.
#' @param is.complete A data frame. Contains information about habitat asymmetry, selection coefficient and whether speciation was complete or not (see argument `ri.threshold`) for each simulation. Typically the output of `find_completed`.
#' @param ri.threshold Numeric. If `is.complete` is not provided, the function will run `find_completed` to create it. `ri.threshold` is the threshold value of final mating isolation above which speciation is complete.

# Function to plot speciation probability heatmap
plot_probability_heatmap <- function(dir = ".", is.complete, ri.threshold = 0.9) {

  homedir <- getwd()
  setwd(dir)

  # If identity of the completed speciation events is provided...
  if(exists("is.complete")) {

    message("is.complete was provided")

  } else {

    # Find what speciation events are complete
    is.complete <- find_completed(dir = ".", ri.threshold = ri.threshold)

  }

  # For each parameter set...
  paramSets <- as.factor(paste(is.complete$habitat_asymmetry, is.complete$sel_coeff_ecol))

  # ...Calculate the probability of speciation
  specProb <- tapply(is.complete$is.complete, paramSets, mean)

  # Make it a data frame
  specProbDF <- cbind(do.call("rbind", lapply(strsplit(as.character(levels(paramSets)), " "), as.numeric)), specProb)
  colnames(specProbDF)[1:2] <- names(is.complete)[1:2]
  specProbDF <- as.data.frame(specProbDF)

  # Rearrange into a matrix suitable for plotting
  library(reshape2)
  specProbMat <- acast(specProbDF, habitat_asymmetry ~ sel_coeff_ecol, value.var = "specProb")

  library(extrafont)
  loadfonts()

  # Plot
  library(fields)
  pdf("../../plots/heatmap_baseR.pdf", family = "Garamond", width = 4, height = 4)
  image.plot(x = as.numeric(rownames(specProbMat)), y = as.numeric(colnames(specProbMat)), z = specProbMat, xlab = "Habitat asymmetry", ylab = "Selection coefficient", main = "Speciation probability", las = 1)
  dev.off()

  # GGplot
  library(ggplot2)
  myHeatmap <- ggplot(data = specProbDF, mapping = aes(x = habitat_asymmetry, y = sel_coeff_ecol, fill = specProb)) + geom_tile() + xlab(label = "Habitat asymmetry") + ylab(label = "Selection coefficient") + scale_fill_gradient(name = "Speciation probability") + theme_bw() + theme(text=element_text(family="Garamond", size=14))
  print(myHeatmap)
  ggsave("../../plots/heatmap_ggplot.pdf", myHeatmap, width=4, height=4)

  setwd(homedir)

  # Return the matrix of speciation probabilities used to make the heatmap
  return(specProbMat)

}
