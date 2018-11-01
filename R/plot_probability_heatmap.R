#' Speciation likelihood heatmap
#'
#' @param dir A string. The path to the folders containing the simulations.
#' @param is.complete A data frame. Contains information about habitat asymmetry, selection coefficient and whether speciation was complete or not (see argument `ri.threshold`) for each simulation. Typically the output of `find_completed`.
#' @param ri.threshold Numeric. If `is.complete` is not provided, the function will run `find_completed` to create it. `ri.threshold` is the threshold value of final mating isolation above which speciation is complete.
#' @param isRipa Logical. Whether to plot parameter space in terms of resource ratio and niche width (as in Ripa et al.) or in terms of habitat asymmetry and selection coefficient (FALSE).

# Function to plot speciation probability heatmap
plot_probability_heatmap <- function(dir = ".", is.complete, ri.threshold = 0.9, isRipa = F) {

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

  if(isRipa) {
    specProbDF$habitat_asymmetry <- 1 - specProbDF$habitat_asymmetry
    specProbDF$sel_coeff_ecol <- 1 / sqrt(2 * specProbDF$sel_coeff_ecol)
  }

  library(extrafont)
  loadfonts()

  # GGplot
  library(ggplot2)

  plotname <- paste0("../../plots/likelihood_scape", ifelse(!isRipa, ".pdf", "_ripaetal.pdf"))
  xlab <- ifelse(!isRipa, "Habitat asymmetry", "Resource ratio")
  ylab <- ifelse(!isRipa, "Selection coefficient", "Niche width")

  myHeatmap <- ggplot(data = specProbDF, mapping = aes(x = habitat_asymmetry, y = sel_coeff_ecol, fill = specProb)) + geom_tile() + xlab(label = xlab) + ylab(label = ylab) + scale_fill_gradient(name = "Speciation probability") + theme_bw() + theme(text=element_text(family="Garamond", size=14))
  print(myHeatmap)
  ggsave(plotname, myHeatmap, width=6, height=4)

  setwd(homedir)

}
