#' Extract trajectories in speciation cube
#'
#' @param dir A string. The path to the folders containing the simulations.
#' @param is.complete A data frame. Contains information about habitat asymmetry, selection coefficient and whether speciation was complete or not (see argument `ri.threshold`) for each simulation. Typically the output of `find_completed`.
#' @return A list of data frames, one for each simulation. Each data frame contains three columns, one for each dimension of the speciation cube.

# Function to extract trajectory information
extract_trajectories <- function(dir, is.complete) {

  homedir <- getwd()
  setwd(".")

  # Focus on the relevant folders, if specified
  allFolders <- list.files()
  if(exists("is.complete")) {
    whichComplete <- as.logical(is.complete$is.complete)
    whichFolders <- allFolders[whichComplete]
  } else {
    whichFolders <- allFolders
  }

  # Loop through these folders and record the final value of
  trajectories <- lapply(whichFolders, function(curr.folder) {

    message(curr.folder)

    # Load the dataset
    currFiles <- list.files(curr.folder)
    if(length(grep("*.dat", currFiles)) == 0) stop(paste("data file missing from folder", curr.folder))
    dataFile <- paste(curr.folder, currFiles[grep("*.dat", currFiles)], sep = '/')
    simulation <- read.delim(dataFile)

    # Return relevant columns
    burnin <- simulation[,1] < 0 # remove burnin
    relevantData <- simulation[!burnin,]
    relevantData <- relevantData[,c("speciation.cube.spatial.isolation", "speciation.cube.ecological.isolation", "speciation.cube.mating.isolation")]

    return(relevantData)

  })

  setwd(homedir)

  return(trajectories)

}
