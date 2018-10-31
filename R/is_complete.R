#' Is a speciation event complete?
#'
#' @param folder A string. The path to the folder of a given simulation.
#' @param ri.threshold Numeric. The threshold value of final mating isolation above which speciation is complete.
#' @return A vector containing the habitat asymmetry, selection coefficient and whether speciation is complete (0 or 1).

is_complete <- function(folder, ri.threshold = 0.9) {

  message(folder)

  # Record parameter values
  currFiles <- list.files(folder)
  if(!"parameters.txt" %in% currFiles) stop(paste("parameters.txt missing from folder", folder))
  paramFile <- paste(folder, "parameters.txt", sep = "/")
  parameters <- read.delim(paramFile)
  i <- grep("habitat_asymmetry", parameters[,1])
  j <- grep("sel_coeff_ecol", parameters[,1])
  paramValues <- sapply(c(i, j),  function(x){

    as.numeric(unlist(strsplit(as.character(parameters[x,1]), " "))[-1])

  })
  names(paramValues) <- c("habitat_asymmetry", "sel_coeff_ecol")

  # Read data file
  if(length(grep("*.dat", currFiles)) == 0) stop(paste("data file missing in folder", folder))
  dataFile <- paste(folder, currFiles[grep("*.dat", currFiles)], sep = "/")
  simulation <- read.delim(dataFile)

  # Tell whether speciation is complete -- look at the last value
  isComplete <- as.numeric(simulation$speciation.cube.mating.isolation[nrow(simulation)] > ri.threshold)

  # Return parameters + speciation
  out <- c(paramValues, isComplete)
  names(out) <- c(names(paramValues), "is.complete")
  return(out)

}
