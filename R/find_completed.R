#' Find completed speciation events
#'
#' @param dir A string. The path to the folders containing the simulations.
#' @param ri.threshold Numeric. The threshold value of final mating isolation above which speciation is complete.
#' @return A data frame containing information about habitat asymmetry, selection coefficient and whether speciation is complete, for each simulation.

# Function to find which speciation events are completed
find_completed <- function(dir = ".", ri.threshold = 0.9) {

  message("Going through folders...")
  endpoints <- lapply(list.files(dir), is_complete, ri.threshold)
  is.complete <- as.data.frame(do.call("rbind", endpoints))
  return(is.complete)

}
