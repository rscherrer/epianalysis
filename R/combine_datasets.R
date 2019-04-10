#' Combine extracted simulation datasets
#'
#' This function extracts data from different input paths and combine them. It is a wrapper around \code{extract_from_simulations}.
#'
#' @param inpaths Where the simulation outputs are to be found. Need several of them, of course.
#' @param vars What variables through time to extract from simulation output data files?
#' @param params What parameter values to record for each simulation?
#' @param merge_factor The name of the column to add to the master data frame; the factor that differs between the different datasets that are combined.
#' @param merge_levels The levels of that factor. Should be of the same length as \code{inpaths}.
#' @return A data frame with individual simulationa in rows, and in columns the variables of interest at each time point, as well as the values of the selected parameters.
#' @export

# Function to extract data from different input paths and combine them
combine_datasets <- function(inpaths, vars, params, merge_factor, merge_levels) {

  if(length(merge_levels) != length(inpaths)) stop("inpaths and merge_levels should have the same length")

  datasets <- lapply(inpaths, function(inpath) {

    # Extract data from the simulations into a dataset
    dataset <- extract_from_simulations(inpath, vars, params)

  })

  # Merge datasets from the different input paths
  nrepets <- sapply(datasets, nrow)
  master_dataset <- do.call("rbind", datasets)

  master_dataset$merge_factor <- do.call("c", mapply(function(x, y) rep(x, y), merge_levels, nrepets, SIMPLIFY = F))
  colnames(master_dataset)[ncol(master_dataset)] <- merge_factor

  return(master_dataset)

}
