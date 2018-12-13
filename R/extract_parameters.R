#' Extract parameters of the simulations
#'
#' @param datapath Path to where the simulation folders are.
#' @param outpath Where to save the output data frame. Optional.
#' @param whichparams What parameters to extract?
#' @return A data frame with the parameter values for each simulation.
#' @export

# Extract parameter sets from the simulations
extract_parameters <- function(datapath, outpath, whichparams = c("habitat_asymmetry", "sel_coeff_ecol")) {

  homedir <- getwd()
  setwd(datapath)
  message("Extracting simulation parameter sets...")

  simulfolders <- list.files()

  # For each simulation folder...
  params <- lapply(simulfolders, function(currfolder) {

    message(currfolder)

    setwd(currfolder)

    # Look at the files in the folder
    simulfiles <- list.files()

    # Find the parameter file
    id.paramfile <- grep("parameters", simulfiles)

    # Exception
    if (length(id.paramfile) == 0)
      stop(paste("parameter file not found in folder", currfolder))
    if (length(id.paramfile) > 1)
      stop(paste("more than one parameter file found in folder",
                 currfolder))

    # Read the file
    paramfile <- simulfiles[id.paramfile]
    params <- read.delim(paramfile, header = F)
    params <- as.character(params[,1])

    # Extract the parameters of interest
    params <- lapply(whichparams, function(whichparam) {

      # Find the parameter
      id.param <- grep(whichparam, params)
      if(length(id.param) == 0) stop(paste("parameter", whichparam, "not found"))

      # Extract value(s)
      row <- gsub(paste0(whichparam, " "), "", params[id.param])
      row <- unlist(strsplit(row, " "))
      if(length(row) == 0) return(whichparam) else return(row)

    })

    # Organize them into a vector
    nparams <- sapply(params, length)
    paranames <- unlist(mapply(function(x,y) rep(x,y), whichparams, nparams))
    paranames <- names(paranames)
    params <- do.call("c", params)
    names(params) <- paranames

    setwd("..")

    return(params)

  })

  params <- as.data.frame(do.call("rbind", params))

  # Go home
  setwd(homedir)

  # Save, if needed
  if(!missing(outpath)) write.csv(params, outpath)

  return(params)

}
