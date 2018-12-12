#' Extract trajectories in speciation cube
#'
#' @param datapath Path to where the simulation folders are.
#' @param outpath Where to save the output data frame.
#' @return A data frame with one column for time and three columns per simulation containing coordinates in speciation cube. The data frame is saved as a CSV file in outpath.
#' @export

# Function to extract trajectories from a raw data folder into a single data frame
extract_trajectories <- function(datapath, outpath) {

  homedir <- getwd()
  setwd(datapath)

  message("Extracting speciation cube trajectories...")

  # Loop through every folder in here
  simulfolders <- list.files()
  trajectories <- lapply(simulfolders, function(currfolder) {

    message(currfolder)

    # Go into that folder
    setwd(currfolder)

    # Look for a .dat file
    simulfiles <- list.files()
    id.datafile <- grep(".dat$", simulfiles)

    # Exception handling
    if(length(id.datafile) == 0) stop(paste("data file not found in folder", currfolder))
    if(length(id.datafile) > 1) stop(paste("more than one data file found in folder", currfolder))

    # If found, read that file
    datafile <- simulfiles[id.datafile]
    data <- read.delim(datafile)

    # Extract the time column
    time <- data[,1]

    # Pull out the cube coordinates
    coordinates <- data[,grep("speciation.cube", colnames(data))]

    setwd("..")

    out <- as.data.frame(cbind(time, coordinates))
    return(out)

  })

  # Record the duration of each simulation
  durations <- sapply(trajectories, nrow)

  # What is the maximum duration?
  maxduration <- max(durations)

  # Are all simulations complete?
  noextinct <- all(durations == maxduration)

  # If anyone went extinct...
  if(!noextinct) {

    # Which ones?
    id.extinct <- durations != maxduration

    # For each of these simulations
    for(id in which(id.extinct)) {

      # Count the number of missing rows
      nmissingrows <- maxduration - durations[id]

      # Append a NA matrix of the right size to the existing data frame
      trajectories[[id]] <- rbind(trajectories[[id]], matrix(NA, ncol = 4, nrow = nmissingrows))

    }

    # Check
    if(!all(sapply(trajectories, nrow) == maxduration)) stop("some populations went extinct and were not properly resized with NAs")

  }

  # Merge everything into a data frame
  out <- do.call("cbind", trajectories)

  # Remove the time columns except the first one
  timecols <- seq(1, ncol(out), 4)[-1]
  out <- out[,-timecols]

  # Rename columns
  nsimul <- (ncol(out)-1)/3
  colnames(out)[-1] <- paste0(rep(c("x", "y", "z"), nsimul), rep(seq_len(nsimul), each = 3))

  # Go home
  setwd(homedir)

  # Save output
  write.csv(out, paste(outpath, "trajectories.csv", sep = "/"), row.names = F)

  return(out)

}
