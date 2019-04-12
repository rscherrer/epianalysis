#' Extract data from simulations
#'
#' This function loops through the simulation output folders and reads in the information from the variables and parameter values of interest. It writes all this into a large data frame that can then be saved as CSV. Much lighter to analyze than going through all simulations folders every time.
#'
#' @param inpath Where the simulation outputs are to be found.
#' @param vars What variables through time to extract from simulation output data files?
#' @param params What parameter values to record for each simulation?
#' @return A data frame with individual simulationa in rows, and in columns the variables of interest at each time point, as well as the values of the selected parameters.
#' @export

# Function to extract a set of data through time from the simulations + parameter values
extract_from_simulations <- function(inpath, vars, params) {

  # Go where the simulations are
  homedir <- getwd()
  setwd(inpath)

  # Go through every simulation
  simul_folders <- list.files()
  extracted_data <- lapply(simul_folders, function(curr_simul_folder) {

    message(curr_simul_folder)
    setwd(curr_simul_folder)

    # Check that a single data file is present
    if(length(grep(".dat$", list.files())) != 1) stop(paste("Number of data (.dat) files was not 1 in", curr_simul_folder))

    # Read the data file
    simulation_data <- read.delim(list.files()[grep(".dat$", list.files())])

    # Extract time in a separate object
    time_vector <- simulation_data[, 1]

    # Extract variables of interest through time
    simulation_data <- simulation_data[, vars]

    # Read the parameter file
    if(length(grep("parameters.txt", list.files())) != 1) stop(paste("parameters.txt not found in", curr_simul_folder))
    parameters <- read.delim("parameters.txt", header = F)
    parameters <- as.character(parameters[,1])

    # Extract the parameter values of interest
    parameter_values <- parameters[sapply(params, function(curr_param) grep(curr_param, parameters))]
    parameter_values <- as.numeric(gsub("^.* ", "", parameter_values))

    # Remove burnin, if any
    is_burnin_time <- time_vector < 0
    time_vector <- time_vector[!is_burnin_time]
    simulation_data <- simulation_data[!is_burnin_time,]

    # Return a list of parameter values, variables through time and time vector
    extracted_data <- list(time = time_vector, params = parameter_values, data = simulation_data)

    setwd("..")

    return(extracted_data)

  })

  # When this is done, go through extracted data and record the longest set of time points
  ntimepoints <- sapply(extracted_data, function(curr_simulation_data) length(curr_simulation_data$time))
  timepoints <- extracted_data[[which(ntimepoints == max(ntimepoints))[1]]]$time

  # Go again through all data and count how many timepoints are missing for each one (cases of extinction before end of simul)
  extracted_data <- mapply(function(curr_simulation_data, curr_ntimepoints) {

    if(curr_ntimepoints != max(ntimepoints)) {

      # Add as many NAs as necessary to all the variable for each simulation with missing timepoints
      curr_simulation_data$data <- rbind(curr_simulation_data$data, matrix(NA, ncol = length(vars), nrow = max(ntimepoints) - curr_ntimepoints))

    }

    return(curr_simulation_data)

  }, extracted_data, ntimepoints, SIMPLIFY = F)

  # Prepare column names
  colnames <- paste(rep(vars, length(timepoints)), rep(timepoints, each = length(vars)), sep = "_")

  # Build a big dataset with simulations in rows and variables in columns
  extracted_data <- lapply(extracted_data, function(curr_simulation_data) {

    # One row per simulation
    curr_simulation_data$data <- c(t(as.matrix(curr_simulation_data$data)))

    return(curr_simulation_data)

  })

  # Extract parameter values
  extracted_params <- t(sapply(extracted_data, function(curr_simulation_data) {

    curr_simulation_data$params

  }))
  colnames(extracted_params) <- params

  # Assemble
  extracted_data <- do.call("rbind", lapply(extracted_data, function(curr_simulation_data) curr_simulation_data$data))
  extracted_data <- as.data.frame(extracted_data)
  colnames(extracted_data) <- colnames
  extracted_data <- cbind(extracted_data, extracted_params)
  extracted_data$path <- paste(getwd(), simul_folders, sep = "/")

  # Back to home directory
  setwd(homedir)

  # Return the dataset
  return(extracted_data)

}
