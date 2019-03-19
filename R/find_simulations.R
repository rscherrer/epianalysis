#' Find simulations
#'
#' @param speciation_cube_data A data frame containing trajectories in speciation cube for all simulations (in rows).
#' @param path If \code{speciation_cube_data} not provided, the path to where the simulation data are to be found.
#' @param pars A list of parameter values. The names in the list should be the names of the parameters as they appear in the parameters.txt file.
#'
#' @return A logical vector indicated simulations that match the search.
#' @export

# Find sets of simulations based on parameter values
find_simulations <- function(speciation_cube_data, path = ".", pars = list(sel_coeff_ecol = 0.8, habitat_asymmetry = 1.0)) {

  if(!missing(speciation_cube_data)) {
    if(all(names(pars) %in% colnames(speciation_cube_data))) {

      par_indices <- sapply(names(pars), function(curr_par) {
        grep(curr_par, colnames(speciation_cube_data))
      })

      is_good <- mapply(function(curr_index, curr_par_value) {
        speciation_cube_data[,curr_index] == curr_par_value
      }, par_indices, pars, SIMPLIFY = F)
      is_good <- apply(do.call("cbind", is_good), 1, function(x) all(x))

      return(is_good)
    }
  }

  homedir <- getwd()
  setwd(path)

  is_good <- sapply(list.files(), function(curr_simulation_folder) {

    setwd(curr_simulation_folder)
    if(length(grep("parameters.txt", list.files())) == 0) stop(paste("no parameter file found in folder", curr_simulation_folder))
    params <- unlist(read.delim(list.files()[grep("parameters.txt", list.files())], header = F))
    par_indices <- sapply(names(pars), function(curr_par) grep(curr_par, params))
    par_values <- as.numeric(sapply(strsplit(as.character(params[par_indices]), " "), "[", 2))
    setwd("..")
    if(all(pars == par_values)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })

  setwd(homedir)
  return(is_good)

}
