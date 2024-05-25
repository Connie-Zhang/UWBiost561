#' Title
#'
#' @param density_vec a vector of partial clique densities
#' @param trials a vector of trials that we want to run
#' @param imp_numbers a vector of implementation numbers that we want to simulate over
#'
#' @return a list of trials for different partial clique densities, inside each list is a list of results for each implementation
#' @export
#'
#' @examples
simulation <- function(density_vec, trials, imp_numbers){
  stopifnot(length(unique(density_vec))==length(density_vec),
            trials>=2,
            length(imp_numbers)==25)
  level_trial_list <- lapply(density_vec, function(density){
    print(paste("Value of clique edge density:", density))

  # loop over the different trials for this level
    trial_list <- lapply(1:trials, function(trial){
    print(paste("Working on trial:", trial))
    set.seed(trial) # to freeze the randomness of adj_mat

    # generate the data
    data <- UWBiost561::generate_partial_clique(n = 10,
                                                clique_fraction = 0.8,
                                                clique_edge_density = density)
    adj_mat <- data$adj_mat

    # loop over the methods for this trial
    result_list <- lapply(imp_numbers, function(imp_number){
      set.seed(trial) # to freeze the randomness of the method
      cat('*')
      result <- UWBiost561::compute_maximal_partial_clique_master(
        adj_mat = adj_mat,
        alpha = 0.5,
        number = imp_number,
        time_limit = 30
      )


      return(result)
    })
    names(result_list) <- paste("Implementation:", imp_numbers)
    cat("\n")

    return(result_list)
  })
  names(trial_list) <- paste("Trial:", 1:trials)
  print("====")

  return(trial_list)
})}
