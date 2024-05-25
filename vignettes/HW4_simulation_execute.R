rm(list=ls())
set.seed(10)
library(UWBiost561)

imp_numbers <- 1:25
trials <- 2
density_vec <- c(0.1,0.2)

level_trial_list <- UWBiost561::simulation(density_vec, trials, imp_numbers)
names(level_trial_list) <- paste0("clique edge density:", density_vec)
date_of_run <- Sys.time()
session_info <- devtools::session_info()

save(level_trial_list, # save your results
     density_vec, # save which alphas you used (for convenience)
     date_of_run, session_info,
     file = "~/HW4_simulation.RData")
