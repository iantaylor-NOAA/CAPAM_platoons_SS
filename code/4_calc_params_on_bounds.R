# load pkgs, set options -----
library(r4ss)
source(file.path("code", "SS_run_functions.R"))
library(furrr)
library(dplyr)
library(tidyr)
library(purrr)

# fixed values ----
mydir <- getwd()
outer_folder <- file.path(mydir, "Scen_K_LINFcvp2")
outer_folder_output <- file.path(mydir, "output", basename(outer_folder))
cases <- list.dirs(outer_folder_output, full.names = FALSE, recursive = FALSE)
run_date <-  "2021_06_24"

Rdata_folder <- file.path("Rdata_output", basename(outer_folder))

# look for parameters on bounds ----
# Note that this step can take a while to run, beware!


mod_paths_platoon <- file.path("output", basename(outer_folder), cases, paste0("runs_plats_", run_date))
mod_paths_platoon <- unlist(lapply(mod_paths_platoon, function(x) list.dirs(x, recursive = FALSE)))
mod_paths_no_platoon <-  
  file.path("output", basename(outer_folder), cases, paste0("runs_no_plats_", run_date))
mod_paths_no_platoon <- unlist(lapply(mod_paths_no_platoon, 
                                      function(x) list.dirs(x, recursive = FALSE)))
mod_paths_all <- c(mod_paths_platoon, mod_paths_no_platoon)

future::plan(multisession)
all_params_on_bounds <- furrr::future_map(mod_paths_all, ~get_params_on_bounds(.x))
all_params_on_bounds_vec <- unlist(all_params_on_bounds)
names(all_params_on_bounds_vec) <- mod_paths_all
unique(all_params_on_bounds_vec)

write.csv(all_params_on_bounds_vec, paste0("all_params_on_bounds_", run_date, ".csv"))

# About 6 models have params on bounds still. perhaps ok?
