# Process otupt from the simulations run in 1_

# to do: run this. 2 runs failed, need to look at why and see if there is a way
# to prevent or if runs should just be thrown out?
# do this tomorrow.

# edit to work with new location.

# load pkgs, set options ----
#library(r4ss)
source(file.path("code", "SS_run_functions.R"))

# fixed values ----
mydir <- getwd()
outer_folder <- file.path(mydir, "Scenarios")
outer_folder_output <- file.path(mydir, "output", basename(outer_folder))
cases <- list.dirs(outer_folder_output, full.names = FALSE, recursive = FALSE)
run_date <- "2022_10_04"
saved <- TRUE

Rdata_folder <- file.path("Rdata_output", basename(outer_folder))

# load saved output, create csvs ----
for(icase in cases) {
  mydir.dat <- file.path(outer_folder, icase)
  out_ab <- file.path(mydir.dat, '..', 'ResultsSSab', icase)
  out_pl <- file.path(mydir.dat, '..', 'ResultsSSpl', icase)
  dir.create(out_ab, showWarnings = FALSE)
  dir.create(out_pl, showWarnings = FALSE)
  if (saved) {
    load(file.path(Rdata_folder, paste0('case', icase,'_stuff_', run_date, ".Rdata")))
  } else {
    outer_folder <- file.path(mydir, "Scenarios")
    outer_folder_output <- file.path("output", basename(outer_folder))
    mydir_today_plat <- file.path(outer_folder_output, icase, paste0("runs_plats_", run_date))
    mydir_today_no_plat <- file.path(outer_folder_output, icase, paste0("runs_no_plats_", run_date))
    # summarize output from both models----
    # get the output and summarize it (1 is with platoons, 2 is without)
    modlist_plat <- tryCatch(r4ss::SSgetoutput(dirvec = dir(mydir_today_plat, full.names = TRUE)[seq_len(n)],
                            getcovar = FALSE), error = function (e) print(e))
    modsum1 <- tryCatch(r4ss::SSsummarize(modlist_plat), error = function (e) print(e))
    
    modlist_no_plat <- tryCatch(r4ss::SSgetoutput(dirvec = dir(mydir_today_no_plat, full.names = TRUE)[seq_len(n)],
                            getcovar = FALSE), error = function (e) print(e))
    modsum2 <- tryCatch(r4ss::SSsummarize(modlist_no_plat), error = function (e) print(e))
    save(modlist_plat, modlist_no_plat, modsum1, modsum2,
         file = file.path(file.path(mydir, Rdata_output_folder, paste0('case', icase, '_stuff_', run_date, '.Rdata'))))
  }

  # summarize 30cm+ fish across models
  modsum1 <- add30plus(modsum1, modlist_plat)
  modsum2 <- add30plus(modsum2, modlist_no_plat)
  
  # format the output
  partable1 <- format_params(modsum1)
  partable2 <- format_params(modsum2)
  
  ## SSsummary_platoons <- modsum1
  ## SSsummary_NOplatoons <- modsum2
  ## save(SSsummary_platoons, SSsummary_NOplatoons,
  ##      file = file.path(file.path(mydir, 'SSsummaries_13Nov2020.Rdata')))
  
  # check model convergence
  cat("converge for models:")
  print(icase)
  print(partable1$converged)
  print(partable2$converged)
  # turn off output to CSV files

  
  # write to files ----

  write.csv(partable1,
            file = file.path(out_pl, "SS_parameters.csv"),
            row.names = FALSE)
  write.csv(partable2,
            file = file.path(out_ab, "SS_parameters.csv"),
            row.names = FALSE)
  write.csv(modsum1$recruits,
            file = file.path(out_pl, "SS_recruitment.csv"),
            row.names = FALSE)
  write.csv(modsum2$recruits,
            file = file.path(out_ab, "SS_recruitment.csv"),
            row.names = FALSE)
  write.csv(modsum1$Fvalue,
            file = file.path(out_pl, "SS_exploitation.csv"),
            row.names = FALSE)
  write.csv(modsum2$Fvalue,
            file = file.path(out_ab, "SS_exploitation.csv"),
            row.names = FALSE)
  write.csv(modsum1$N30plus,
            file = file.path(out_pl, "SS_numbers30plus.csv"),
            row.names = FALSE)
  write.csv(modsum2$N30plus,
            file = file.path(out_ab, "SS_numbers30plus.csv"),
            row.names = FALSE)
  write.csv(modsum1$B30plus,
            file = file.path(out_pl, "SS_biomass30plus.csv"),
            row.names = FALSE)
  write.csv(modsum2$B30plus,
            file = file.path(out_ab, "SS_biomass30plus.csv"),
            row.names = FALSE)
}


