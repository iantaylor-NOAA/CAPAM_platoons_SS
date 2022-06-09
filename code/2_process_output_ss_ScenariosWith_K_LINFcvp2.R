# Process otupt from the simulations run in 1_

# to do: run this. 2 runs failed, need to look at why and see if there is a way
# to prevent or if runs should just be thrown out?
# do this tomorrow.

# edit to work with new location.

# load pkgs, set options ----
library(r4ss)
source(file.path("code", "SS_run_functions.R"))

# fixed values ----
mydir <- getwd()
outer_folder <- file.path(mydir, "Scenarios")
outer_folder_output <- file.path(mydir, "output", basename(outer_folder))
cases <- list.dirs(outer_folder_output, full.names = FALSE, recursive = FALSE)
run_date <-  "2021_06_24"

Rdata_folder <- file.path("Rdata_output", basename(outer_folder))

# load saved output, create csvs ----
for(icase in cases) {
  mydir.dat <- file.path(outer_folder, icase)
  out_ab <- file.path(mydir.dat, '../ResultsSSab')
  out_pl <- file.path(mydir.dat, '../ResultsSSpl')
  load(file.path(Rdata_folder, paste0('case', icase,'_stuff_', run_date, ".Rdata")))


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
  print(icase)
  print(table(partable1$converged))
  print(table(partable2$converged))
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

