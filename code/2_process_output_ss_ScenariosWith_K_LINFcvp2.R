# Process otupt from the simulations run in 1_run_sims...

# load pkgs, set options ----
source(file.path("code", "SS_run_functions.R"))

# fixed values ----
mydir <- getwd()
outer_folder <- file.path(mydir, "Scenarios")
outer_folder_output <- file.path(mydir, "output", basename(outer_folder))
cases <- list.dirs(outer_folder_output, full.names = FALSE, recursive = FALSE)
run_date <- "2022_10_04"
#run_date <- "2022_12_27"
saved <- TRUE

Rdata_folder <- file.path("Rdata_output", basename(outer_folder))

# load saved output, create csvs ----
#for (icase in cases) {
#for (icase in cases[grepl("L9545", cases)]) { # subset for those with wider selectivity (Dec 2022 re-run)
for (icase in cases[!grepl("L9545", cases)]) { # subset for those with wider selectivity (Oct 2022 run)
  out_ab <- file.path(outer_folder, paste0("ResultsSSab_", run_date), icase)
  out_pl <- file.path(outer_folder, paste0("ResultsSSpl_", run_date), icase)
  dir.create(out_ab, showWarnings = FALSE, recursive = TRUE)
  dir.create(out_pl, showWarnings = FALSE, recursive = TRUE)
  if (saved) {
    load(file.path(Rdata_folder, paste0("case", icase, "_stuff_", run_date, ".Rdata")))
  } else {
    outer_folder <- file.path(mydir, "Scenarios")
    outer_folder_output <- file.path("output", basename(outer_folder))
    mydir_today_plat <- file.path(outer_folder_output, icase, paste0("runs_plats_", run_date))
    mydir_today_no_plat <- file.path(outer_folder_output, icase, paste0("runs_no_plats_", run_date))
    # summarize output from both models----
    # get the output and summarize it (1 is with platoons, 2 is without)
    modlist_plat <- tryCatch(r4ss::SSgetoutput(
      dirvec = dir(mydir_today_plat, full.names = TRUE)[seq_len(n)],
      getcovar = FALSE
    ), error = function(e) print(e))
    modsum1 <- tryCatch(r4ss::SSsummarize(modlist_plat), error = function(e) print(e))

    modlist_no_plat <- tryCatch(r4ss::SSgetoutput(
      dirvec = dir(mydir_today_no_plat, full.names = TRUE)[seq_len(n)],
      getcovar = FALSE
    ), error = function(e) print(e))
    modsum2 <- tryCatch(r4ss::SSsummarize(modlist_no_plat), error = function(e) print(e))
    save(modlist_plat, modlist_no_plat, modsum1, modsum2,
      file = file.path(file.path(mydir, Rdata_output_folder, paste0("case", icase, "_stuff_", run_date, ".Rdata")))
    )
  }

  # summarize 30cm+ fish across models
  modsum1 <- add30plus(modsum1, modlist_plat)
  modsum2 <- add30plus(modsum2, modlist_no_plat)
  # summarize 40cm+ fish (added July 2023)
  modsum1 <- add40plus(modsum1, modlist_plat)
  modsum2 <- add40plus(modsum2, modlist_no_plat)

  # format the output
  partable1 <- format_params(modsum1)
  partable2 <- format_params(modsum2)

  # get the F rates
  F_rates1 <- get_F_rates(modlist_plat)
  F_rates2 <- get_F_rates(modlist_no_plat)

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
    row.names = FALSE
  )
  write.csv(partable2,
    file = file.path(out_ab, "SS_parameters.csv"),
    row.names = FALSE
  )
  write.csv(modsum1$recruits,
    file = file.path(out_pl, "SS_recruitment.csv"),
    row.names = FALSE
  )
  write.csv(modsum2$recruits,
    file = file.path(out_ab, "SS_recruitment.csv"),
    row.names = FALSE
  )
  write.csv(modsum1$Fvalue, # exploitation
    file = file.path(out_pl, "SS_exploitation.csv"),
    row.names = FALSE
  )
  write.csv(modsum2$Fvalue, # exploitation
    file = file.path(out_ab, "SS_exploitation.csv"),
    row.names = FALSE
  )  
  write.csv(
    F_rates1, # instantaneous F
    file = file.path(out_pl, "SS_fishing_mortality.csv"),
    row.names = FALSE
  )
  write.csv(#modsum2$Fvalue, # exploitation
    F_rates2, # instantaneous F
    file = file.path(out_ab, "SS_fishing_mortality.csv"),
    row.names = FALSE
  )
  write.csv(modsum1$N30plus,
    file = file.path(out_pl, "SS_numbers30plus.csv"),
    row.names = FALSE
  )
  write.csv(modsum2$N30plus,
    file = file.path(out_ab, "SS_numbers30plus.csv"),
    row.names = FALSE
  )
  write.csv(modsum1$B30plus,
    file = file.path(out_pl, "SS_biomass30plus.csv"),
    row.names = FALSE
  )
  write.csv(modsum2$B30plus,
    file = file.path(out_ab, "SS_biomass30plus.csv"),
    row.names = FALSE
  )
  # 40cm plus added July 2023
  write.csv(modsum1$N40plus,
    file = file.path(out_pl, "SS_numbers40plus.csv"),
    row.names = FALSE
  )
  write.csv(modsum2$N40plus,
    file = file.path(out_ab, "SS_numbers40plus.csv"),
    row.names = FALSE
  )
  write.csv(modsum1$B40plus,
    file = file.path(out_pl, "SS_biomass40plus.csv"),
    row.names = FALSE
  )
  write.csv(modsum2$B40plus,
    file = file.path(out_ab, "SS_biomass40plus.csv"),
    row.names = FALSE
  )


}
