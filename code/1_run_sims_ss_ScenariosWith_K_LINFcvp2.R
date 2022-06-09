# load packages, set options ----
# devtools::install_github("r4ss/SSutils")
library(SSutils) # to run in parallel
library(r4ss)

mydir <- getwd()
source(file.path("code", "SS_run_functions.R"))

# set fixed values ----
accuage <- 24
yrs <- 1990:2019

# total number of sim runs per case
n <- 2 # to start
# vector of cases
outer_folder <- file.path(mydir, "Scenarios")
# note: expecting there to be 4 cases.
cases <- list.dirs(outer_folder, full.names = FALSE, recursive = FALSE)

ss_name <- "ss_3.30.19.01.exe" # this is in each model folder or in the path.
run_date <- format(Sys.Date(), "%Y_%m_%d")

Rdata_output_folder <- file.path("Rdata_output", basename(outer_folder))
dir.create("Rdata_output")
dir.create(Rdata_output_folder)

dir.create("output")
outer_folder_output <- file.path("output", basename(outer_folder))
dir.create(outer_folder_output)

# setup for run ----
for (icase in cases) {
  mydir.dat <- file.path(outer_folder, icase, "IBMData")
  dir.create(file.path(outer_folder_output, icase))
  mydir_today_plat <- file.path(outer_folder_output, icase, paste0("runs_plats_", run_date))
  mydir_today_no_plat <- file.path(outer_folder_output, icase, paste0("runs_no_plats_", run_date))
  dir.template_current <- file.path('CAPAM_platoons_template_current')
  # read the OM files (output from IBM)
  agelen <- read.table(file.path(mydir.dat, 'AGE-LENGTH41.OUT'),
                       skip = 2, header = TRUE)
  cwe <- read.table(file.path(mydir.dat, 'CwEByMonth.OUT'),
                    skip = 1, header = TRUE)
  true <- read.table(file.path(mydir.dat, 'True_IBM_Values.TRU'),
                     skip = 7, header = TRUE)
  
  dir.create(mydir_today_plat)
  dir.create(mydir_today_no_plat)
  # note that A indicates runs with CV = 0.1, while B indicates runs with CV = 0.2
  if (icase %in% grep("A_II_Fp4", cases, value = TRUE)) {
    # for baseline, want to use init F
    build_models(run = 1:n, updatedat = TRUE, dir = mydir_today_plat, 
      use_initF = TRUE, dir.template = dir.template_current, agelen = agelen, 
      cwe = cwe, M_val = 0.1, CV_vals = c(0.1, 0.1)) # based on what we were told the setting in the IBM was...
  }
  if (icase %in% grep("B_II_Fp4", cases, value = TRUE)) {
    # for baseline, want to use init F
    build_models(run = 1:n, updatedat = TRUE, dir = mydir_today_plat, 
      use_initF = TRUE, dir.template = dir.template_current, agelen = agelen, 
      cwe = cwe, M_val = 0.1, CV_vals = c(0.2, 0.2)) # based on what we were told the setting in the IBM was...
  }
  if (icase %in% grep("A_II_1WayTrip", cases, value = TRUE)) {
    build_models(run = 1:n, updatedat = TRUE, dir = mydir_today_plat,
                 use_initF = FALSE, dir.template = dir.template_current, 
                 agelen = agelen, 
                 cwe = cwe, M_val = 0.1, CV_vals = c(0.1, 0.1)) #base on what we were told the setting in the IBM was
  }
  if (icase %in% grep("B_II_1WayTrip", cases, value = TRUE)) {
    build_models(run = 1:n, updatedat = TRUE, dir = mydir_today_plat,
                 use_initF = FALSE, dir.template = dir.template_current, 
                 agelen = agelen, 
                 cwe = cwe, M_val = 0.1, CV_vals = c(0.2, 0.2)) #base on what we were told the setting in the IBM was
  }
  dirs1 <- file.path(mydir_today_plat,
                     paste0('run',
                            substring(1000 + seq_len(n), 2)))
  # copy platoons directories to no-platoons directories,then remove platoons
  r4ss::populate_multiple_folders(outerdir.old = mydir_today_plat,
                                  outerdir.new = mydir_today_no_plat,
                                  create.dir = TRUE, 
                                  overwrite = TRUE,
                                  use_ss_new = FALSE,
                                  exe.only = FALSE,
                                  verbose = TRUE)
  dirs2 <- dir(mydir_today_no_plat, full.names = TRUE)
  for(idir in dirs2){
    remove_platoons(idir)
  }
} # end loop over cases

for (icase in cases) { 
  mydir_today_plat <- file.path(outer_folder_output, icase, paste0("runs_plats_", run_date))
  mydir_today_no_plat <- file.path(outer_folder_output, icase, paste0("runs_no_plats_", run_date))
  
  SSutils::run_SS_models(dirvec = dir(mydir_today_plat, full.names = TRUE)[seq_len(n)],
                      systemcmd = TRUE, skipfinished = FALSE,
                      extras = "-nox", model = ss_name, exe_in_path = TRUE,
                      intern = TRUE)
  SSutils::run_SS_models(dirvec = dir(mydir_today_no_plat, full.names = TRUE)[seq_len(n)],
                      systemcmd = TRUE, skipfinished = FALSE,
                      extras = "-nox", model = ss_name, exe_in_path = TRUE,
                      intern = TRUE)
  
  # summarize output ----
  # get the output and summarize it (1 is with platoons, 2 is without)
  modlist_plat <- tryCatch(SSgetoutput(dirvec = dir(mydir_today_plat, full.names = TRUE)[seq_len(n)],
                          getcovar = FALSE), error = function (e) print(e))
  modsum1 <- tryCatch(SSsummarize(modlist_plat), error = function (e) print(e))
  
  modlist_no_plat <- tryCatch(SSgetoutput(dirvec = dir(mydir_today_no_plat, full.names = TRUE)[seq_len(n)],
                          getcovar = FALSE), error = function (e) print(e))
  modsum2 <- tryCatch(SSsummarize(modlist_no_plat), error = function (e) print(e))
  save(modlist_plat, modlist_no_plat, modsum1, modsum2,
       file = file.path(file.path(mydir, Rdata_output_folder, paste0('case', icase, '_stuff_', run_date, '.Rdata'))))
}

