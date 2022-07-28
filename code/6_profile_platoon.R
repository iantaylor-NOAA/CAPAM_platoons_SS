# profile over Platoon_within/between_stdev_ratio (no read if N_platoons=1)
# RM realized this can make a big difference, so may wan to set it differently for each scenario?

# Why is q fixed? where does this value come from? is it assumed, or was it calculated from the IBM true values?
# 

# take 1 of each model for the scenarios and profile over a range of platoon_within/between values to see how it affect NLL and 

# load packages, set options ----
# devtools::install_github("r4ss/SSutils")
library(SSutils) # to run in parallel
library(r4ss)
library(ggplot2)

mydir <- getwd()
source(file.path("code", "SS_run_functions.R"))

# set fixed values ----
accuage <- 24
yrs <- 1990:2019

# total number of sim runs per case
n <- 1 
# vector of cases
outer_folder <- file.path(mydir, "Scenarios")
# note: expecting there to be 4 cases.
cases <- list.dirs(outer_folder, full.names = FALSE, recursive = FALSE)

ss_name <- "ss_3.30.19.01.exe" # this is in each model folder or in the path.
run_date <- "2022_07_07"

dir.create("output_profile")
outer_folder_output <- file.path("output_profile", basename(outer_folder))
dir.create(outer_folder_output)

# vector of platoon ratio number
vec_of_plat_ratio <- seq(0.2, 1.6, by = 0.2)


# setup for run ----
for (icase in cases) {
  mydir.dat <- file.path(outer_folder, icase)
  dir.create(file.path(outer_folder_output, icase))
  mydir_today_plat <- file.path(outer_folder_output, icase, paste0("runs_plats_", run_date))
  dir.template_current <- file.path('CAPAM_platoons_template_current')
  
  # read the OM files (output from IBM)
  agelen <- read.table(file.path(mydir.dat, 'AGE-LENGTH41.OUT'),
                       skip = 2, header = TRUE)
  cwe <- read.table(file.path(mydir.dat, 'CwEByMonth.OUT'),
                    skip = 1, header = TRUE)
  true <- read.table(file.path(mydir.dat, 'True_IBM_Values.TRU'),
                     skip = 7, header = TRUE)
  
  dir.create(mydir_today_plat)

  if (icase %in% grep("A_IIa_Fp4", cases, value = TRUE)) {
    # for baseline, want to use init F
    build_models(run = 1:n, updatedat = TRUE, dir = mydir_today_plat, 
      use_initF = TRUE, dir.template = dir.template_current, agelen = agelen, 
      cwe = cwe, M_val = 0.1, CV_vals = c(0.1, 0.1)) # based on what we were told the setting in the IBM was...
  }
  if (icase %in% grep("B_IIa_Fp4", cases, value = TRUE)) {
    # for baseline, want to use init F
    build_models(run = 1:n, updatedat = TRUE, dir = mydir_today_plat, 
      use_initF = TRUE, dir.template = dir.template_current, agelen = agelen, 
      cwe = cwe, M_val = 0.1, CV_vals = c(0.2, 0.2)) # based on what we were told the setting in the IBM was...
  }
  if (icase %in% grep("A_IIa_1WayTrip", cases, value = TRUE)) {
    build_models(run = 1:n, updatedat = TRUE, dir = mydir_today_plat,
                 use_initF = FALSE, dir.template = dir.template_current, 
                 agelen = agelen, 
                 cwe = cwe, M_val = 0.1, CV_vals = c(0.1, 0.1)) #base on what we were told the setting in the IBM was
  }
  if (icase %in% grep("B_IIa_1WayTrip", cases, value = TRUE)) {
    build_models(run = 1:n, updatedat = TRUE, dir = mydir_today_plat,
                 use_initF = FALSE, dir.template = dir.template_current, 
                 agelen = agelen, 
                 cwe = cwe, M_val = 0.1, CV_vals = c(0.2, 0.2)) #base on what we were told the setting in the IBM was
  }
}

# Run models ----
for (icase in cases) { 
  mydir_today_plat <- file.path(outer_folder_output, icase, paste0("runs_plats_", run_date))
  SSutils::run_SS_models(dirvec = dir(mydir_today_plat, full.names = TRUE)[seq_len(n)],
                      systemcmd = TRUE, skipfinished = FALSE,
                      extras = "-nox", model = ss_name, exe_in_path = TRUE,
                      intern = TRUE)
}

# see which failed
test <- lapply(cases, function(icase) {
  mydir_today_plat <- file.path(outer_folder_output, icase, paste0("runs_plats_", run_date))
  file.exists(file.path(mydir_today_plat, "CAPAM_platoons_run001", "control.ss_new"))
})

# see max gradients
max_grad <- lapply(cases, function(icase) {
  mydir_today_plat <- file.path(outer_folder_output, icase, paste0("runs_plats_", run_date))
  tmp <- readLines(file.path(mydir_today_plat, "CAPAM_platoons_run001", "ss.par"))
  tmp[1]
})


# Run profile for each of the 4 scenarios ----

for(icase in cases) {
    plat_scen_path <- file.path(outer_folder_output, icase, paste0("runs_plats_", run_date))
    dat <- r4ss::SS_readdat(file.path(plat_scen_path, "CAPAM_platoons_run001", 
      "platoons_data.ss"))
    lapply(vec_of_plat_ratio, function(ratio, scen_path, dat, ss_name) {
        # Copy input files to new folder

        dir_new_ratio <- file.path(scen_path, paste0("profile_", as.character(ratio)))
        r4ss::copy_SS_inputs(dir.old = file.path(scen_path,  "CAPAM_platoons_run001"), 
          dir.new = dir_new_ratio)
        # Modify the ctl file
        ctl <- r4ss::SS_readctl(file.path(dir_new_ratio, "platoons_control.ss"),
         use_datlist = T, datlist = dat)
        ctl$sd_ratio <- ratio
        r4ss::SS_writectl(ctl, outfile = file.path(dir_new_ratio, "platoons_control.ss"),
         overwrite = TRUE)
        # Run the model
          SSutils::run_SS_models(dirvec = dir_new_ratio,
                      systemcmd = TRUE, skipfinished = FALSE,
                      extras = "-nox", model = ss_name, exe_in_path = TRUE,
                      intern = TRUE)
          return(0)
    }, scen_path = plat_scen_path, dat = dat, ss_name = ss_name)
}

# get results of profile
results <- vector(mode = "list", length = length(cases))
i <- 0
for (icase in cases) { 
  plat_scen_path <- file.path(outer_folder_output, icase, paste0("runs_plats_", run_date))
  i <- i + 1
  tot_vec_of_plat <- vec_of_plat_ratio
  results[[i]] <- lapply(tot_vec_of_plat, function(ratio, scen_path, case) {
    if(file.exists(file.path(scen_path, paste0("profile_", as.character(ratio)), "control.ss_new"))) {
      out <- r4ss::SS_output(file.path(scen_path, paste0("profile_", ratio)))
      l_at_age_max <- out$parameters["L_at_Amax_Fem_GP_1","Value"]
      like <- out$likelihoods_used["TOTAL","values"]
      out_df <- data.frame(case = case, ratio = ratio, 
        l_at_age_max = l_at_age_max, likelihood = like)
    } else {
      out_df <- data.frame(case = case, ratio = ratio,
        l_at_age_max = as.numeric(NA), likelihood = as.numeric(NA))
    }
    return(out_df)
  }, scen_path = plat_scen_path, case = icase)
}
tmp <- do.call("rbind", results)
df_results <- do.call("rbind", tmp)

write.csv(df_results, file.path(outer_folder_output, "profile_results.csv"))

ggplot(df_results, aes(x = ratio, y = likelihood))+
  geom_line(aes(color = case))+
  geom_point(aes(color = case))+
  theme_classic()
ggsave(file.path(outer_folder_output, "profile_likelihood.png"))

ggplot(df_results, aes(x = ratio, y = l_at_age_max))+
  geom_line(aes(color = case))+
  geom_point(aes(color = case))+
  theme_classic()
  ggsave(file.path(outer_folder_output, "profile_l_at_age_max.png"))
