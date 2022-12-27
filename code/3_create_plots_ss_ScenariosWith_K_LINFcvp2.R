# create plots ----

# load pkgs, set options -----
library(r4ss)
source(file.path("code", "SS_run_functions.R"))
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)

# fixed values ----
mydir <- getwd()
outer_folder <- file.path(mydir, "Scenarios")
outer_folder_output <- file.path(mydir, "output", basename(outer_folder))
cases <- list.dirs(outer_folder_output, full.names = FALSE, recursive = FALSE)
run_date <-  "2022_10_04"

Rdata_folder <- file.path("Rdata_output", basename(outer_folder))

# read in csv files ----

csv_list <- lapply(cases, function(icase, outer_folder) {
  mydir.dat <- file.path(outer_folder, icase)
  out_ab <- file.path(mydir.dat, '../ResultsSSab')
  out_pl <- file.path(mydir.dat, '../ResultsSSpl')
  partable_plat <- read.csv(file.path(out_pl, "SS_parameters.csv"))
  partable_noplat <- read.csv(file = file.path(out_ab, "SS_parameters.csv"))
  recruits_plat <- read.csv(file = file.path(out_pl, "SS_recruitment.csv"))
  recruits_noplat <- read.csv(file.path(out_ab, "SS_recruitment.csv"))
  F_plat <- read.csv(file.path(out_pl, "SS_exploitation.csv"))
  F_noplat <- read.csv(file.path(out_ab, "SS_exploitation.csv"))
  N30plus_plat <- read.csv(file.path(out_pl, "SS_numbers30plus.csv"))
  N30plus_noplat <- read.csv(file.path(out_ab, "SS_numbers30plus.csv"))
  B30plus_plat <- read.csv(file.path(out_pl, "SS_biomass30plus.csv"))
  B30plus_noplat <- read.csv(file.path(out_ab, "SS_biomass30plus.csv"))
  plat_list <- list(partable = partable_plat,
                    recruits = recruits_plat, 
                    F_val = F_plat, 
                    N30plus = N30plus_plat, 
                    B30plus = B30plus_plat)
  noplat_list <- list(partable = partable_noplat,
                    recruits = recruits_noplat, 
                    F_val = F_noplat, 
                    N30plus = N30plus_noplat, 
                    B30plus = B30plus_noplat)
  val_list <- list(platoons = plat_list,
                   no_platoons = noplat_list)
  val_list
}, outer_folder = outer_folder)
names(csv_list) <- cases
# look at iterations that didn't converge ----
# TODO: remove iterations that didnt converge
# also look at these data sets and try to diagnose the issues; is it something with
# the data set? Something with the run? It is only a few iterations for each of the
# scenarios at most, so probably not worth spending a ton of time on.


partbl_df_plat <- mapply(make_plotting_df,
                         scen_list = csv_list, scen_name = names(csv_list), 
                         MoreArgs = list(platoons = TRUE, metric = "partable"),
                         SIMPLIFY = FALSE)
partbl_df_noplat <- mapply(make_plotting_df,
                           scen_list = csv_list, scen_name = names(csv_list), 
                           MoreArgs = list(platoons = FALSE, metric = "partable"),
                           SIMPLIFY = FALSE)
partbl_df_plat <- do.call(rbind, partbl_df_plat)
partbl_df_noplat <- do.call(rbind, partbl_df_noplat)
partbl_df <- rbind(partbl_df_plat, partbl_df_noplat)

if(all(partbl_df$converged == TRUE)) {
  message("All model runs converged")
} else {
  message("Not all model runs converged")
}

# remove non converged runs from partbl
partbl_df <- partbl_df[partbl_df$converged == TRUE, ]

# create table so can remove non converged runs from the other datasets.
not_converged <- partbl_df[partbl_df$converged == FALSE, c("run", "scen", "platoons")]
print(nrow(not_converged))

# partable, look at estimates -----
partbl_long <- gather(partbl_df, key = "parameter", value = "value", 4:9)

ggplot(partbl_long, aes(x = scen, y = value)) +
  geom_boxplot(aes(fill = platoons))+
  scale_fill_brewer(palette = "Set2")+
  facet_wrap(~parameter, scales = "free_y")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90))

ggplot(partbl_df, aes(x = K, y = Linf_mm)) +
  geom_point(aes(color = platoons))+
  scale_color_brewer(palette = "Set2")+
  facet_wrap(~scen)+
  theme_classic()
# KD plots, F ----
F_df_plat <- mapply(make_plotting_df,
                    scen_list = csv_list, scen_name = names(csv_list), 
                    MoreArgs = list(platoons = TRUE, metric = "F_val"),
                    SIMPLIFY = FALSE)
F_df_noplat <- mapply(make_plotting_df,
                       scen_list = csv_list, scen_name = names(csv_list), 
                       MoreArgs = list(platoons = FALSE, metric = "F_val"),
                       SIMPLIFY = FALSE)
F_df_plat <- do.call(rbind, F_df_plat)
F_df_noplat <- do.call(rbind, F_df_noplat)
F_df <- rbind(F_df_plat, F_df_noplat)

iter_cols <- grep("replist", colnames(F_df))
F_df_long <- gather(F_df, "iteration", "F_val", all_of(iter_cols)) %>% 
               separate(col = iteration, into = c(NA, "run"), sep = "list", 
                        remove = TRUE)
F_df_long$run <- as.integer(F_df_long$run)
# remove unconverged runs
F_df_long <- anti_join(F_df_long, not_converged)
F_df_long$run <- as.character(F_df_long$run)

ggplot(F_df_long, aes(x = Yr, y = F_val)) +
  geom_line(aes(color = platoons, linetype = run)) +
  facet_wrap(~scen) +
  scale_linetype_manual(values = rep("solid", times = length(iter_cols))) +
  scale_color_brewer(palette = "Set2")+
  theme_classic()

ggplot(F_df_long, aes(x = Yr, y = F_val)) +
  geom_smooth(aes(color = platoons)) +
  facet_wrap(~scen) +
  scale_color_brewer(palette = "Set2")+
  theme_classic()

# KD plots, N ----

N_df_plat <- mapply(make_plotting_df,
                    scen_list = csv_list, scen_name = names(csv_list), 
                    MoreArgs = list(platoons = TRUE, metric = "N30plus"),
                    SIMPLIFY = FALSE)
N_df_noplat <- mapply(make_plotting_df,
                      scen_list = csv_list, scen_name = names(csv_list), 
                      MoreArgs = list(platoons = FALSE, metric = "N30plus"),
                      SIMPLIFY = FALSE)
N_df_plat <- do.call(rbind, N_df_plat)
N_df_noplat <- do.call(rbind, N_df_noplat)
N_df <- rbind(N_df_plat, N_df_noplat)

iter_cols <- grep("run", colnames(N_df))
N_df_long <- gather(N_df, "iteration", "N_val", all_of(iter_cols)) %>% 
  separate(col = iteration, into = c(NA, "run"), sep = "un", 
           remove = TRUE)
N_df_long$run <- as.integer(N_df_long$run)
# remove unconverged runs
N_df_long <- anti_join(N_df_long, not_converged)
N_df_long$run <- as.character(N_df_long$run)


ggplot(N_df_long, aes(x = Yr, y = N_val)) +
  geom_line(aes(color = platoons, linetype = run)) +
  facet_wrap(~scen) +
  scale_linetype_manual(values = rep("solid", times = length(iter_cols))) +
  scale_color_brewer(palette = "Set2")+
  theme_classic()

ggplot(N_df_long, aes(x = Yr, y = N_val)) +
  geom_smooth(aes(color = platoons)) +
  facet_wrap(~scen) +
  scale_color_brewer(palette = "Set2")+
  theme_classic()

# Biomass ---
B_df_plat <- mapply(make_plotting_df,
                    scen_list = csv_list, scen_name = names(csv_list), 
                    MoreArgs = list(platoons = TRUE, metric = "B30plus"),
                    SIMPLIFY = FALSE)
B_df_noplat <- mapply(make_plotting_df,
                      scen_list = csv_list, scen_name = names(csv_list), 
                      MoreArgs = list(platoons = FALSE, metric = "B30plus"),
                      SIMPLIFY = FALSE)
B_df_plat <- do.call(rbind, B_df_plat)
B_df_noplat <- do.call(rbind, B_df_noplat)
B_df <- rbind(B_df_plat, B_df_noplat)

iter_cols <- grep("run", colnames(B_df))
B_df_long <- gather(B_df, "iteration", "B_val", all_of(iter_cols)) %>% 
  separate(col = iteration, into = c(NA, "run"), sep = "un", 
           remove = TRUE)
B_df_long$run <- as.integer(B_df_long$run)
# remove unconverged runs
B_df_long <- anti_join(B_df_long, not_converged)
B_df_long$run <- as.character(B_df_long$run)

ggplot(B_df_long, aes(x = Yr, y = B_val)) +
  geom_line(aes(color = platoons, linetype = run)) +
  facet_wrap(~scen) +
  scale_linetype_manual(values = rep("solid", times = length(iter_cols))) +
  scale_color_brewer(palette = "Set2")+
  theme_classic()

ggplot(B_df_long, aes(x = Yr, y = B_val)) +
  geom_smooth(aes(color = platoons)) +
  facet_wrap(~scen) +
  scale_color_brewer(palette = "Set2")+
  theme_classic()

# recruits ----

recruits_df_plat <- mapply(make_plotting_df,
                    scen_list = csv_list, scen_name = names(csv_list), 
                    MoreArgs = list(platoons = TRUE, metric = "recruits"),
                    SIMPLIFY = FALSE)
recruits_df_noplat <- mapply(make_plotting_df,
                      scen_list = csv_list, scen_name = names(csv_list), 
                      MoreArgs = list(platoons = FALSE, metric = "recruits"),
                      SIMPLIFY = FALSE)
recruits_df_plat <- do.call(rbind, recruits_df_plat)
recruits_df_noplat <- do.call(rbind, recruits_df_noplat)
recruits_df <- rbind(recruits_df_plat, recruits_df_noplat)

iter_cols <- grep("replist", colnames(recruits_df))
recruits_df_long <- gather(recruits_df, "iteration", "recruits", all_of(iter_cols)) %>% 
  separate(col = iteration, into = c(NA, "run"), sep = "list", 
           remove = TRUE)
# remove unconverged runs
recruits_df_long$run <- as.integer(recruits_df_long$run)
recruits_df_long <- anti_join(recruits_df_long, not_converged)
recruits_df_long$run <- as.character(recruits_df_long$run)

ggplot(recruits_df_long, aes(x = Yr, y = recruits)) +
  geom_line(aes(color = platoons, linetype = run)) +
  facet_wrap(~scen) +
  scale_linetype_manual(values = rep("solid", times = length(iter_cols))) +
  scale_color_brewer(palette = "Set2")+
  theme_classic()

ggplot(recruits_df_long, aes(x = Yr, y = recruits)) +
  geom_smooth(aes(color = platoons)) +
  facet_wrap(~scen) +
  scale_color_brewer(palette = "Set2")+
  theme_classic()

# TODO: make some RE plots ----
# read in true values (skipped 7 lines include parameter values).
true_vals <- lapply(cases,
       function(x) {
         read.table(file.path(outer_folder, x, "True_IBM_Values.TRU"), 
         skip = 7, header = TRUE)
       })

matplot(true_vals[[1]]$iyear, true_vals[[1]]$StartYrBgt30)
