# load packages, set options ----
library(r4ss)
mydir <- getwd() #If using Rstudio project.
source("SS_run_functions.R")

# set fixed values (may want to change in the future)
accuage <- 24

# # Missing something here? commenting out seince doesn't seem like tmp 1 or 2 are
# # used elsewhere in the script.
# if(FALSE){
#   #creating dummy mean length-at-age inputs for template
#   tmp <- expand.grid(month = c(3,9), year = 1990:2019)
#   tmp2 <- data.frame(tmp$year, tmp$month, fleet = -1, sex = 0,
#                      part = 0, ageerr = 1, ignore = 999)
#   for(a in 1:accuage){
#     tmp2[paste0("a",a)] <- 1
#   }
#   for(a in 1:accuage){
#     tmp2[paste0("N",a)] <- 100
#   }
# }

yrs <- 1990:2019

# total number of sim runs per case
n <- 2 # to start
# vector of cases

# cases <- c("Baseline_KnifeEdge40cm_Fp4_Mp05",
#            "Baseline_LessSteepSel_L95eq45",
#            "OneWayTrip_FRising10YrsFr0top25_Mp15",
#            "OneWayTrip_LessSteepSel_L95eq45")
cases <- "test"
run_date <- "8June2021" # adjust based on the date of run; append to output.
ss_name <- "ss_3.30.16.exe" # this is in each model folder or in the path.

# setup for run ----

for (icase in 1:4) {
  mydir.dat <- file.path(mydir, 'IBM_data_9June2020')#,
                         # cases[icase],
                         # 'IBMData')
  ## mydir.dat <- file.path(mydir, 'IBM_data_28Oct2020',
  ##                        'Baseline_KnifeEdge40cm_Fp4_Mp05/IBMData')
  ## mydir.dat <- file.path(mydir, 'IBM_data_28Oct2020',
  ##                        'OneWayTrip_FRising10YrsFr0top25_Mp15/IBMData')
  
  mydir.today1 <- file.path(mydir.dat, paste0("runs_with_platoons_", run_date))
  mydir.today2 <- file.path(mydir.dat, paste0("runs_no_platoons_", run_date))
  dir.template_current <- file.path(mydir, 'CAPAM_platoons_template_current')
  dir.template_initF <- file.path(mydir, 'CAPAM_platoons_template_initF')
  
  # read the OM files (output from IBM)
  agelen <- read.table(file.path(mydir.dat, 'AGE-LENGTH41.OUT'),
                       skip = 2, header = TRUE)
  cwe <- read.table(file.path(mydir.dat, 'CwEByMonth.OUT'),
                    skip = 1, header = TRUE)
  true <- read.table(file.path(mydir.dat, 'True_IBM_Values.TRU'),
                     skip = 7, header = TRUE)
  
  # subset to simulation 1 only of the OM
  agelen1 <- agelen[agelen$irun == 1,]
  cwe1 <- cwe[cwe$RUN == 1,]
  
  # source this function
  dir.create(mydir.today1)
  dir.create(mydir.today2)
  
  if (icase %in% 1:2) {
    build_models(run = 1:n, updatedat = TRUE, dir = mydir.today1,
                 dir.template = dir.template_initF)
  }
  if (icase %in% 3:4) {
    build_models(run = 1:n, updatedat = TRUE, dir = mydir.today1,
                 dir.template = dir.template_current)
  }
  
  runs <- 1:n
  dirs1 <- file.path(mydir.today1,
                     paste0('CAPAM_platoons_run',
                            substring(1000 + runs, 2)))
  # copy platoons directories to no-platoons directories and then remove platoons
  r4ss::populate_multiple_folders(outerdir.old = mydir.today1,
                                  outerdir.new = mydir.today2,
                                  create.dir = TRUE, 
                                  overwrite = TRUE,
                                  use_ss_new = FALSE,
                                  exe.only = FALSE,
                                  verbose = TRUE)
  dirs2 <- dir(mydir.today2, full.names = TRUE)
  for(idir in dirs2){
    remove_platoons(idir)
  }
} # end loop over cases


# run the models
for (icase in seq_along(cases)) { 
  mydir.dat <- file.path(mydir, 'IBM_data_9June2020',
                         cases[icase],
                         'IBMData')
  mydir.today1 <- file.path(mydir.dat, paste0("runs_with_platoons_", run_date))
  mydir.today2 <- file.path(mydir.dat, paste0("runs_no_platoons_", run_date))
  
  r4ss::run_SS_models(dirvec = dir(mydir.today1, full.names = TRUE)[1:n],
                      systemcmd = TRUE, skipfinished = FALSE,
                      extras = "-nox", model = ss_name, exe_in_path = TRUE,
                      intern = TRUE)
  r4ss::run_SS_models(dirvec = dir(mydir.today2, full.names = TRUE)[1:n],
                      systemcmd = TRUE, skipfinished = FALSE,
                      extras = "-nox", model = ss_name, exe_in_path = TRUE,
                      intern = TRUE)
  
  # get the output and summarize it (1 is with platoons, 2 is without)
  modlist1 <- SSgetoutput(dirvec = dir(mydir.today1, full.names = TRUE)[1:n],
                          getcovar = FALSE)
  ## # replace run 79 after manual fix to remove 0 observations
  ## modlist1[[79]] <- SS_output(dir(mydir.today1, full.names = TRUE)[79])
  modsum1 <- SSsummarize(modlist1)
  
  modlist2 <- SSgetoutput(dirvec = dir(mydir.today2, full.names = TRUE)[1:n],
                          getcovar = FALSE)
  ## # replace run 79 after manual fix to remove 0 observations
  ## modlist2[[79]] <- SS_output(dir(mydir.today2, full.names = TRUE)[79])
  modsum2 <- SSsummarize(modlist2)
  save(modlist1, modlist2, modsum1, modsum2,
       file = file.path(file.path(mydir, paste0('case', icase, '_stuff_', run_date, '.Rdata'))))
}

# TODO: adapt above for use with new data sets
  
  # load stuff saved above ----
  #TODO: adapt for KD.
    mydir.dat <- file.path(mydir, 'IBM_data_28Oct2020',
                           cases[icase],
                           'IBMData')
    out_ab <- file.path(mydir.dat, '../ResultsSSab')
    out_pl <- file.path(mydir.dat, '../ResultsSSpl')

    load(file.path(file.path(mydir, 'IBM_data_28Oct2020',
                             paste0('case', icase,
                                    '_stuff_4Dec2020.Rdata'))))
    ## load(file.path(mydir, 'stuff_29Oct2020.Rdata'))
    ## load(file.path(mydir, 'SSsummaries_29Oct2020.Rdata'))
    
    # summarize 30cm+ fish across models
    modsum1 <- add30plus(modsum1, modlist1)
    modsum2 <- add30plus(modsum2, modlist2)

    # format the output
    partable1 <- format_params(modsum1)
    partable2 <- format_params(modsum2)

    ## SSsummary_platoons <- modsum1
    ## SSsummary_NOplatoons <- modsum2
    ## save(SSsummary_platoons, SSsummary_NOplatoons,
    ##      file = file.path(file.path(mydir, 'SSsummaries_13Nov2020.Rdata')))

    print(cases[icase])
    print(table(partable1$converged))
    print(table(partable2$converged))
    # turn off output to CSV files
    write <- FALSE
    
    # write to files
    if(write){
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
  
  # p1a <- SS_output('C:/SS/McGarvey/CAPAM_platoons_runs_Oct29/CAPAM_platoons_run001')
  p1a <- modlist1[[1]]
  p1adat <- SS_readdat(file.path(p1a$inputs$dir, p1a$Data_File))
  p1b <- modlist2[[1]]

  # plot illustrating platoons
  png(file.path(mydir, "platoons_run001_length_distribution_v2.png"),
      width = 6.5, height = 5, res = 300, units ='in')
  colvec <- rich.colors.short(6, alpha = 0.9)[-1]
  colvec <- adjustcolor(colvec, offset = c(-0.2, -0.2, -0.2, 0))
  #colvec <- rainbow(5, alpha = 1.0)
  #colvec <- rich.colors.short(4, alpha = 0.6)[c(4,2,1,2,4)]
  plot(0, type = 'n',
       xlim = c(1,15),
       ylim = c(4, 100),
       xaxs = 'i',
       yaxs = 'i',
       xlab = "Age (years)",
       ylab = "Length (mm)",
       axes = FALSE)
  axis(1)
  axis(2, at = seq(0,120,10), labels = 10*seq(0,120,10), las = 1)
  for(i in c(3,2,4,1,5)){
    SSplotAgeMatrix(p1a, slices = i, add = TRUE,
                    col.bars = colvec[i],
                    shift_lo = 2 * (i - 1) / 5,
                    shift_hi = -2 * (5 - i) / 5,
                    scale = 15*c(0.031, 0.237, 0.464, 0.237, 0.031)[i])
  }
  ybins <- seq(0, 120, 2)
  accuage <- p1a$accuage
  col.grid = 'grey90'
  abline(h=ybins, v=0:accuage, col=col.grid, lwd=0.5)
  box()
  dev.off()
  
  
  # plots of SS estimates
  par(mfcol=c(2,3))
  hist(as.numeric(modsum1$pars[modsum1$pars$Label=="L_at_Amax_Fem_GP_1", 1:20]),
       breaks=seq(80,140,4), xlab = "L-inf", main = "5 platoons")
  abline(v = 100, col = 2, lwd = 3, lty = 2)
  hist(as.numeric(modsum2$pars[modsum2$pars$Label=="L_at_Amax_Fem_GP_1", 1:20]),
       breaks=seq(80,140,4), xlab = "L-inf", main = "1 platoon")  
  abline(v = 100, col = 2, lwd = 3, lty = 2)
  hist(as.numeric(modsum1$pars[modsum1$pars$Label=="VonBert_K_Fem_GP_1", 1:20]),
       breaks=seq(0.05,0.15,0.01), xlab = "K parameter", main = "")
  abline(v = 0.1, col = 2, lwd = 3, lty = 2)
  hist(as.numeric(modsum2$pars[modsum2$pars$Label=="VonBert_K_Fem_GP_1", 1:20]),
       breaks=seq(0.05,0.15,0.01), xlab = "K parameter", main = "")
  abline(v = 0.1, col = 2, lwd = 3, lty = 2)
  hist(as.numeric(modsum1$pars[modsum1$pars$Label=="Size_inflection_fishery(1)", 1:20]),
       breaks=seq(5,0.15,0.01), xlab = "L50%", main = "")
  abline(v = 0.1, col = 2, lwd = 3, lty = 2)
  hist(as.numeric(modsum2$pars[modsum2$pars$Label=="Size_inflection_fishery(1)", 1:20]),
       breaks=seq(0.05,0.15,0.01), xlab = "L50%", main = "")
  abline(v = 0.1, col = 2, lwd = 3, lty = 2)

  # 2D plot of Linf vs K
  plot(
    as.numeric(modsum1$pars[modsum1$pars$Label=="L_at_Amax_Fem_GP_1", 1:20]),
    as.numeric(modsum1$pars[modsum1$pars$Label=="VonBert_K_Fem_GP_1", 1:20]),
    xlim = c(80,140),
    ylim = c(0.05, 0.15)
  )
  points(
    as.numeric(modsum2$pars[modsum2$pars$Label=="L_at_Amax_Fem_GP_1", 1:20]),
    as.numeric(modsum2$pars[modsum2$pars$Label=="VonBert_K_Fem_GP_1", 1:20]),
    col = 2)
  meanLinf1 <- mean(as.numeric(modsum1$pars[modsum1$pars$Label=="L_at_Amax_Fem_GP_1", 1:20]))
  meanLinf2 <- mean(as.numeric(modsum2$pars[modsum2$pars$Label=="L_at_Amax_Fem_GP_1", 1:20]))
  meanK1 <- mean(as.numeric(modsum1$pars[modsum1$pars$Label=="VonBert_K_Fem_GP_1", 1:20]))
  meanK2 <- mean(as.numeric(modsum2$pars[modsum2$pars$Label=="VonBert_K_Fem_GP_1", 1:20]))
  points(meanLinf1, meanK1, pch=16, cex = 2)
  points(meanLinf2, meanK2, pch=16, cex = 2, col = 2)
  abline(v = 100, h = 0.1, lty=3)

  # time series of B30+
  startyr <- modsum1$B30plus$Yr %% 1 == 0
  plot(x = true$iyear, y = true$StartYrBgtL50,
       col = rainbow(20, alpha = 0.3)[true$irun],
       ylim = c(0, 1.05*max(true$StartYrBgtL50)),
       yaxs = 'i')
  n <- 1
  matplot(modsum1$B30plus$Yr[startyr], modsum1$B30plus[startyr, 1:n],
          type = 'l',
          col = rgb(1, 0, 0, 0.2),
          add = TRUE)
  matplot(modsum2$B30plus$Yr[startyr], modsum2$B30plus[startyr, 1:n],
          type = 'l',
          col = rgb(0, 0, 1, 0.2),
          add = TRUE)

  # time series of N30+
  startyr <- modsum1$N30plus$Yr %% 1 == 0
  plot(x = true$iyear, y = true$StartYrNgtL50,
       col = rainbow(20, alpha = 0.3)[true$irun],
       ylim = c(0, 1.05*max(true$StartYrNgtL50)),
       yaxs = 'i')
  matplot(modsum1$N30plus$Yr[startyr], modsum1$N30plus[startyr, 1:20],
          type = 'l',
          col = rgb(1, 0, 0, 0.2),
          add = TRUE)
  matplot(modsum2$N30plus$Yr[startyr], modsum2$N30plus[startyr, 1:20],
          type = 'l',
          col = rgb(0, 0, 1, 0.2),
          add = TRUE)

  
  # time series of recruitment
  SSplotComparisons(modsum1, subplot=9)
  points(x = true$iyear[true$irun==1], y = true$StartYrRAge0[true$irun==1],
         pch = "+", cex=5)
  SSplotComparisons(modsum2, subplot=9)
  points(x = true$iyear[true$irun==1], y = true$StartYrRAge0[true$irun==1], pch = "+",
         cex=3, col=4)

  # relative error function
  re <- function(est, tru){
    (est - tru)/tru
  }
  plot(1990:2019,
       re(est = modsum1$recruits$replist1[modsum1$recruits$Yr %in% 1990:2019],
          tru = true$StartYrRAge0[true$irun == 1]),
       ylim = c(-.8, .8))
  abline(h = 0)
  points(1990:2019 + 0.1,
         re(est = modsum2$recruits$replist1[modsum2$recruits$Yr %in% 1990:2019],
            tru = true$StartYrRAge0[true$irun == 1]),
         col = 2)
  
} # end if(FALSE)


# things to send:
#growth and selectivity parameters (L50 & L95)
#selectivity by length bin
#time series R0, biomass above 40cm?
# pure exploitation rate?
# population numbers

