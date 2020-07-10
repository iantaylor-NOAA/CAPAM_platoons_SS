mydir <- 'c:/SS/McGarvey/CAPAM_platoons_SS'
mydir.dat <- file.path(mydir, 'IBM_data_9June2020')
require(r4ss) # required for SS_readdat and SS_writedat
#require(SSutils)

if(FALSE){
  #creating dummy mean length-at-age inputs for template
  tmp <- expand.grid(month = c(3,9), year = 1990:2019)
  tmp2 <- data.frame(tmp$year, tmp$month, fleet = -1, sex = 0,
                     part = 0, ageerr = 1, ignore = 999)
  for(a in 1:30){
    tmp2[paste0("a",a)] <- 1
  }
  for(a in 1:30){
    tmp2[paste0("N",a)] <- 100
  }
}


yrs <- 1990:2019


makedat <- function(irun, agelen.i, cwe.i, dir.i,
                    overwrite = FALSE, verbose = TRUE){
  # read dummy data file
  datfile <- SS_readdat(file.path(mydir,
                                  'CAPAM_platoons_template',
                                  'platoon_data_template.ss'),
                        version = 3.30,
                        verbose = FALSE)

  #### get catch and CPUE
  datfile$CPUE <- datfile$CPUE[NULL,]

  # loop over years
  for(y in yrs){
    # loop over seasons
    for(s in 1:2){
      # define month ranges
      if(s == 1){
        months <- 1:6
        month <- 3
      }
      if(s == 2){
        months <- 7:12
        month <- 9
      }
      catch.value <- sum(cwe.i$CAWT[cwe.i$YR == y & cwe.i$MONTH %in% s])
      datfile$catch$catch[datfile$catch$year == y &
                          datfile$catch$seas == s] <- catch.value

      effort.value <- sum(cwe.i$EFFORT[cwe.i$YR == y & cwe.i$MONTH %in% s])
      cpue.value <- catch.value / effort.value
      newrow.cpue <- data.frame(year = y,
                                seas = month,
                                index = 1,
                                obs = cpue.value,
                                se_log = 0.1)
      datfile$CPUE <- rbind(datfile$CPUE, newrow.cpue)
    } # end loop over seasons
  } # end loop over years

  #### get length and age comps

  # remove dummy length and age comps
  datfile$lencomp <- datfile$lencomp[NULL,]
  datfile$agecomp <- datfile$agecomp[NULL,]
  # make default value in mean size at age = -999 with
  # sample size 0
  datfile$MeanSize_at_Age_obs[,paste0("a", 1:30)] <- -999
  datfile$MeanSize_at_Age_obs[,paste0("N_a", 1:30)] <- 0
  # shorter name for data frame to make code more compact
  SatA <- datfile$MeanSize_at_Age_obs
  
  yrs <- 1990:2019
  lbins <- seq(16, 100, by = 2)
  abins <- 1:30

  # loop over years
  for(y in yrs){
    # loop over seasons
    for(s in 1:2){
      # define month ranges
      if(s == 1){
        months <- 1:6
        month <- 3
      }
      if(s == 2){
        months <- 7:12
        month <- 9
      }
      # subset rows of the IBM output
      samps <- agelen.i[agelen.i$iyear == y & agelen.i$itspy %in% s,]

      if(verbose){
        print(nrow(samps))
      }
      
      # get lengths
      lens <- samps$LEN
      # make length comp
      len.comp <- hist(lens, breaks = c(lbins, 200), plot = FALSE)$counts
      newrow.len <- data.frame(Yr = y,
                               Seas = month,
                               FltSvy = 1,
                               Gender = 0,
                               Part = 0,
                               Nsamp = length(lens),
                               t(len.comp))
      names(newrow.len)[-(1:6)] <- paste0("l", lbins)

      datfile$lencomp <- rbind(datfile$lencomp, newrow.len)

      # make marginal age comp (with negative fleet to exclude from likelihood)
      ages <- samps$AGE
      age.comp <- hist(ages, breaks = c(abins, 200), plot = FALSE)$counts
      newrow.age <- data.frame(Yr = y,
                               Seas = month,
                               FltSvy = -1,
                               Gender = 0,
                               Part = 0,
                               Ageerr = 1,
                               Lbin_lo = -1,
                               Lbin_hi = -1,
                               Nsamp = length(ages),
                               t(age.comp))
      names(newrow.age)[-(1:9)] <- paste0("a", abins)
      datfile$agecomp <- rbind(datfile$agecomp, newrow.age)

      # make conditional age at length comp
      for(lbin in lbins){
        if(any(samps$LEN >= lbin & samps$LEN < lbin + 2)){
          ages <- samps$AGE[samps$LEN >= lbin & samps$LEN < lbin + 2]
          age.comp <- hist(ages, breaks = c(abins, 200), plot = FALSE)$counts
          newrow.age <- data.frame(Yr = y,
                                   Seas = month,
                                   FltSvy = 1,
                                   Gender = 0,
                                   Part = 0,
                                   Ageerr = 1,
                                   Lbin_lo = lbin,
                                   Lbin_hi = lbin,
                                   Nsamp = length(ages),
                                   t(age.comp))
          names(newrow.age)[-(1:9)] <- paste0("a", abins)
          datfile$agecomp <- rbind(datfile$agecomp, newrow.age)
        } # end check for ages within this length bin
      } # end loop over length bins

      # make mean size at age obs
      for(abin in abins){
        lens <- samps$LEN[floor(samps$AGE) == abin]
        if(length(lens) > 0){
          SatA[SatA$Yr == y & SatA$Seas == month,
               paste0("a", abin)] <- mean(lens)
          SatA[SatA$Yr == y & SatA$Seas == month,
               paste0("N_a", abin)] <- length(lens)
        } # end check for lengths at this age
      } # end loop over age bins
      if(verbose){
        print(SatA[SatA$Yr == y & SatA$Seas == month,])
      }
    } # end loop over seasons
  } # end loop over years

  # restore mean size at age data frame to list object
  datfile$MeanSize_at_Age_obs <- SatA

  SS_writedat(datfile, file.path(dir.i, 'platoons_data.ss'),
              overwrite = overwrite)
}

##### run stuff

build_models <- function(runs = 1:100,
                         updatedat = FALSE,
                         overwrite = TRUE,
                         dir){
  for(irun in runs){

    # copy all non-data files
    newdir <- file.path(dir,
                        paste0('CAPAM_platoons_run',
                               substring(1000 + irun, 2)))
    
    if(!dir.exists(newdir)){
      dir.create(newdir)
    }
    
    file.copy(file.path(mydir, 'CAPAM_platoons_template/starter.ss'),
              file.path(newdir, 'starter.ss'), overwrite = overwrite)
    file.copy(file.path(mydir, 'CAPAM_platoons_template/forecast.ss'),
              file.path(newdir, 'forecast.ss'), overwrite = overwrite)
    file.copy(file.path(mydir, 'CAPAM_platoons_template/platoons_control.ss'),
              file.path(newdir, 'platoons_control.ss'), overwrite = overwrite)
    file.copy(file.path(mydir, 'CAPAM_platoons_template/ss.exe'),
              file.path(newdir, 'ss.exe'), overwrite = overwrite)

    # update data file (not needed if change is only to control)
    if(updatedat){
      agelen.i <- agelen[agelen$irun == irun, ]
      cwe.i <- cwe[cwe$RUN == irun, ]
      makedat(irun      = irun,
              agelen.i  = agelen.i,
              cwe.i     = cwe.i,
              dir.i     = newdir,
              overwrite = overwrite,
              verbose = FALSE)
    }
  }
}

remove_platoons <- function(dir){
  # change starter file to point to different control file
  start <- SS_readstarter(file.path(dir, 'starter.ss'))
  start$ctlfile <- "no_platoons_control.ss"
  SS_writestarter(start, dir = dir, overwrite = TRUE)
  # change control file to convert these lines:
  ## 5 #_N_platoons_Within_GrowthPattern
  ## 1 #_Platoon_between/within_stdev_ratio (no read if N_platoons=1)
  ## 0.031 0.237 0.464 0.237 0.031 #vector_platoon_dist_(-1_in_first_val_gives_normal_approx)
  # into this line:
  ## 1 #_N_platoons_Within_GrowthPattern
  controlLines <- readLines(file.path(dir, 'platoons_control.ss'))
  rows <- grep("N_platoons_Within_GrowthPattern", controlLines) + 0:2
  controlLines[rows[1]] <- "1 #_N_platoons_Within_GrowthPattern"
  controlLines <- controlLines[-rows[2:3]]
  writeLines(controlLines, file.path(dir, 'no_platoons_control.ss'))
}

get_par <- function(pars, string, N = NULL){
  # function to get a single row from the pars table in the list
  # returned by SSsummarize
  if(is.null(N)){
    N <- ncol(pars) - 3
  }
  return(as.numeric(pars[pars$Label == string, 1:N]))
}

format_params <- function(modsum){
  pars <- modsum$pars
  L50 <- get_par(pars, "Size_inflection_fishery(1)")
  L50to95 <- get_par(pars, "Size_95%width_fishery(1)")
  logR0 <- get_par(pars, "SR_LN(R0)")
  ## pars.out <- data.frame(K = get_par(pars, "VonBert_K_Fem_GP_1"),
  ##                        Linf = 10*get_par(pars, "L_at_Amax_Fem_GP_1"),
  ##                        L50_mm = 10*L50
  ##                        L95_mm = 10*(L50 + L50to95),
  ##                        qEt0_com_SSG_1 = NA,
  ##                        sigm = NA,
  ##                        sigexp = NA,
  ##                        F0 = NA,
  ##                        R0_thousands = exp(logR0),
  ##                        sigCl1tHL = NA,
  ##                        sigCw1Et1 = NA)
  pars.out <- data.frame(K = get_par(pars, "VonBert_K_Fem_GP_1"),
                         Linf_mm = 10*get_par(pars, "L_at_Amax_Fem_GP_1"),
                         CV_old = get_par(pars, "CV_old_Fem_GP_1"),
                         L50_mm = 10*L50,
                         L95_mm = 10*(L50 + L50to95),
                         R0_thousands = exp(logR0))
  return(pars.out)
}

## K	0.196749
## Linf	742.472
## l5095HL0	42.3705
## l95HL0	437.979
## qEt0_com_SSG_1	5.14052
## sigm	0.0857169
## sigexp	1
## F0	0.388752
## R0initial	11488
## sigCl1tHL	14.0537
## sigCw1Et1	1220.53

if(FALSE){
  #### commands outside of a function
  
  # read files from Richard McGarvey
  agelen <- read.table(file.path(mydir.dat, 'AGE-LENGTH41.OUT'),
                       skip = 2, header = TRUE)
  cwe <- read.table(file.path(mydir.dat, 'CwEByMonth.OUT'),
                    skip = 1, header = TRUE)

  # subset to simulation 1 only
  agelen1 <- agelen[agelen$irun == 1,]
  cwe1 <- cwe[cwe$RUN == 1,]

  # source this function
  source('c:/ss/McGarvey/CAPAM_platoons_SS/CAPAM_platoons_notes.R')
  mydir.today1 <- file.path(mydir.dat, 'runs_with_platoons_22June')
  mydir.today2 <- file.path(mydir.dat, 'runs_no_platoons_22June')
  dir.create(mydir.today1)
  dir.create(mydir.today2)
  build_models(run = 1:20, updatedat = TRUE, dir = mydir.today1)
  build_models(run = 21:100, updatedat = TRUE, dir = mydir.today1)
  ## p1 <- SS_output('c:/SS/McGarvey//CAPAM_platoons_runs_22June/CAPAM_platoons_run001')
  ## SS_plots(p1)

  # populate paltoons directories with executable
  SSutils::populate_multiple_folders(outerdir.old = mydir.today1,
                                     outerdir.new = mydir.today1,
                                     create.dir = FALSE, 
                                     overwrite = FALSE,
                                     use_ss_new = FALSE,
                                     exe.dir = 'C:/ss/SSv3.30.15.06beta_custom_reporting',
                                     exe.file = "ss.exe", 
                                     exe.only = TRUE,
                                     verbose = TRUE)

  runs <- 1:20
  dirs1 <- file.path(mydir.today1,
                     paste0('CAPAM_platoons_run',
                            substring(1000 + runs, 2)))
  SSutils::populate_multiple_folders(outerdir.old = mydir.today1,
                                     outerdir.new = mydir.today2,
                                     create.dir = TRUE, 
                                     overwrite = FALSE,
                                     use_ss_new = FALSE,
                                     exe.dir = 'C:/ss/SSv3.30.15.06beta_custom_reporting',
                                     exe.file = "ss.exe", 
                                     exe.only = FALSE,
                                     verbose = TRUE)
  dirs2 <- dir(mydir.today2, full.names = TRUE)
  for(idir in dirs2){
    remove_platoons(idir)
  }

  # run the models
  source('c:/ss/McGarvey/CAPAM_platoons_SS/CAPAM_platoons_notes.R')
  SSutils::run_SS_models(dirvec = dir(mydir.today1, full.names = TRUE)[1:20],
                         systemcmd = TRUE, skipfinished = FALSE,
                         extras = "-nox",
                         intern = TRUE)
  source('c:/ss/McGarvey/CAPAM_platoons_SS/CAPAM_platoons_notes.R')
  SSutils::run_SS_models(dirvec = dir(mydir.today2, full.names = TRUE)[1:20],
                         systemcmd = TRUE, skipfinished = FALSE,
                         extras = "-nox",
                         intern = TRUE)

  # get the output and summarize it (1 is with platoons, 2 is without)
  modlist1 <- SSgetoutput(dirvec = dir(mydir.today1, full.names = TRUE))
  modsum1 <- SSsummarize(modlist1)
  modlist2 <- SSgetoutput(dirvec = dir(mydir.today2, full.names = TRUE))
  modsum2 <- SSsummarize(modlist2)

  # format the output
  partable1 <- format_params(modsum1)
  partable2 <- format_params(modsum2)

  SSsummary_platoons <- modsum1
  SSsummary_NOplatoons <- modsum2
  save(SSsummary_platoons, SSsummary_NOplatoons,
       file = file.path(file.path(mydir, 'SSsummaries_2July2020.Rdata')))

  # write to files
  write.csv(partable1, file = file.path(mydir, "SS_parameters_with_platoons_2July2020.csv"), row.names = FALSE)
  write.csv(partable2, file = file.path(mydir, "SS_parameters_NO_platoons_2July2020.csv"), row.names = FALSE)

  # p1a <- SS_output('C:/SS/McGarvey/CAPAM_platoons_runs_Oct29/CAPAM_platoons_run001')
  p1a <- modlist1[[1]]
  p1adat <- SS_readdat(file.path(p1a$inputs$dir, p1a$Data_File))
  p1b <- modlist2[[1]]

  # illustrating platoons
  #colvec <- rich.colors.short(6, alpha = 0.4)[-1]
  png(file.path(mydir, "platoons_run001_length_distribution.png"),
                width = 6.5, height = 5, res = 300, units ='in')
  colvec <- rainbow(5, alpha = 1.0)
  #colvec <- rich.colors.short(4, alpha = 0.6)[c(4,2,1,2,4)]
  plot(0, type = 'n', xlim = c(1,15), ylim = c(4, 100), xaxs = 'i', yaxs = 'i',
       xlab = "Age (years)", ylab = "Length (mm)", axes = FALSE)
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
  
  median(partable1$Linf_mm)
  ## [1] 1053.025
  median(partable2$Linf_mm)
  ## [1] 1073.17

  median(partable1$CV_old)
  ## [1] 1053.025
  median(partable2$CV_old)


}


