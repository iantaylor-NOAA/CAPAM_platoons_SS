mydir <- 'c:/SS/McGarvey/'
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
  datfile <- SS_readdat(file.path(mydir, 'CAPAM_platoons_SS/CAPAM_platoons_template/platoon_data_template.ss'),
                        version = 3.30, verbose = FALSE)

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

build_models <- function(runs = 1:100, updatedat = FALSE, overwrite = TRUE){
  for(irun in runs){

    # copy all non-data files
    newdir <- file.path(mydir,
                        paste0('CAPAM_platoons_runs_Oct29/CAPAM_platoons_run',
                               substring(1000 + irun, 2)))
    
    if(!dir.exists(newdir)){
      dir.create(newdir)
    }
    
    file.copy(file.path(mydir, 'CAPAM_platoons_SS/CAPAM_platoons_template/starter.ss'),
              file.path(newdir, 'starter.ss'), overwrite = overwrite)
    file.copy(file.path(mydir, 'CAPAM_platoons_SS/CAPAM_platoons_template/forecast.ss'),
              file.path(newdir, 'forecast.ss'), overwrite = overwrite)
    file.copy(file.path(mydir, 'CAPAM_platoons_SS/CAPAM_platoons_template/platoons_control.ss'),
              file.path(newdir, 'platoons_control.ss'), overwrite = overwrite)
    file.copy(file.path(mydir, 'CAPAM_platoons_SS/CAPAM_platoons_template/ss.exe'),
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

mydir.today <- 'c:/SS/McGarvey/CAPAM_platoons_runs_Oct29'
mydir.today2 <- 'c:/SS/McGarvey/CAPAM_no_platoons_runs_Oct29'

if(FALSE){

  # read files from Richard McGarvey
  agelen <- read.table(file.path(mydir, 'Oct29_files/AGE-LENGTH41.OUT'),
                       skip = 2, header = TRUE)
  cwe <- read.table(file.path(mydir, 'Oct29_files/CwEByMonth.OUT'),
                    skip = 1, header = TRUE)

  # subset to simulation 1 only
  agelen1 <- agelen[agelen$irun == 1,]
  cwe1 <- cwe[cwe$RUN == 1,]

  source('c:/ss/McGarvey/CAPAM_platoons_SS/CAPAM_platoons_notes.R')
  build_models(run = 1:20, updatedat = TRUE)
  build_models(run = 21:100, updatedat = TRUE)
  p1 <- SS_output('c:/SS/McGarvey//CAPAM_platoons_runs_Oct29/CAPAM_platoons_run001')
  SS_plots(p1)
  runs <- 1:100
  dirs <- file.path(mydir.today,
                    paste0('CAPAM_platoons_run',
                           substring(1000 + runs, 2)))
  SSutils::populate_multiple_folders(outerdir.old = mydir.today,
                                     outerdir.new = mydir.today2,
                                     create.dir = TRUE, 
                                     overwrite = FALSE,
                                     use_ss_new = FALSE,
                                     exe.dir = 'C:/ss/SSv3.30.14.05_Sept5',
                                     exe.file = "ss.exe", 
                                     exe.only = FALSE,
                                     verbose = TRUE)
  dirs2 <- dir(mydir.today2, full.names = TRUE)
  for(idir in dirs2){
    remove_platoons(idir)
  }

  source('c:/ss/McGarvey/CAPAM_platoons_SS/CAPAM_platoons_notes.R')
  SSutils::run_SS_models(dirvec = dir(mydir.today[21:100], full.names = TRUE),
                         systemcmd = TRUE, skipfinished = FALSE,
                         extras = "-nox -nohess",
                         intern = TRUE)
  SSutils::run_SS_models(dirvec = dir(mydir.today2[21:100], full.names = TRUE),
                         systemcmd = TRUE, skipfinished = FALSE,
                         extras = "-nox -nohess",
                         intern = TRUE)
}
