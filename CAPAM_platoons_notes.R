mydir <- 'c:/SS/McGarvey/CAPAM_platoons_SS'
accuage <- 24
require(r4ss) # required for SS_readdat and SS_writedat
#require(SSutils)

if(FALSE){
  #creating dummy mean length-at-age inputs for template
  tmp <- expand.grid(month = c(3,9), year = 1990:2019)
  tmp2 <- data.frame(tmp$year, tmp$month, fleet = -1, sex = 0,
                     part = 0, ageerr = 1, ignore = 999)
  for(a in 1:accuage){
    tmp2[paste0("a",a)] <- 1
  }
  for(a in 1:accuage){
    tmp2[paste0("N",a)] <- 100
  }
}


yrs <- 1990:2019


makedat <- function(irun, agelen.i, cwe.i, dir.i,
                    dir.template,
                    init_catch = FALSE,
                    overwrite = FALSE,
                    verbose = TRUE){
  # read dummy data file
  datfile <- SS_readdat(file.path(dir.template,
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
      catch.value <- sum(cwe.i$CAWT[cwe.i$YR == y & cwe.i$Tstep %in% months])
      datfile$catch$catch[datfile$catch$year == y &
                          datfile$catch$seas == s] <- catch.value

      effort.value <- sum(cwe.i$EFFORT_W[cwe.i$YR == y & cwe.i$Tstep %in% months])
      cpue.value <- catch.value / effort.value
      newrow.cpue <- data.frame(year = y,
                                seas = month,
                                index = 1,
                                obs = cpue.value,
                                se_log = 0.1)
      datfile$CPUE <- rbind(datfile$CPUE, newrow.cpue)
    } # end loop over seasons
  } # end loop over years

  # use average catch for first 5 years as equilibrium value
  # summing across seasons and dividing by 5 should get an annual value
  if(init_catch){
    for(s in 1:2){
      catch.5yr.avg.s <- sum(datfile$catch$catch[datfile$catch$year %in% 1990:1994 &
                                                 datfile$catch$seas == s])/5
      datfile$catch$catch[datfile$catch$year == -999 &
                          datfile$catch$seas == s] <- catch.5yr.avg.s
    }
  }
  
  #### get length and age comps

  # remove dummy length and age comps
  datfile$lencomp <- datfile$lencomp[NULL,]
  datfile$agecomp <- datfile$agecomp[NULL,]
  # make default value in mean size at age = -999 with
  # sample size 0
  datfile$MeanSize_at_Age_obs[,paste0("a", datfile$agebin_vector)] <- -999
  datfile$MeanSize_at_Age_obs[,paste0("N_a", datfile$agebin_vector)] <- 0
  # shorter name for data frame to make code more compact
  SatA <- datfile$MeanSize_at_Age_obs
  
  yrs <- 1990:2019
  lbins <- seq(16, 100, by = 2)
  abins <- 1:accuage

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
      samps <- agelen.i[agelen.i$iyear == y & agelen.i$itspy %in% months,]

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
      if(newrow.len$Nsamp > 0){
        datfile$lencomp <- rbind(datfile$lencomp, newrow.len)
      }

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
                         dir.template,
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
    
    file.copy(file.path(dir.template, 'starter.ss'),
              file.path(newdir, 'starter.ss'), overwrite = overwrite)
    file.copy(file.path(dir.template, 'forecast.ss'),
              file.path(newdir, 'forecast.ss'), overwrite = overwrite)
    file.copy(file.path(dir.template, 'platoons_control.ss'),
              file.path(newdir, 'platoons_control.ss'), overwrite = overwrite)
    file.copy(file.path(dir.template, 'ss.exe'),
              file.path(newdir, 'ss.exe'), overwrite = overwrite)

    # update data file (not needed if change is only to control)
    if(updatedat){
      agelen.i <- agelen[agelen$irun == irun, ]
      cwe.i <- cwe[cwe$RUN == irun, ]
      makedat(irun      = irun,
              agelen.i  = agelen.i,
              cwe.i     = cwe.i,
              dir.i     = newdir,
              dir.template = dir.template,
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
  pars.out <- data.frame(
    run = 1:modsum$n,
    max_gradient = modsum$maxgrad,
    converged = as.numeric(modsum$BratioSD[1, 1:modsum$n]) != 0,
    K = get_par(pars, "VonBert_K_Fem_GP_1"),
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

get30plus <- function(mod, min = 30){
  # calculate biomass and number of 30+ cm fish
  natlen <- mod$natlen
  Yr <- seq(1990, 2019.5, by = 0.5)
  bins <- mod$lbinspop[mod$lbinspop >= min]
  # aggregate over platoons
  natlen.mat <- matrix(0, nrow = 60, ncol = length(bins))
  for(iplatoon in unique(natlen$Platoon)){
    natlen.mat <- natlen.mat +
      as.matrix(natlen[natlen$Platoon == iplatoon &
                       natlen$Time %in% seq(1990, 2019.5, by = 0.5),
                       paste(bins)])
  }
  # weight for 30+cm fish
  wt30plus <- mod$biology$Wt_len[mod$biology$Low %in% bins]
  N30plus <- apply(natlen.mat, MARGIN = 1, FUN = sum)
  B30plus <- natlen.mat %*% wt30plus
  data.frame(Yr, N30plus = N30plus, B30plus = B30plus)
}

add30plus <- function(modsum, modlist){
  # add biomass and number of 30+ cm fish to SSsummarize output
    # summarize 30cm+ fish across models
  modsum$N30plus <- NULL
  modsum$B30plus <- NULL
  for (imod in 1:n) {
    info30plus <- get30plus(modlist[[imod]])
    modsum$N30plus <- cbind(modsum$N30plus, info30plus$N30plus)
    modsum$B30plus <- cbind(modsum$B30plus, info30plus$B30plus)
  }
  # convert to data.frame
  modsum$N30plus <- as.data.frame(modsum$N30plus)
  modsum$B30plus <- as.data.frame(modsum$B30plus)
  # rename columns
  names(modsum$N30plus) <- paste0("run", 1:n)
  names(modsum$B30plus) <- paste0("run", 1:n)
  # add year column at the end
  modsum$N30plus$Yr <- info30plus$Yr
  modsum$B30plus$Yr <- info30plus$Yr
  # return modified data frame
  modsum
}

if(FALSE){
  source('c:/ss/McGarvey/CAPAM_platoons_SS/CAPAM_platoons_notes.R')

  #### commands outside of a function
  cases <- c("Baseline_KnifeEdge40cm_Fp4_Mp05",
             "Baseline_LessSteepSel_L95eq45",
             "OneWayTrip_FRising10YrsFr0top25_Mp15",
             "OneWayTrip_LessSteepSel_L95eq45")

  for (icase in 1:4) {
    mydir.dat <- file.path(mydir, 'IBM_data_28Oct2020',
                           cases[icase]
                           )
    mydir.today1 <- file.path(mydir.dat, 'runs_with_platoons_4Dec')
    mydir.today2 <- file.path(mydir.dat, 'runs_no_platoons_4Dec')
    dir.template_current <- file.path(mydir, 'CAPAM_platoons_template_current')
    dir.template_initF <- file.path(mydir, 'CAPAM_platoons_template_initF')

    n <- 100
    
    # read files from Richard McGarvey
    agelen <- read.table(file.path(mydir.dat, 'AGE-LENGTH41.OUT'),
                         skip = 2, header = TRUE)
    cwe <- read.table(file.path(mydir.dat, 'CwEByMonth.OUT'),
                      skip = 1, header = TRUE)
    true <- read.table(file.path(mydir.dat, 'True_IBM_Values.TRU'),
                       skip = 7, header = TRUE)
    
    # subset to simulation 1 only
    agelen1 <- agelen[agelen$irun == 1,]
    cwe1 <- cwe[cwe$RUN == 1,]

    # source this function
    source('c:/ss/McGarvey/CAPAM_platoons_SS/CAPAM_platoons_notes.R')
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
                                    exe.dir = 'C:/ss/SSv3.30.16.02_Sept24',
                                    exe.file = "ss.exe", 
                                    exe.only = FALSE,
                                    verbose = TRUE)
    dirs2 <- dir(mydir.today2, full.names = TRUE)
    for(idir in dirs2){
      remove_platoons(idir)
    }
  } # end loop over cases

  
  # run the models

  source('c:/ss/McGarvey/CAPAM_platoons_SS/CAPAM_platoons_notes.R')
  cases <- c("Baseline_KnifeEdge40cm_Fp4_Mp05",
             "Baseline_LessSteepSel_L95eq45",
             "OneWayTrip_FRising10YrsFr0top25_Mp15",
             "OneWayTrip_LessSteepSel_L95eq45")

  for (icase in 4) {
    n <- 100
    mydir.dat <- file.path(mydir, 'IBM_data_28Oct2020',
                           cases[icase])
    mydir.today1 <- file.path(mydir.dat, 'runs_with_platoons_4Dec')
    mydir.today2 <- file.path(mydir.dat, 'runs_no_platoons_4Dec')

    #n <- 100
    r4ss::run_SS_models(dirvec = dir(mydir.today1, full.names = TRUE)[1:n],
                        systemcmd = TRUE, skipfinished = FALSE,
                        extras = "-nox",
                        intern = TRUE)
    source('c:/ss/McGarvey/CAPAM_platoons_SS/CAPAM_platoons_notes.R')
    r4ss::run_SS_models(dirvec = dir(mydir.today2, full.names = TRUE)[1:n],
                        systemcmd = TRUE, skipfinished = FALSE,
                        extras = "-nox",
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
         file = file.path(file.path(mydir, paste0('case', icase, '_stuff_4Dec2020.Rdata'))))
  }
  
  # load stuff saved above
  source('c:/ss/McGarvey/CAPAM_platoons_SS/CAPAM_platoons_notes.R')
  cases <- c("Baseline_KnifeEdge40cm_Fp4_Mp05",
             "Baseline_LessSteepSel_L95eq45",
             "OneWayTrip_FRising10YrsFr0top25_Mp15",
             "OneWayTrip_LessSteepSel_L95eq45")
  for (icase in 1:4) {
    n <- 100
    mydir.dat <- file.path(mydir, 'IBM_data_28Oct2020',
                           cases[icase])
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
  } # end loop over cases
  
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

