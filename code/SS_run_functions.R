makedat <- function(irun, agelen.i, cwe.i, dir.i,
                    dir.template,
                    overwrite = FALSE,
                    use_initF = FALSE,
                    verbose = TRUE, yrs){
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
        month <- 4
      }
      if(s == 2){
        months <- 7:12
        month <- 10
      }
      catch.value <- sum(cwe.i$CAWT[cwe.i$YR == y & cwe.i$MONTH %in% months])
      datfile$catch$catch[datfile$catch$year == y &
                          datfile$catch$seas == s] <- catch.value

      effort.value <- sum(cwe.i$EFFORT_W[cwe.i$YR == y & cwe.i$MONTH %in% months])
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
  if (use_initF) {
    for (s in c(2,1)) {
      catch.5yr.avg.s <- sum(datfile$catch$catch[datfile$catch$year %in% 1990:1994 &
                                                 datfile$catch$seas == s])/5
      tmp_catch <- data.frame(year = -999, seas = s, fleet = 1, catch = catch.5yr.avg.s, catch_se = 2.00)
      datfile$catch <- rbind(tmp_catch, datfile$catch)
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
        month <- 4
      }
      if(s == 2){
        months <- 7:12
        month <- 10
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

build_models <- function(runs = 1:2, # change default for now to avoid running too many models at first
                         dir.template,
                         use_initF = FALSE,
                         updatedat = FALSE,
                         overwrite = TRUE,
                         M_val  = 0.1, # default
                         CV_vals = c(0.1, 0.1),
                         dir, agelen, cwe){
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
# change control file ----
      # add to the ctl file changes
      dat <- r4ss::SS_readdat(file.path(dir.template, "platoon_data_template.ss"))
      ctl <- r4ss::SS_readctl(file.path(newdir, "platoons_control.ss"),
       use_datlist = TRUE, datlist = dat)
    if(use_initF) {
      ctl$init_F <- data.frame(LO = c(0.01, 0.01), HI = 0.8, INIT = 0.4, PRIOR = 0.1, 
                               PR_SD = 99, PR_type = 0, PHASE = 2, PType = 18)
      rownames(ctl$init_F) <- c("InitF_seas_1_flt_1fishery",
                                "InitF_seas_2_flt_1fishery")
    }
    ctl$MG_parms["NatM_p_1_Fem_GP_1", "INIT" ] <- M_val
    ctl$MG_parms["CV_young_Fem_GP_1", INIT] <- CV_vals[1]
    ctl$MG_parms["CV_old_Fem_GP_1", INIT]   <- CV_vals[2]
    r4ss::SS_writectl(ctl, file.path(newdir, "platoons_control.ss"),
                       overwrite = TRUE)

    # update data file (not needed if change is only to control)
    if(updatedat){
      agelen.i <- agelen[agelen$irun == irun, ]
      cwe.i <- cwe[cwe$RUN == irun, ]
      makedat(irun      = irun,
              agelen.i  = agelen.i,
              cwe.i     = cwe.i,
              dir.i     = newdir,
              dir.template = dir.template,
              use_initF = use_initF,
              overwrite = overwrite,
              verbose = FALSE, yrs = yrs)
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
  ## 0.4 #_Platoon_between/within_stdev_ratio (no read if N_platoons=1)
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
  n <- modsum[["n"]]
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

re <- function(est, tru){
    (est - tru)/tru
}

make_plotting_df <- function(scen_list, scen_name, platoons = TRUE, metric) {
  if(platoons == TRUE) {
    platoon_str <- "platoons"
  } else {
    platoon_str <- "no_platoons"
  }
  df <- scen_list[[platoon_str]][[metric]]
  df$scen <- scen_name
  df$platoons <- platoons
  df
}

get_params_on_bounds <- function(dir) {
  out <- r4ss::SS_output(dir, verbose = F, printstats = F)
  params_on_bounds <- tidyr::drop_na(
    out$parameters[out$parameters$Status == "HI"|
                     out$parameters$Status == "LO", ],Status)
  params_on_bounds <- paste0(params_on_bounds$Label, collapse = ", ")
}

