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
outer_folder <- file.path(mydir, "Scen_K_LINFcvp2")
outer_folder_output <- file.path(mydir, "output", basename(outer_folder))
cases <- list.dirs(outer_folder_output, full.names = FALSE, recursive = FALSE)
run_date <-  "2021_06_16"

Rdata_folder <- file.path("Rdata_output", basename(outer_folder))

# load saved output, create csvs ----
for(icase in cases) {
  mydir.dat <- file.path(outer_folder, icase, "IBMData")
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

# plots ----

# p1a <- SS_output('C:/SS/McGarvey/CAPAM_platoons_runs_Oct29/CAPAM_platoons_run001')
p1a <- modlist_plat[[1]]
p1adat <- SS_readdat(file.path(p1a$inputs$dir, p1a$Data_File))
p1b <- modlist_no_plat[[1]]

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
hist(as.numeric(modsum1$pars[modsum1$pars$Label=="L_at_Amax_Fem_GP_1", 1:2]),
     breaks=seq(80,140,4), xlab = "L-inf", main = "5 platoons")
abline(v = 100, col = 2, lwd = 3, lty = 2)
hist(as.numeric(modsum2$pars[modsum2$pars$Label=="L_at_Amax_Fem_GP_1", 1:2]),
     #breaks=seq(80,140,4), 
     xlab = "L-inf", main = "1 platoon")  
abline(v = 100, col = 2, lwd = 3, lty = 2)
hist(as.numeric(modsum1$pars[modsum1$pars$Label=="VonBert_K_Fem_GP_1", 1:2]),
     breaks=seq(0.05,0.15,0.01), xlab = "K parameter", main = "")
abline(v = 0.1, col = 2, lwd = 3, lty = 2)
hist(as.numeric(modsum2$pars[modsum2$pars$Label=="VonBert_K_Fem_GP_1", 1:2]),
     breaks=seq(0.05,0.15,0.01), xlab = "K parameter", main = "")
abline(v = 0.1, col = 2, lwd = 3, lty = 2)
hist(as.numeric(modsum1$pars[modsum1$pars$Label=="Size_inflection_fishery(1)", 1:2]),
     #breaks=seq(5,0.15,0.01), 
     xlab = "L50%", main = "")
abline(v = 0.1, col = 2, lwd = 3, lty = 2)
hist(as.numeric(modsum2$pars[modsum2$pars$Label=="Size_inflection_fishery(1)", 1:2]),
     #breaks=seq(0.05,0.15,0.01), 
     xlab = "L50%", main = "")
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



# things to send:
#growth and selectivity parameters (L50 & L95)
#selectivity by length bin
#time series R0, biomass above 40cm?
# pure exploitation rate?
# population numbers

