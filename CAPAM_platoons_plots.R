#mydir <- 'c:/SS/McGarvey/'
#require(r4ss) # required for SS_readdat and SS_writedat

# read files from Richard McGarvey
# see CAPAM_platoons_notes.R for where these get read

if(FALSE){
  ## agelen <- read.table(file.path(mydir, 'Oct29_files/AGE-LENGTH41.OUT'),
  ##                      skip = 2, header = TRUE)
  ## cwe <- read.table(file.path(mydir, 'Oct29_files/CwEByMonth.OUT'),
  ##                   skip = 1, header = TRUE)
  ## # subset to simulation 1 only
  ## agelen1 <- agelen[agelen$irun == 1,]
  ## cwe1 <- cwe[cwe$RUN == 1]

  ## p1 <- SS_output('c:/SS/McGarvey/CAPAM_platoons_runs_Oct29/CAPAM_platoons_run001')
  ## p1b <- SS_output('c:/SS/McGarvey/CAPAM_no_platoons_runs_Oct29/CAPAM_platoons_run001')

  # with platoons
  p1a <- modlist1[[1]]
  # NO platoons
  p1b <- modlist2[[1]]
}



# plotting mean length at age
runs <- 1:10
runs <- 1
# subset data
agelen.sub <- agelen[agelen$irun %in% runs, ]
growth <- aggregate(agelen.sub$LEN, by = list(floor(agelen.sub$AGE)),
                    FUN = quantile, c(0.05, 0.5, 0.95))
growth <- cbind(growth[-2], growth[[2]])
names(growth) <- c("age","q05","q50","q95")

# make plot
plot(floor(agelen.sub$AGE) + runif(n = nrow(agelen.sub), min = -0.2, max = 0.2),
     agelen.sub$LEN, xlim = c(2, 20),
     pch=16, col = gray(0, 0.04), ylim = c(30,90),
     xlab = "Age (years)",
     ylab = "Length (cm)")

lines(growth$age, growth$q50, col=1, lwd=2, lty = 2)
lines(growth$age, growth$q05, col=1, lwd=2, lty = 3)
lines(growth$age, growth$q95, col=1, lwd=2, lty = 3)

## #Growth parameters, mean_K, mean_Linf, CV_K, CV_Linf, rho=cor_KvsLinf
##  0.2000 75.0000  0.0010  0.1500 -0.0010



## points(agelen$AGE, agelen$LEN, xlim = c(0, 20),
##        pch=16, col = gray(0, 0.01), ylim = c(20,90))

col.a <- rgb(0,0,1,.2)
col.b <- rgb(1,0,0,.2)
col.c <- rgb(0,1,0,.2)
col.a2 <- rgb(0,0,1,.7)
col.b2 <- rgb(1,0,0,.7)
col.c2 <- rgb(0,1,0,.7)
SSplotBiology(p1a, subplot = 1, add = TRUE, colvec = rep(col.a, 3))
SSplotBiology(p1b, subplot = 1, add = TRUE, colvec = rep(col.b, 3))

points(p1a$ladbase$Bin[p1a$ladbase$Month == 3],
       p1a$ladbase$Exp[p1a$ladbase$Month == 3], pch=1, cex = 1.5, col=col.a2)
points(p1b$ladbase$Bin[p1b$ladbase$Month == 3],
       p1b$ladbase$Exp[p1b$ladbase$Month == 3], pch=2, cex = 1.5, col=col.b2)
## legend('topleft',
##        pch = 16, col = gray


## growth curves


SSplotBiology(m2, subplot = 1, add = FALSE, colvec = rep(col.a, 3))
SSplotBiology(m1, subplot = 1, add = TRUE, colvec = rep(col.b, 3))
SSplotBiology(m3, subplot = 1, add = TRUE, colvec = rep(col.c, 3))
legend('topleft', fill=c(col.a, col.b, col.c), legend=c('No platoons', 'Platoons with rho = 1.0', 'Platoons with rho = 0.7'))


m1 <- SS_output('C:/SS/McGarvey/CAPAM_platoons_SS/IBM_data_22Oct2020/runs_with_platoons_13Nov/CAPAM_platoons_run001')
m2 <- SS_output('C:/SS/McGarvey/CAPAM_platoons_SS/IBM_data_22Oct2020/runs_no_platoons_13Nov/CAPAM_platoons_run001')
m3 <- SS_output('C:/SS/McGarvey/CAPAM_platoons_SS/IBM_data_22Oct2020/CAPAM_platoons_run001_ratio0.7')
SSplotComparisons(SSsummarize(list(m1,m2,m3)))
