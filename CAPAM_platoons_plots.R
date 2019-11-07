mydir <- 'c:/SS/McGarvey/'
require(r4ss) # required for SS_readdat and SS_writedat

# read files from Richard McGarvey
if(FALSE){
  agelen <- read.table(file.path(mydir, 'Oct29_files/AGE-LENGTH41.OUT'),
                       skip = 2, header = TRUE)
  cwe <- read.table(file.path(mydir, 'Oct29_files/CwEByMonth.OUT'),
                    skip = 1, header = TRUE)
  # subset to simulation 1 only
  agelen1 <- agelen[agelen$irun == 1,]
  cwe1 <- cwe[cwe$RUN == 1]

  p1 <- SS_output('c:/SS/McGarvey/CAPAM_platoons_runs_Oct29/CAPAM_platoons_run001')
  p1b <- SS_output('c:/SS/McGarvey/CAPAM_no_platoons_runs_Oct29/CAPAM_platoons_run001')


}



# plotting mean length at age
runs <- 1:10
# subset data
agelen.sub <- agelen[agelen$irun %in% runs, ]
growth <- aggregate(agelen.sub$LEN, by = list(floor(agelen.sub$AGE)),
                    FUN = quantile, c(0.05, 0.5, 0.95))
growth <- cbind(growth[-2], growth[[2]])
names(growth) <- c("age","q05","q50","q95")

# make plot
plot(floor(agelen.sub$AGE) + runif(n = nrow(agelen.sub), min = -0.2, max = 0.2),
     agelen.sub$LEN, xlim = c(0, 20),
     pch=16, col = gray(0, 0.04), ylim = c(20,90))

lines(growth$age, growth$q50, col=2, lwd=2)
lines(growth$age, growth$q05, col=2, lwd=2, lty = 2)
lines(growth$age, growth$q95, col=2, lwd=2, lty = 2)

## #Growth parameters, mean_K, mean_Linf, CV_K, CV_Linf, rho=cor_KvsLinf
##  0.2000 75.0000  0.0010  0.1500 -0.0010



## points(agelen$AGE, agelen$LEN, xlim = c(0, 20),
##        pch=16, col = gray(0, 0.01), ylim = c(20,90))
SSplotBiology(p1, subplot = 1, add = TRUE, colvec = rep(rgb(0,1,0,.2), 3))
SSplotBiology(p1b, subplot = 1, add = TRUE, colvec = rep(rgb(1,0,0,.2), 3))

points(p1$ladbase$Bin[p1$ladbase$Month == 3],
       p1$ladbase$Exp[p1$ladbase$Month == 3], pch=16, col=4)
points(p1b$ladbase$Bin[p1b$ladbase$Month == 3],
       p1b$ladbase$Exp[p1b$ladbase$Month == 3], pch=16, col=5)
