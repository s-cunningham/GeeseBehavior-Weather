#*****
#*****

# Project: Geese Behavior & Weather
# Date: 24 Aug 2022
# Author: Stephanie Cunningham

#*****
#*****

library(tidyverse)
library(jagsUI)

set.seed(123)

# Read in weather covariates
wdat <- read_csv("files_for_models/weather_covars.csv")

# Read in ACC data
dat <- read_csv("files_for_models/daily_odba_behavior.csv")

# Join weather and ACC data
dat <- left_join(dat, wdat, by=c("animal_id", "date"))
dat <- as.data.frame(dat)

# read in migration dates
mdates <- read_csv("files_for_models/migration_dates.csv")

# save maximum duration of migration period
dur <- max(mdates$duration) 

# subset to migration dates
dat$migration <- NA
un.id <- unique(dat$animal_id)
for (i in 1:length(un.id)) {
  
  # subset migration dates
  md <- mdates[mdates$animal_id==un.id[i],]
  
  # Create vector of days
  mdays <- md$start:md$end
  
  # Add column, indicate when is migration
  dat$migration[dat$animal_id==un.id[i] & (dat$julian %in% mdays)] <- "yes"
  
}

# Subset to just migration
dat <- dat[!is.na(dat$migration), 1:22]

# Scale covariates and take log odba
dat[,c(19:22)] <- scale(dat[,c(19:22)]) 
dat$lnODBAmedian <- log(dat$median.odba)

# Save number of individuals
nind <- length(unique(dat$animal_id))

# Set up data matrices
prcp <- matrix(NA, nrow=nind, ncol=dur)
mintemp <- matrix(NA, nrow=nind, ncol=dur)
Y <- matrix(NA, nrow=nind, ncol=dur)
beta1 <- matrix(NA, nrow=nind, ncol=dur)
beta2 <- matrix(NA, nrow=nind, ncol=dur)
beta3 <- matrix(NA, nrow=nind, ncol=dur)

# Add data to matrices
for (i in 1:length(un.id)) {  
  r <- dim(dat[dat$animal_id==un.id[i],])[1]
  prcp[i,1:r] <- dat[dat$animal_id==un.id[i],19]
  mintemp[i,1:r] <- dat[dat$animal_id==un.id[i],21]
  Y[i,1:r] <- dat[dat$animal_id==un.id[i],23] # ODBA
  beta1[i,1:r] <- rnorm(r)
  beta2[i,1:r] <- rnorm(r)
  beta3[i,1:r] <- rnorm(r)
}

# Run each model individually
sink("R/dlm_odba_ind.txt")
cat("
    model {
    
    # Priors
    beta0 ~ dnorm(0, 0.01)        # Intercept

    mu1 ~ dnorm(0, 0.01)          # beta1[1]
    mu2 ~ dnorm(0, 0.01)          # beta2[1]
    
    eta.p1 ~ dgamma(0.001, 0.001) # Process variance for covariate 1
    sd.q1 <- 1/sqrt(eta.p1)
    
    eta.p2 ~ dgamma(0.001, 0.001) # Process variance for covariate 1
    sd.q2 <- 1/sqrt(eta.p2)
    
    tau.o ~ dgamma(0.001, 0.001)
    sd.r <- 1/sqrt(tau.o)
    
    # Initialize (fill in first value)
    beta1[1] <- mu1
    beta2[1] <- mu2

    predY[1] <- beta0 + beta1[1]*prcp[1] + beta2[1]*temp[1]
    y[1] ~ dnorm(predY[1], tau.o)

    # Likelihood
    for (i in 2:dur) {

      # Process model
      predX1[i] <- 1*beta1[i-1]
      beta1[i] ~ dnorm(predX1[i], eta.p1)	# coefficients for precipitation rate

      predX2[i] <- 1*beta2[i-1]
      beta2[i] ~ dnorm(predX2[i], eta.p2)	# coefficiencts for temperature

      # Observation model
      predY[i] <- beta0 + beta1[i]*prcp[i] + beta2[i]*temp[i]
      y[i] ~ dnorm(predY[i], tau.o)	      # Observation variation
    }
    
    }", fill=TRUE)
sink()

# Run model for each bird
for (i in 1:length(un.id)) {
  
  y <- Y[i,]
  p <- prcp[i,]
  temp <- mintemp[i,]
  d <- mdates$duration[i] 
  
  b1 <- beta1[i,]
  b2 <- beta2[i,]
  bird <- dat$animal_id[i]
  
  # Bundle data
  jags.data <- list(y=y, prcp=p, temp=temp, dur=d)
  
  # Initial values
  inits <- function() {list(mu1=rnorm(1), mu2=rnorm(1),
                            eta.p1=1, eta.p2=1, tau.o=1,
                            beta0=rnorm(1), beta1=b1[1:d], beta2=b2[1:d])}
  
  # Parameters monitored
  params <- c("sd.r", "sd.q1", "sd.q2","beta0", "beta1", "beta2")
  
  # MCMC settings
  ni <- 120000
  nb <- 80000
  nt <- 1
  nc <- 3
  
  # Run model
  out <- jags(jags.data, inits, params, "R/dlm_odba_ind.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, parallel=TRUE)
  
  # Print R hat
  print(range(out$Rhat))
  
  filename = paste0("results/mtemp-prate_ODBA_", bird, ".Rdata" )
  save(file=filename, list="out")
  
  smry <- as.data.frame(out1$summary)
  
  csvname = paste0("results/mtemp-prate_dlmODBA_summary_", bird, ".csv" )
  write_csv(smry, csvname)
  
}




