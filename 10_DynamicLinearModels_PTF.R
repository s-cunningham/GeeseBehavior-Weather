#*****
#*****

# Project: Geese Behavior & Weather
# Date: 24 Aug 2022
# Author: Stephanie Cunningham

#*****
#*****

library(tidyverse)
library(jagsUI)

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
