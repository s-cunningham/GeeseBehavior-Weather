#*****
#*****

# Project: Geese Behavior & Weather
# Date: 25 Aug 2022
# Author: Stephanie Cunningham
# ODBA

#*****
#*****

library(tidyverse)
library(jagsUI)

# Read in ACC data
dat <- read_csv("files_for_models/daily_odba_behavior.csv")
dat <- as.data.frame(dat)

# read in migration dates
mdates <- read_csv("files_for_models/migration_dates.csv")

# subset to migration dates
dat$RelDay <- NA
dat$RevRelDay <- NA
dat$migration <- NA
un.id <- unique(dat$animal_id)
for (i in 1:length(un.id)) {
  
  # subset migration dates
  md <- mdates[mdates$animal_id==un.id[i],]
  
  # Create vector of days
  mdays <- md$start:md$end
  
  # Add column, indicate when is migration
  dat$RelDay[dat$animal_id==un.id[i] & (dat$julian %in% mdays)] <- 1:length(mdays)
  dat$RevRelDay[dat$animal_id==un.id[i] & (dat$julian %in% mdays)] <- length(mdays):1
  dat$migration[dat$animal_id==un.id[i] & (dat$julian %in% mdays)] <- "yes"
  
}

# Subset to just migration
dat <- dat[!is.na(dat$migration), 1:20]

# subset to only collar birds
dat <- dat[dat$tag!="EOBS",]
un.id <- unique(dat$animal_id)
mdates <- mdates[mdates$animal_id %in% un.id,]

# read in attempt/defer
response <- read_csv("files_for_models/attempt_defer_collars.csv")
chars <- "MF"
pop <- ifelse(apply(response[,1], 1, sjmisc::str_contains, c("M", "F"), logic='or'), 1, 2)

# Scale covariates and take log odba
dat[,18] <- scale(dat[,18]) 

# Set up data matrices
nind <- nrow(response)
Y <- matrix(NA,nrow=nind, ncol=min(mdates$duration))
days <- min(mdates$duration)
response$yrnr <- as.numeric(factor(response$year, levels=c(2017, 2018), labels=c(1,2)))

for (i in 1:length(un.id)) {
  Y[i,] <- dat[dat$animal_id==un.id[i] & dat$RevRelDay<=days,18] 
}

# Run JAGS model
sink("R/sam_odba.txt")
cat("
    model {
    
    ## Priors
    # Regression parameters
    for (i in 1:2) {
        beta0[i] ~ dnorm(0, tau.alpha)
    }
    tau.alpha <- 1/(sd.alpha*sd.alpha)
    sd.alpha ~ dunif(0,1)
    
    beta1 ~ dnorm(0, 0.01)
    beta2 ~ dnorm(0, 0.01)
    beta3 ~ dnorm(0, 0.01)

    # Dirichlet prior for daily ODBA weights
    for(j in 1:days){
      delta[j] ~ dgamma(1,1)
    }
    
    for (j in 1:days) {
      weight[j] <- delta[j]/sumD
        for (i in 1:nind) {
      # For each time period into the past, compute weighted ODBA variable
        antX1[j,i] <- weight[j]*odba[i,days-j+1]
        }
    # Reorder weights from recent to older
    weightOrdered[days-j+1] <- weight[j]
    }
    
    # Compute sum of deltas (unnormalized weights)
    sumD <- sum(delta)
    
    # Compute cumulative daily weights
    for (j in 1:days) {
    cum.weight[j] <- sum(weightOrdered[1:j])
    }
    
    # Compute antecedent ODBA by summing weighted ODBA variable over past days
    for (i in 1:nind) {
    antODBA[i] <- sum(antX1[,i])
    }
    
    ## Likelihood
    for (i in 1:nind) {
      defer[i] ~ dbern(p[i])
      logit(p[i]) <- beta0[year[i]] + beta1*antODBA[i] + beta2*pop[i] + beta3*antODBA[i]*pop[i]
    }

    }", fill=TRUE)
sink()

# Bundle data
jags.data <- list(defer=response$defer, nind=nind, odba=Y, year=response$yrnr, 
                  un.yr=2, pop=pop, days=days) 

# Initial values
inits <- function() {list(beta0=rnorm(2), beta1=rnorm(1), beta2=rnorm(1), beta3=rnorm(1), 
                          delta=rep(1,days), alpha=runif(2, -1, 1))}

# Parameters monitored
params <- c("beta0", "beta1", "beta2", "beta3", "delta","antODBA",
             "weight", "antX1", "weightOrdered", "cum.weight")

# MCMC settings
ni <- 5000  
nb <- 2500
nt <- 1
nc <- 3

# Run model
out <- jags(jags.data, inits, params, "R/sam_odba.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, parallel=TRUE)

print(out, digits=3)
