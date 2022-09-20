#*****
#*****

# Project: Geese Behavior & Weather
# Date: 25 Aug 2022
# Author: Stephanie Cunningham
# PTF

#*****
#*****
 
library(tidyverse)
library(jagsUI)

set.seed(123)

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

# Scale covariates 
dat[,16] <- scale(dat[,16]) 

# Set up data matrices
nind <- nrow(response)
Y <- matrix(NA,nrow=nind, ncol=min(mdates$duration))
days <- min(mdates$duration)
response$yrnr <- as.numeric(factor(response$year, levels=c(2017, 2018), labels=c(1,2)))

for (i in 1:length(un.id)) {
  Y[i,] <- dat[dat$animal_id==un.id[i] & dat$RevRelDay<=days,16] 
}

# Run JAGS model
sink("R/sam_ptf.txt")
cat("
    model {
    
    ## Priors
    # Regression parameters
    alpha ~ dnorm(0, 1/2.25)        # Intercept
    beta1 ~ dnorm(0, 1/2.25)        # Slope effect of antecedent PTF
    beta2[1] <- 0                 # Set to zero effect firl level of population
    beta3[1] <- 0                 # Set to zero effect firl level of population of antecedent PTF
    
    beta2[2] ~ dnorm(0, 1/2.25)     # Prior for effects of factor population
    beta3[2] ~ dnorm(0, 1/2.25)     # Prior for effects of factor population

    # Dirichlet prior for daily ODBA weights
    for(j in 1:days){
      delta[j] ~ dgamma(1,1)
    }
    
    for (j in 1:days) {
      weight[j] <- delta[j]/sumD
        for (i in 1:nind) {
      # For each time period into the past, compute weighted ODBA variable
        antX1[j,i] <- weight[j]*ptf[i,days-j+1]
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
    
    # Compute antecedent PTF by summing weighted PTF variable over past days
    for (i in 1:nind) {
    antPTF[i] <- sum(antX1[,i])
    }
    
    ## Likelihood
    for (i in 1:nind) {
      defer[i] ~ dbern(p[i])
      logit(p[i]) <- alpha + beta1*antPTF[i] + beta2[pop[i]] + beta3[pop[i]]*antPTF[i]
    }

    }", fill=TRUE)
sink()

# Bundle data
jags.data <- list(defer=response$defer, nind=nind, ptf=Y, pop=pop, days=days) 

# Initial values
inits <- function() {list(alpha=rnorm(1), beta1=rnorm(1), beta2=c(NA, rnorm(1)),
                          beta3=c(NA, rnorm(1)), delta=rep(1,days))}

# Parameters monitored
params <- c("alpha", "beta1", "beta2", "beta3", "antPTF", "delta",
            "weight", "antX1", "weightOrdered", "cum.weight")

# MCMC settings
ni <- 5000  
nb <- 2500
nt <- 1
nc <- 3

# Run model
out <- jags(jags.data, inits, params, "R/sam_ptf.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, parallel=TRUE)

print(out, digits=3)

# Look at traceplots
traceplot(out, parameters=c("alpha", "beta1", "beta2", "beta3"))

# Visualize effects
whiskerplot(out, c("beta1", "beta2", "beta3"))

# Save output
smry <- as.data.frame(out$summary)
write_csv(smry, "results/PTF_sam.csv")

save(file="results/PTF_sam.Rdata", list="out")


