#**********************************************************************************************************************************
#**********************************************************************************************************************************

# Project: Thesis Chapter 1 Objective 2
# Date: 4 November 2019
# Author: Stephanie Cunningham
# Description: Stochastic antecedent models - based on Ogle et al. (2015)

#**********************************************************************************************************************************
#**********************************************************************************************************************************


library(jagsUI)
library(tidyverse)


dat1 <-read.csv("output/dlm_emm_data.csv", stringsAsFactors=FALSE)
dat1 <- dat1[,-1]

# dat1 <- dat1[complete.cases(dat1),]

un.id <- unique(dat1$animal_id)

for (i in 1:length(un.id)) {
  temp <- dat1[dat1$animal_id==un.id[i],]
  w <- temp[temp$timeperiod==1,]
  # r <- aggregate(julian ~ animal_id, data=temp, FUN=range)
  if (nrow(w) < 10) {
    print(paste0(temp$animal_id[1], " ", nrow(w)))
  }
}

dat1 <- dat1[dat1$animal_id!="RP08F" & dat1$animal_id!="RP15F" & dat1$animal_id!="2164"
             & dat1$animal_id!="2176" & dat1$animal_id!="2160" & dat1$animal_id!="2167",]

ranges <- aggregate(julian ~ animal_id, data=dat1, FUN=range)
dr <- data.frame(id=ranges$animal_id, start=ranges$julian[,1], end=ranges$julian[,2])
dr$id <- as.character(dr$id)
dr <- mutate(dr, length=end-start)
dr[dr$length==min(dr$length),]

un.id <- unique(dat1$animal_id)
dat <- data.frame()
for (i in 1:length(un.id)) {
  temp <- dat1[dat1$animal_id==un.id[i],]
  if (temp$pop[1]=="GRLD") {
    temp2 <- subset(temp, rel.day<=50) # original 62 / 50
    dat <- rbind(dat, temp2)
  } else {
    temp2 <- subset(temp, rel.day<=88)  # original 93 / 88
    dat <- rbind(dat, temp2)
  }
}

dat <- subset(dat, pop=="NAMC")

# Determine duration of observation period for each bird
ranges <- aggregate(julian ~ animal_id, data=dat, FUN=range)
dr <- data.frame(id=ranges$animal_id, start=ranges$julian[,1], end=ranges$julian[,2])
dr$id <- as.character(dr$id)
dr <- mutate(dr, length=end-start)

dur <- dr$length + 1
un.dur <- unique(dur)

nind <- length(unique(dat$animal_id))

# Calculate proportion of time feeding
dat <- mutate(dat, prop.graze=graze/total)

# STANDARDIZE VARIABLES
mcPTF <- scale(dat$prop.graze)
dat$prop.graze <- scale(dat$prop.graze)
dat$log.odba <- log(dat$odba)
mcODBA <- scale(dat$log.odba)
dat$log.odba <- scale(dat$log.odba)

# Set up data matrices
Y <- matrix(NA,nrow=nind, ncol=max(dur))

un.id <- unique(dat$animal_id)
for (i in 1:length(un.id)) {
  r <- dim(dat[dat$animal_id==un.id[i],])[1]
  Y[i,1:r] <- dat[dat$animal_id==un.id[i], 26] # PTF
}

dat2 <- dat[,c(1,4,24,11,9,12)]
dat2 <- distinct(dat2)

un.yr <- length(unique(dat2$year))

# NAMC
dat2$yrnr <- ifelse(dat2$year==2018, 1, 2)

# write.csv(dat2, "output/namc-pts.csv")

sink("R/ecomem_1.txt")
cat("
    model {
    
    ## Priors
    # Regression parameters
    for (i in 1:2) {
        beta0[i] ~ dnorm(0, 0.01)
    }
    beta1 ~ dnorm(0, 0.01)

    # Dirichlet prior for daily ODBA weights
    for(j in 1:un.dur){
      delta[j] ~ dgamma(1,1)
    }

    for (j in 1:un.dur) {
      weight[j] <- delta[j]/sumD
        for (i in 1:nind) {
        # For each time period into the past, compute weighted PTF variable
        antX1[j,i] <- weight[j]*ptf[i,un.dur-j+1]
        }
      # Reorder weights from recent to older
      weightOrdered[un.dur-j+1] <- weight[j]
    }
    
    # Compute sum of deltas (unnormalized weights)
    sumD <- sum(delta)
    
    # Compute cumulative daily weights
    for (j in 1:un.dur) {
      cum.weight[j] <- sum(weightOrdered[1:j])
    }

    # Compute antecedent PTF by summing weighted PTF variable over past days
    for (i in 1:nind) {
      antPTF[i] <- sum(antX1[,i])
    }

    ## Likelihood
    for (i in 1:nind) {
      ad[i] ~ dbern(p[i])
      logit(p[i]) <- beta0[year[i]] + beta1*antPTF[i] 
    }

}", fill=TRUE)
sink()

# Bundle data
jags.data <- list(ad=dat2[,5], nind=nind, un.dur=un.dur, ptf=Y, year=dat2$yrnr)

# Initial values
inits <- function() {list(beta0=rnorm(2), beta1=rnorm(1), delta=rep(1,un.dur), sig=1)}

# Parameters monitored
params <- c("beta0", "beta1", "p", "delta", "sig", "antPTF", "weight", "antX1", "weightOrdered", "cum.weight")


# MCMC settings
ni <- 5000  
nb <- 2500
nt <- 1
nc <- 3

# Run model
out2 <- jags(jags.data, inits, params, "R/ecomem_1.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, parallel=TRUE)

print(out2, digits=3)

range(out2$Rhat)

smry <- as.data.frame(out2$summary)
write.csv(smry, "output/modelcoefficients/PTF_ecomem_NAMC4.csv")

# save(file="output/ecomem_NAMC4ptf.Rdata", list="out2")


#### Now do with ODBA ####

# Set up data matrices
Y <- matrix(NA,nrow=nind, ncol=max(dur))

un.id <- unique(dat$animal_id)
for (i in 1:length(un.id)) {
  r <- dim(dat[dat$animal_id==un.id[i],])[1]
  Y[i,1:r] <- dat[dat$animal_id==un.id[i],27] # log(ODBA) standardized
}

sink("R/ecomem_2.txt")
cat("
    model {
    
    ## Priors
    # Regression parameters
    for (i in 1:2) {
        beta0[i] ~ dnorm(0, 0.01)
    }
    beta1 ~ dnorm(0, 0.01)

    # Dirichlet prior for daily ODBA weights
    for(j in 1:un.dur){
      delta[j] ~ dgamma(1,1)
    }
    
    for (j in 1:un.dur) {
      weight[j] <- delta[j]/sumD
        for (i in 1:nind) {
      # For each time period into the past, compute weighted ODBA variable
        antX1[j,i] <- weight[j]*odba[i,un.dur-j+1]
        }
    # Reorder weights from recent to older
    weightOrdered[un.dur-j+1] <- weight[j]
    }
    
    # Compute sum of deltas (unnormalized weights)
    sumD <- sum(delta)
    
    # Compute cumulative daily weights
    for (j in 1:un.dur) {
    cum.weight[j] <- sum(weightOrdered[1:j])
    }
    
    # Compute antecedent ODBA by summing weighted ODBA variable over past days
    for (i in 1:nind) {
    antODBA[i] <- sum(antX1[,i])
    }
    
    ## Likelihood
    for (i in 1:nind) {
      ad[i] ~ dbern(p[i])
      logit(p[i]) <- beta0[year[i]] + beta1*antODBA[i] 
    }

    }", fill=TRUE)
sink()

# Bundle data
jags.data <- list(ad=dat2[,5], nind=nind, un.dur=un.dur, odba=Y, year1=dat2[,7], year=dat2$yrnr) 

# Initial values
inits <- function() {list(beta0=rnorm(2), beta1=rnorm(1), delta=rep(1,un.dur), sig=1)}

# Parameters monitored
params <- c("beta0", "beta1", "delta", "sig", "antODBA", "weight", "antX1", "weightOrdered", "cum.weight")

# MCMC settings
ni <- 5000  
nb <- 2500
nt <- 1
nc <- 3

# Run model
out1 <- jags(jags.data, inits, params, "R/ecomem_2.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, parallel=TRUE)

print(out1, digits=3)

range(out1$Rhat)

smry <- as.data.frame(out1$summary)
write.csv(smry, "output/modelcoefficients/ODBA_ecomem_NAMC4.csv")

# save(file="output/ecomem_NAMCodba4.Rdata", list="out1")


#### GRLD ####
rm(list=ls())

dat1 <-read.csv("output/dlm_emm_data.csv", stringsAsFactors=FALSE)
dat1 <- dat1[,-1]

un.id <- unique(dat1$animal_id)

for (i in 1:length(un.id)) {
  temp <- dat1[dat1$animal_id==un.id[i],]
  w <- temp[temp$timeperiod==1,]
  # r <- aggregate(julian ~ animal_id, data=temp, FUN=range)
  if (nrow(w) < 10) {
    print(paste0(temp$animal_id[1], " ", nrow(w)))
  }
}

dat1 <- dat1[dat1$animal_id!="RP08F" & dat1$animal_id!="RP15F" & dat1$animal_id!="2164"
             & dat1$animal_id!="2176" & dat1$animal_id!="2160" & dat1$animal_id!="2167",]

ranges <- aggregate(julian ~ animal_id, data=dat1, FUN=range)
dr <- data.frame(id=ranges$animal_id, start=ranges$julian[,1], end=ranges$julian[,2])
dr$id <- as.character(dr$id)
dr <- mutate(dr, length=end-start)
dr[dr$length==min(dr$length),]

un.id <- unique(dat1$animal_id)
dat <- data.frame()
for (i in 1:length(un.id)) {
  temp <- dat1[dat1$animal_id==un.id[i],]
  if (temp$pop[1]=="GRLD") {
    temp2 <- subset(temp, rel.day<=50) # original 62 / 50
    dat <- rbind(dat, temp2)
  } else {
    temp2 <- subset(temp, rel.day<=88)  # original 93 / 88
    dat <- rbind(dat, temp2)
  }
}

dat <- subset(dat, pop=="GRLD")

# Determine duration of observation period for each bird
ranges <- aggregate(julian ~ animal_id, data=dat, FUN=range)
dr <- data.frame(id=ranges$animal_id, start=ranges$julian[,1], end=ranges$julian[,2])
dr$id <- as.character(dr$id)
dr <- mutate(dr, length=end-start)

dur <- dr$length + 1
un.dur <- unique(dur)

nind <- length(unique(dat$animal_id))

# Calculate proportion of time feeding
dat <- mutate(dat, prop.graze=graze/total)

# STANDARDIZE VARIABLES
grPTF <- scale(dat$prop.graze)
dat$prop.graze <- scale(dat$prop.graze)
dat$log.odba <- log(dat$odba)
grODBA <- scale(dat$log.odba)
dat$log.odba <- scale(dat$log.odba)

# Set up data matrices
Y <- matrix(NA,nrow=nind, ncol=max(dur))

un.id <- unique(dat$animal_id)
for (i in 1:length(un.id)) {
  r <- dim(dat[dat$animal_id==un.id[i],])[1]
  Y[i,1:r] <- dat[dat$animal_id==un.id[i], 26] # PTF
}

dat2 <- dat[,c(1,4,24,11,9,12)]
dat2 <- distinct(dat2)

un.yr <- length(unique(dat2$year))

dat2$yrnr[dat2$year==2018] <- 1
dat2$yrnr[dat2$year==2013] <- 2
dat2$yrnr[dat2$year==2012] <- 3

sink("R/ecomem_1.txt")
cat("
    model {
    
    ## Priors
    # Regression parameters
    for (i in 1:3) {
     beta0[i] ~ dnorm(0, 0.01)
    }
    beta1 ~ dnorm(0, 0.01)

    # Dirichlet prior for daily ODBA weights
    for(j in 1:un.dur){
      delta[j] ~ dgamma(1,1)
    }

    for (j in 1:un.dur) {
      weight[j] <- delta[j]/sumD
        for (i in 1:nind) {
        # For each time period into the past, compute weighted ODBA variable
        antX1[j,i] <- weight[j]*ptf[i,un.dur-j+1]
        }
      # Reorder weights from recent to older
      weightOrdered[un.dur-j+1] <- weight[j]
    }
    
    # Compute sum of deltas (unnormalized weights)
    sumD <- sum(delta)
    
    # Compute cumulative daily weights
    for (j in 1:un.dur) {
      cum.weight[j] <- sum(weightOrdered[1:j])
    }

    # Compute antecedent PTF by summing weighted ODBA variable over past days
    for (i in 1:nind) {
      antPTF[i] <- sum(antX1[,i])
    }

    ## Likelihood
    for (i in 1:nind) {
      ad[i] ~ dbern(p[i])
      logit(p[i]) <- beta0[year[i]] + beta1*antPTF[i]
    }

}", fill=TRUE)
sink()

# Bundle data
jags.data <- list(ad=dat2[,5], nind=nind, un.dur=un.dur, ptf=Y, year=dat2$yrnr)

# Initial values
inits <- function() {list(beta0=rnorm(3), beta1=rnorm(1), delta=rep(1,un.dur), sig=1)}

# Parameters monitored
params <- c("beta0", "beta1", "p", "delta", "sig", "antPTF", "weight", "antX1", "weightOrdered", "cum.weight")

# MCMC settings
ni <- 5000  
nb <- 2500
nt <- 1
nc <- 3

# Run model
out4 <- jags(jags.data, inits, params, "R/ecomem_1.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, parallel=TRUE)

print(out4, digits=3)

range(out4$Rhat)

smry <- as.data.frame(out4$summary)
write.csv(smry, "output/modelcoefficients/PTF_ecomem_GRLD4.csv")

save(file="output/ecomem_GRLD4ptf.Rdata", list="out4")


#### Now do with ODBA ####

# Set up data matrices
Y <- matrix(NA,nrow=nind, ncol=max(dur))

un.id <- unique(dat$animal_id)
for (i in 1:length(un.id)) {
  r <- dim(dat[dat$animal_id==un.id[i],])[1]
  Y[i,1:r] <- dat[dat$animal_id==un.id[i],27] # log(ODBA) standardized
}

sink("R/ecomem_2.txt")
cat("
    model {
    
    ## Priors
    # Regression parameters
    for (i in 1:3) {
     beta0[i] ~ dnorm(0, 0.01)
    }
    beta1 ~ dnorm(0, 0.01)
    
    # Dirichlet prior for daily ODBA weights
    for(j in 1:un.dur){
      delta[j] ~ dgamma(1,1)
    }
    
    for (j in 1:un.dur) {
      weight[j] <- delta[j]/sumD
        for (i in 1:nind) {
      # For each time period into the past, compute weighted ODBA variable
        antX1[j,i] <- weight[j]*odba[i,un.dur-j+1]
        }
    # Reorder weights from recent to older
    weightOrdered[un.dur-j+1] <- weight[j]
    }
    
    # Compute sum of deltas (unnormalized weights)
    sumD <- sum(delta)
    
    # Compute cumulative daily weights
    for (j in 1:un.dur) {
    cum.weight[j] <- sum(weightOrdered[1:j])
    }
    
    # Compute antecedent ODBA by summing weighted ODBA variable over past days
    for (i in 1:nind) {
    antODBA[i] <- sum(antX1[,i])
    }
    
    ## Likelihood
    for (i in 1:nind) {
      ad[i] ~ dbern(p[i])
      logit(p[i]) <- beta0[year[i]] + beta1*antODBA[i]
    }

    }", fill=TRUE)
sink()

# Bundle data
jags.data <- list(ad=dat2[,5], nind=nind, un.dur=un.dur, odba=Y, year=dat2$yrnr) 

# Initial values
inits <- function() {list(beta0=rnorm(3), beta1=rnorm(1), delta=rep(1,un.dur), sig=1)}

# Parameters monitored
params <- c("beta0", "beta1", "delta", "sig", "antODBA", "weight", "antX1", "weightOrdered", "cum.weight")

# MCMC settings
ni <- 5000  
nb <- 2500
nt <- 1
nc <- 3

# Run model
out3 <- jags(jags.data, inits, params, "R/ecomem_2.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, parallel=TRUE)

print(out3, digits=3)

range(out3$Rhat)

smry <- as.data.frame(out3$summary)
write.csv(smry, "output/modelcoefficients/ODBA_ecomem_GRLD4.csv")

save(file="output/ecomem_GRLDodba4.Rdata", list="out3")


