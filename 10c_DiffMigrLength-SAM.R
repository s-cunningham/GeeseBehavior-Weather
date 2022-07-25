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
library(forecast)

# Read in data
dat1 <-read.csv("output/dlm_emm_data.csv", stringsAsFactors=FALSE)
dat1 <- dat1[,-1]

dat1 <- dat1[dat1$animal_id!="LM31F",] # Censor this individual, 43 days with > 0.5 bursts missing

dat1 <- subset(dat1, pop=="GRLD")

# Determine duration of observation period for each bird
ranges <- aggregate(julian ~ animal_id, data=dat1, FUN=range)
dr <- data.frame(id=ranges$animal_id, start=ranges$julian[,1], end=ranges$julian[,2])
dr$id <- as.character(dr$id)
dr <- mutate(dr, length=end-start+1) # Calculate length of migration

un.len <- unique(dr$length)
plot(density(dr$length))

max.dur <- round(quantile(un.len, probs=.75)) # use quantile to set maximum migration duration

# dr$max <- ifelse(dr$length>=max.dur, 1, 2)
# ggplot(dr) + 
#   geom_segment(aes(y=factor(id), yend=factor(id), x=start, xend=end, color=factor(max))) +
#   theme_bw() + xlab("Julian Day") + ylab("Individual ID")

# Subset data to new "max" migration
un.id <- unique(dat1$animal_id)
dat <- data.frame()
for (i in 1:length(un.id)) {
  temp <- dat1[dat1$animal_id==un.id[i],]
  if (temp$pop[1]=="GRLD") {
    temp2 <- subset(temp, rel.day<=61) # 
    dat <- rbind(dat, temp2)
  } else {
    temp2 <- subset(temp, rel.day<=106)  # 
    dat <- rbind(dat, temp2)
  }
}

# Save values for model
ranges <- aggregate(julian ~ animal_id, data=dat, FUN=range)
dr <- data.frame(id=ranges$animal_id, start=ranges$julian[,1], end=ranges$julian[,2])
dr$id <- as.character(dr$id)
dr <- mutate(dr, length=end-start+1) # Calculate length of migration
dr <- mutate(dr, missing=max.dur-length)

mig.len <- dr$length

M <- sum(dr$missing) 

# Set value for number of individuals that have the maximum duration migration
fmind <- sum(mig.len==max.dur)

# Number of individuals in model
nind <- length(unique(dat$animal_id))

# Calculate proportion of time feeding
dat <- mutate(dat, prop.graze=graze/total)

# STANDARDIZE VARIABLES
mcPTF <- scale(dat$prop.graze)
dat$prop.graze <- scale(dat$prop.graze)
dat$log.odba <- log(dat$odba)
mcODBA <- scale(dat$log.odba)
dat$log.odba <- scale(dat$log.odba)

# calculate range 
range(dat$prop.graze)

# maximum 
n <- 240

# order data by number of days in migration
n.order <- order(dr$length, decreasing=TRUE)
dr <- dr[n.order,]
ids.sorted <- unique(dr$id)

dat <- dat %>% arrange(factor(animal_id, levels=ids.sorted))

as.data.frame(dat %>% group_by(animal_id) %>% summarise(ranges=range(total)))

dat$lth <- ifelse(dat$total<120,1,0)
as.data.frame(dat %>% group_by(animal_id) %>% summarise(sparse=sum(lth)))

ggplot(dat) +
  geom_line(aes(x=julian, y=total, color=animal_id)) +
  geom_point(aes(x=julian, y=total, color=animal_id, shape=tag)) +
  theme_bw()

# Set up data matrices
Y <- matrix(NA,nrow=nind, ncol=max.dur)

un.id <- unique(dat$animal_id)
for (i in 1:length(un.id)) {
  r <- dim(dat[dat$animal_id==un.id[i],])[1]
  Y[i,1:r] <- dat[dat$animal_id==un.id[i], 26] # PTF
  # Y[i,1:r] <- dat[dat$animal_id==un.id[i], 27] # ODBA
}

dat2 <- dat[,c(1,4,24,11,9,12)]
dat2 <- distinct(dat2)

# GRLD
dat2$yrnr <- factor(dat2$year, levels=c(2012,2013,2018), labels=c(1,2,3))

# NAMC
# dat2$yrnr <- ifelse(dat2$year==2018, 1, 2)

# Nr <- dim(Y)[1]
# Nc <- dim(Y)[2]
  
# Fill in missing values on the end
for (i in 1:nrow(Y)) {
  if (sum(is.na(Y[i,])) > 0) {
    nmiss <- sum(is.na(Y[i,]))
    cmiss <- which(is.na(Y[i,]))
    
    notm <- as.vector(Y[i,])
    notm <- notm[!is.na(notm)]
    notm <- ts(notm, start=1, end=length(notm), frequency=1)
    
    ndays <- length(notm)
    
    fit <- auto.arima(notm, stepwise=FALSE, approximation=FALSE)
    plot(forecast(fit,h=10))
    
    nvals <- predict(fit, type="response", n.ahead=nmiss)
    Y[i,cmiss] <- nvals$pred
  }
}

# Model
sink("R/ecomem_1e.txt")
cat("
    model {
    
    ## Priors
    # Regression parameters
    for (i in 1:3) {
        beta0[i] ~ dnorm(0, 0.01)
    }
    beta1 ~ dnorm(0, 0.01)

    # Dirichlet prior for daily ODBA weights
    for(j in 1:max.dur){
      delta[j] ~ dgamma(1,1)
    }
    
    for (j in 1:max.dur) {
      weight[j] <- delta[j]/sumD

        # For each time period into the past, compute weighted PTF variable
        for (i in 1:nind) {
          antX1[j,i] <- weight[j]*ptf[i,max.dur-j+1]
        }
        
      # Reorder weights from recent to older
      weightOrdered[max.dur-j+1] <- weight[j]
    }
    
    # Compute sum of deltas (unnormalized weights)
    sumD <- sum(delta)
    
    # Compute cumulative daily weights
    for (j in 1:max.dur) {
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
jags.data <- list(ad=dat2[,5], nind=nind, max.dur=max.dur, mig.len=mig.len, ptf=Y, year=as.numeric(dat2$yrnr))

# Initial values
inits <- function() {list(beta0=rnorm(3), beta1=rnorm(1), delta=rep(1,max.dur), sig=1)}

# Parameters monitored
params <- c("beta0", "beta1", "p", "delta", "sig", "antPTF", "weight", "antX1", "weightOrdered", "cum.weight")

# MCMC settings
ni <- 5000  
nb <- 2500
nt <- 1
nc <- 3

# Run model
out2 <- jags(jags.data, inits, params, "R/ecomem_1e.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, parallel=TRUE)

print(out2, digits=3)

range(out2$Rhat)

smry <- as.data.frame(out2$summary)
write.csv(smry, "output/modelcoefficients/PTF_ecomem_NAMC4.csv")

plot(out2$mean$cum.weight, pch=19)
segments(x0=1:max.dur, y0=out2$q2.5$cum.weight, x1=1:max.dur, y1=out2$q97.5$cum.weight, col="gray70")
segments(x0=1, y0=(1/max.dur), x1=max.dur, y1=1, col="green", lwd=2)

plot(out2$mean$weightOrdered, ylim=c(0, .2), type="h")
par(new=TRUE)
antX1 <- out2$mean$antX1
matplot(antX1, type="l", col="gray70", lty=1, ylim=c())

wo <- out2$mean$weightOrdered
days <- 1:max.dur
summary(lm(wo~poly(days,3)))

lin <- lm(wo~days)
p2 <- lm(wo~poly(days,2))
p3 <- lm(wo~poly(days,3))

mods <- MuMIn::model.sel(lin, p2, p3)

plot(days, wo, type="h", lwd=3, ylim=c(0,.1))
lines(days, fitted(lm(wo~days)), col="red")

# save(file="output/ecomem_NAMC4ptf.Rdata", list="out2")
