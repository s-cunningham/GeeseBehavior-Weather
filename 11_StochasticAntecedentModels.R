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

# un.id <- unique(dat1$animal_id)
# dat <- data.frame()
# for (i in 1:length(un.id)) {
#   temp <- dat1[dat1$animal_id==un.id[i],]
#   if (temp$pop[1]=="GRLD") {
#     temp2 <- subset(temp, rel.day<=50) # original 62 / 50
#     dat <- rbind(dat, temp2)
#   } else {
#     temp2 <- subset(temp, rel.day<=88)  # original 93 / 88
#     dat <- rbind(dat, temp2)
#   }
# }

dat <- dat1
# Fix attempt/defer classifications based on Tony's comments
# dat$attempt[dat$animal_id=="2160" | dat$animal_id=="2176"] <- 0

# dat <- subset(dat, pop=="GRLD")
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
dat$prop.graze <- scale(dat$prop.graze)
dat$log.odba <- log(dat$odba)
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
# write.csv(dat2, "output/samdata_namc.csv")


un.yr <- length(unique(dat2$year))

# GRLD
# dat2$year[dat2$year==2012] <- 1
# dat2$year[dat2$year==2013] <- 2
# dat2$year[dat2$year==2018] <- 3
# write.csv(dat2, "output/SAM_dat2_GRLD.csv")

# NAMC
# dat2$year[dat2$year==2017] <- 1
# dat2$year[dat2$year==2018] <- 2
# write.csv(dat2, "output/SAM_dat2_NAMC.csv")

dat2$year1 <- 0
dat2$year2 <- 0

# dat2$year1[dat2$year==2012] <- 1
# dat2$year2[dat2$year==2013] <- 1
dat2$year1[dat2$year==2017] <- 1

# ggplot(dat, aes(x=rev(rel.day), y=odba, group=animal_id, color=factor(attempt))) +
  # geom_line(size=1) + theme_bw() + scale_color_manual(values=c("black", "gray60"))

sink("R/ecomem_1.txt")
cat("
    model {
    
    ## Priors
    # Regression parameters

    # for (k in 1:un.yr) {
    #   alpha[k] ~ dnorm(0, 0.01) #1.0E-06)
    #   beta1[k] ~ dnorm(0, 0.01)
    # }
    
    beta0 ~ dnorm(0, 0.01)
    beta1 ~ dnorm(0, 0.01)
    beta2 ~ dnorm(0, 0.01) 
    beta3 ~ dnorm(0, 0.01)

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
      logit(p[i]) <- beta0 + beta1*antPTF[i] + beta2*year1[i] + beta3*year2[i]
      # logit(p[i]) <- alpha[year[i]] + beta1[year[i]]*antPTF[i] 
    }

    ## Derived parameters (effects relative to baseline) - Greenland
    # a.effe2 <- alpha[2] - alpha[1]  # Intercept 2013 vs. 2012 
    # a.effe3 <- alpha[3] - alpha[1]  # Intercept 2018 vs. 2012
    # b.effe2 <- beta1[2] - beta1[1]  # Slope 2013 vs. 2012 
    # b.effe3 <- beta1[3] - beta1[1]  # Slope 2018 vs. 2012

}", fill=TRUE)
sink()

# Bundle data
jags.data <- list(ad=dat2[,5], nind=nind, un.dur=un.dur, ptf=Y, year1=dat2[,7], year2=dat2[,8])
# jags.data <- list(ad=dat2[,5], nind=nind, un.dur=un.dur, ptf=Y, year=dat2[,2], un.yr=un.yr)

# Initial values
inits <- function() {list(beta0=rnorm(1), beta1=rnorm(1), beta2=rnorm(1), beta3=rnorm(1), delta=rep(1,un.dur), sig=1)}
# inits <- function() {list(alpha=rnorm(un.yr), beta1=rnorm(un.yr), delta=rep(1,un.dur))}

# Parameters monitored
params <- c("beta0", "beta1", "beta2", "beta3", "p", "delta", "sig", "antPTF", "weight", "antX1", "weightOrdered", "cum.weight")
# params <- c("alpha", "beta1","p", "a.effe2", "b.effe2", "antPTF", "weight", "antX1", "weightOrdered", "cum.weight")

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
write.csv(smry, "output/modelcoefficients/PTF_ecomem_GRLD3.csv")

save(file="output/ecomem_GRLD3ptf.Rdata", list="out4")
# load("output/ecomem_GRLDptf.Rdata")

# whiskerplot(out4, c("beta0", "beta1", "beta2", "beta3"))
whiskerplot(out2, c("alpha", "beta1"))

cols <- dat2[,5]
cols <- ifelse(cols==0, "red", "blue")
plot(1:nind, out2$mean$antPTF, pch=20, ylab="Antecedent PTF variable", col=cols, cex=2)
legend("topright", c("Defer", "Attempt"), col=c("red", "blue"), pch=20)

plot(out2$mean$cum.weight, pch=20, ylab="Cumulative Weights", main="")
abline(h=0.9, col="gray90")
segments(x0=1,x1=un.dur,y0=0,y1=1)
segments(x0=1:un.dur, x1=1:un.dur, y0=out2$q2.5$cum.weight, y=out2$q97.5$cum.weight, col="gray40")

par(mar=c(4.2,4.2,1,1))
plot(out4$mean$weightOrdered, type="p", ylim=c(0,0.07), pch=20, ylab="Daily Weights")
segments(x0=1:un.dur, x1=1:un.dur, y0=out4$q2.5$weightOrdered, y=out4$q97.5$weightOrdered, col="gray80")
abline(h=(1/50), col="red")

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
    # for (k in 1:un.yr) {
    #   alpha[k] ~ dnorm(0, 0.01) #1.0E-06)
    #   beta1[k] ~ dnorm(0, 0.01)
    # }
    
    beta0 ~ dnorm(0, 0.01)
    beta1 ~ dnorm(0, 0.01)
    beta2 ~ dnorm(0, 0.01) 
    beta3 ~ dnorm(0, 0.01)
    
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
      logit(p[i]) <- beta0 + beta1*antODBA[i] + beta2*year1[i] + beta3*year2[i]
      # logit(p[i]) <- alpha[year[i]] + beta1[year[i]]*antODBA[i]
    }
    
    ## Derived parameters (effects relative to baseline) - Greenland
    # a.effe2 <- alpha[2] - alpha[1]  # Intercept 2013 vs. 2012 
    # a.effe3 <- alpha[3] - alpha[1]  # Intercept 2018 vs. 2012
    # b.effe2 <- beta1[2] - beta1[1]  # Slope 2013 vs. 2012 
    # b.effe3 <- beta1[3] - beta1[1]  # Slope 2018 vs. 2012
    
    }", fill=TRUE)
sink()

# Bundle data
jags.data <- list(ad=dat2[,5], nind=nind, un.dur=un.dur, odba=Y, year1=dat2[,7], year2=dat2[,8]) #
# jags.data <- list(ad=dat2[,5], nind=nind, un.dur=un.dur, odba=Y, year=dat2[,2])

# Initial values
inits <- function() {list(beta0=rnorm(1), beta1=rnorm(1), beta2=rnorm(1), beta3=rnorm(1), delta=rep(1,un.dur), sig=1)}
# inits <- function() {list(alpha=rnorm(un.yr), beta1=rnorm(un.yr), delta=rep(1,un.dur), sig=1)}

# Parameters monitored
params <- c("beta0", "beta1", "beta2", "beta3", "delta", "sig", "antODBA", "weight", "antX1", "weightOrdered", "cum.weight")
# params <- c("alpha", "beta1", "p", "a.effe2", "b.effe2", "antODBA", "weight", "antX1", "weightOrdered", "cum.weight")

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
write.csv(smry, "output/modelcoefficients/ODBA_ecomem_GRLD3.csv")

save(file="output/ecomem_GRLDodba3.Rdata", list="out3")
# load("output/ecomem_GRLDodba2.Rdata")

# whiskerplot(out3, c("beta0", "beta1", "beta2", "beta3"))
# whiskerplot(out3, c("alpha", "beta1"))

cols <- dat2[,5]
cols <- ifelse(cols==0, "red", "blue")
plot(1:nind, out3$mean$antODBA, pch=20, ylab="Antecedent ODBA variable", col=cols, cex=2)
legend("topright", c("Defer", "Attempt"), col=c("red", "blue"), pch=20)

plot(out3$mean$cum.weight, pch=20, ylab="Cumulative Weights", main="", xlab="Days Prior to Breeding")
abline(h=0.9, col="gray90")
segments(x0=1,x1=un.dur,y0=0,y1=1)
segments(x0=1:un.dur, x1=1:un.dur, y0=out3$q2.5$cum.weight, y=out3$q97.5$cum.weight, col="gray40")

par(mar=c(4.2,4.2,1,1))
plot(out1$mean$weightOrdered, type="p", ylim=c(0,0.15), pch=20, ylab="Daily Weights")
segments(x0=1:un.dur, x1=1:un.dur, y0=out1$q2.5$weightOrdered, y=out1$q97.5$weightOrdered, col="gray80")
abline(h=(1/45), col="red")
