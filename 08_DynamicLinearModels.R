#**********************************************************************************************************************************
#**********************************************************************************************************************************

# Project: Thesis Chapter 1 Objective 2
# Date: 3 June 2019
# Author: Stephanie Cunningham
# Description: Converting Hypothesis 1 to time series format (dynamic linear model)

#**********************************************************************************************************************************
#**********************************************************************************************************************************

library(tidyverse)
library(jagsUI)

rm(list=ls())

dat1 <- read.csv("output/dlm_emm_data.csv", stringsAsFactors=FALSE)
dat1 <- dat1[,-c(1,19:22,25,26)]
dat1$date <- as.Date(dat1$date)

wd <- read.csv("output/weather_covars.csv", stringsAsFactors=FALSE)
wd <- wd[,-1]
wd$date <- as.Date(wd$date)

dat <- left_join(dat1, wd, by=c("animal_id", "date"))

cor(dat[,c(20:ncol(dat))])

nobs <- dim(dat)[1]
nind <- length(unique(dat$id_ind))

# What is the maximum duration of spring migration?
ranges <- aggregate(julian ~ animal_id + id_ind, data=dat, FUN=range)
dr <- data.frame(id=ranges$animal_id, id_ind=ranges$id_ind, start=ranges$julian[,1], end=ranges$julian[,2])
dr$id <- as.character(dr$id)
dr <- mutate(dr, length=end-start)
dr[dr$length==max(dr$length),]
dur <- dr$length + 1

dat[,c(20:31)] <- scale(dat[,c(20:31)]) 
dat$log.odba <- log(dat$odba)

dat <- dat[,c(1:13,32,14:31)]

# Set up data matrices
prcp <- matrix(NA, nrow=nind, ncol=max(dur))
mtemp <- matrix(NA, nrow=nind, ncol=max(dur))
mintemp <- matrix(NA, nrow=nind, ncol=max(dur))
press <- matrix(NA, nrow=nind, ncol=max(dur))
omega <- matrix(NA, nrow=nind, ncol=max(dur))
clcov <- matrix(NA, nrow=nind, ncol=max(dur))
weqsd <- matrix(NA, nrow=nind, ncol=max(dur))
Y <- matrix(NA, nrow=nind, ncol=max(dur))
beta1 <- matrix(NA, nrow=nind, ncol=max(dur))
beta2 <- matrix(NA, nrow=nind, ncol=max(dur))
beta3 <- matrix(NA, nrow=nind, ncol=max(dur))

un.id <- unique(dat$animal_id)
for (i in 1:length(un.id)) {
  r <- dim(dat[dat$animal_id==un.id[i],])[1]
  prcp[i,1:r] <- dat[dat$animal_id==un.id[i],31]
  # mtemp[i,1:r] <- dat[dat$animal_id==un.id[i],21]
  mintemp[i,1:r] <- dat[dat$animal_id==un.id[i],22]
  # press[i,1:r] <- dat[dat$animal_id==un.id[i],30]
  # omega[i,1:r] <- dat[dat$animal_id==un.id[i],24]
  # clcov[i,1:r] <- dat[dat$animal_id==un.id[i],29]
  # weqsd[i,1:r] <- dat[dat$animal_id==un.id[i], 32 ]
  Y[i,1:r] <- dat[dat$animal_id==un.id[i],14] # ODBA
  beta1[i,1:r] <- rnorm(dur[i])
  beta2[i,1:r] <- rnorm(dur[i])
  beta3[i,1:r] <- rnorm(dur[i])
}

### Run model with intercept not time-varying
sink("R/dlm_odba.txt")
cat("
    model {
    
    for (i in 1:nind) {
      beta0[i] ~ dnorm(0, 0.01)
      mu1[i] ~ dnorm(0, 0.01)
      mu2[i] ~ dnorm(0, 0.01)
      mu3[i] ~ dnorm(0, 0.01)
      tau.p1[i] ~ dgamma(0.001, 0.001)
      sd.q1[i] <- 1/sqrt(tau.p1[i])
      tau.p2[i] ~ dgamma(0.001, 0.001)
      sd.q2[i] <- 1/sqrt(tau.p2[i])
    }
    
    for (i in 1:nind) {
      tau.o[i] ~ dgamma(0.001, 0.001)
      sd.r[i] <- 1/sqrt(tau.o[i])
      
      # Initialize
      beta1[i,1] <- mu1[i]
      beta2[i,1] <- mu2[i]
      beta3[i,1] <- mu3[i]
      
      predY[i,1] <- beta0[i] + beta1[i,1]*prcp[i,1] + beta2[i,1]*temp[i,1] 
      Y[i,1] ~ dnorm(predY[i,1], tau.o[i])
    }
    
    # Likelihood
    for (i in 1:nind) {
      for (j in 2:dur[i]) {
        
        predX1[i,j] <- 1*beta1[i,j-1]
        beta1[i,j] ~ dnorm(predX1[i,j], tau.p1[i])	# Process variation (coefficients for precipitation)
        
        predX2[i,j] <- 1*beta2[i,j-1]
        beta2[i,j] ~ dnorm(predX2[i,j], tau.p2[i])	# Process variation (coefficiencts for temperature)
        
        predY[i,j] <- beta0[i] + beta1[i,j]*prate[i,j] + beta2[i,j]*temp[i,j] 
        Y[i,j] ~ dnorm(predY[i,j], tau.o[i])	# Observation variation
      }
    }
    
    }", fill=TRUE)
sink()

# Bundle data
jags.data <- list(Y=Y, prate=prate, temp=temp, nind=nind, dur=dur)


# Forced random walks
inits <- function() {list(mu0=rnorm(nind), mu1=rnorm(nind), mu2=rnorm(nind), mu3=rnorm(nind),
                          tau.p0=rep(1,nind), tau.p1=rep(1,nind), tau.p2=rep(1,nind), 
                          tau.o=rep(1,nind),
                          beta0=rnorm(nind), beta1=beta1, beta2=beta2, beta3=beta3)}

# Parameters monitored
params <- c("sd.r", "sd.q0", "sd.q1", "sd.q2", 
            'tau.p0', 'tau.p1', 'tau.p2', 
            "beta0", "beta1", "beta2")


# MCMC settings
# na <- 5000
ni <- 120000
nb <- 80000
nt <- 1
nc <- 3
# Run model
out1 <- jags(jags.data, inits, params, "R/dlm_odba.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, parallel=TRUE)

### Run model with intercept not time-varying
print(out1, digits=3)

range(out1$Rhat)
range(out1$Rhat$beta1, na.rm=TRUE)
range(out1$Rhat$beta2, na.rm=TRUE)
range(out1$Rhat$beta3, na.rm=TRUE)

par(mfrow=c(4,5))
traceplot(out1, c('beta2'))

save(file="output/JAGS/.Rdata", list="out1")
smry <- as.data.frame(out1$summary)
# write.csv(smry, "F:/STEPHANIE/AccelerationAnalysis/output/timeseries/frw_odba-prcp-temp-only_summary.csv")

par(mfrow=c(3,1))
hist(out1$mean$sd.q1)
hist(out1$mean$sd.q2)
hist(out1$mean$sd.q3)

dat <- mutate(dat, prop.fly=fly/total)

dat$mig <- NA
dat$mig <- ifelse(dat$prop.fly>0.25, "y", "n")

# Add posterior means and CRI to 

un.id <- unique(dat$id_ind)
dat2 <- data.frame()
for(j in 1:length(un.id)) {
  upper1 <- lower1 <- upper2 <- lower2 <- upper3 <- lower3 <- numeric()
  for (i in 1:dur[j]) {
    lower1[i] <- quantile(out1$sims.list$beta1[,un.id[j],i], 0.025, na.rm=TRUE)
    upper1[i] <- quantile(out1$sims.list$beta1[,un.id[j],i], 0.975, na.rm=TRUE)
    lower2[i] <- quantile(out1$sims.list$beta2[,un.id[j],i], 0.025, na.rm=TRUE)
    upper2[i] <- quantile(out1$sims.list$beta2[,un.id[j],i], 0.975, na.rm=TRUE)
    lower3[i] <- quantile(out1$sims.list$beta3[,un.id[j],i], 0.025, na.rm=TRUE)
    upper3[i] <- quantile(out1$sims.list$beta3[,un.id[j],i], 0.975, na.rm=TRUE)
  }
  id1 <- data.frame(animal_id=dat[dat$id_ind==un.id[j],1], bird=dat[dat$id_ind==un.id[j],2],
                    julian=dat[dat$id_ind==un.id[j],5], date=dat[dat$id_ind==un.id[j],3], lat=dat[dat$id_ind==un.id[j],19],
                    lon=dat[dat$id_ind==un.id[j],20], log.odba=dat[dat$id_ind==un.id[j],14], mig=dat[dat$id_ind==un.id[j],34],
                    beta1=out1$mean$beta1[j,1:dur[j]], b1lower=lower1, b1upper=upper1,
                    beta2=out1$mean$beta2[j,1:dur[j]], b2lower=lower2, b2upper=upper2,
                    beta3=out1$mean$beta3[j,1:dur[j]], b3lower=lower3, b3upper=upper3)
  dat2 <- rbind(dat2, id1)
}

write.csv(dat2, "output/modelcoefficients/ODBA_clcov-omega-weqsd.csv")
# 
# dat2 <- gather(dat2, key="betas", value="postmean", c(9,12,15))
# dat2 <- gather(dat2, key="lower", value="pct2.5", c(9,11,13))
# dat2 <- gather(dat2, key="upper", value="pct97.5", 9:11)
# 
# 
# ggplot(dat2, aes(x=julian, y=postmean, group=animal_id, color=factor(mig))) + coord_cartesian(ylim=c(-1,4)) +
#   geom_hline(yintercept=0, color="gray40") +
#   geom_segment(aes(x=julian, xend=julian, y=pct2.5, yend=pct97.5)) + facet_wrap(betas ~ animal_id, ncol=2)


ggplot(dat2, aes(x=julian, y=beta1, group=animal_id, color=factor(mig))) + coord_cartesian(ylim=c(-1,4)) +
  geom_hline(yintercept=0, color="gray40") + 
  geom_segment(aes(x=julian, xend=julian, y=b1lower, yend=b1upper, color=factor(mig))) + 
  geom_point() + theme_bw() + facet_wrap(~animal_id, ncol=5) + ylab("Effect of Precipitation Rate") + xlab("Julian Day") +
  scale_color_manual(values=c("black", "red"), name="Migration Day?", breaks=c(0,1), 
                     labels=c("No", "Yes"))  +
  theme(legend.justification=c(1,0), legend.position=c(1,0), 
        legend.title=element_text(face="bold"), legend.background=element_rect(fill=NA))
   
ggplot(dat2, aes(x=julian, y=beta2, group=animal_id, color=factor(mig))) + coord_cartesian(ylim=c(-1,1)) +
  geom_hline(yintercept=0, color="gray40") + 
  geom_segment(aes(x=julian, xend=julian, y=b2lower, yend=b2upper, color=factor(mig))) + 
  geom_point() + theme_bw() + facet_wrap(~animal_id, ncol=5) + ylab("Effect of Temperature") + xlab("Julian Day") +
  scale_color_manual(values=c("black", "red"), name="Migration Day?", breaks=c(0,1), 
                     labels=c("No", "Yes"))  +
  theme(legend.justification=c(1,0), legend.position=c(1,0), 
        legend.title=element_text(face="bold"), legend.background=element_rect(fill=NA))

ggplot(dat2, aes(x=julian, y=beta3, group=animal_id, color=factor(mig))) + coord_cartesian(ylim=c(-1,4)) +
  geom_hline(yintercept=0, color="gray40") + 
  geom_segment(aes(x=julian, xend=julian, y=b3lower, yend=b3upper, color=factor(mig))) + 
  geom_point() + theme_bw() + facet_wrap(~animal_id, ncol=5) + ylab("Effect of Precip*Temp") + xlab("Julian Day") +
  scale_color_manual(values=c("black", "red"), name="Migration Day?", breaks=c(0,1), 
                     labels=c("No", "Yes"))  +
  theme(legend.justification=c(1,0), legend.position=c(1,0), 
        legend.title=element_text(face="bold"), legend.background=element_rect(fill=NA))

#### Proportion of time feeding as response variable ####
prcp <- matrix(NA, nrow=nind, ncol=max(dur))
mtemp <- matrix(NA, nrow=nind, ncol=max(dur))
mintemp <- matrix(NA, nrow=nind, ncol=max(dur))
press <- matrix(NA, nrow=nind, ncol=max(dur))
omega <- matrix(NA, nrow=nind, ncol=max(dur))
clcov <- matrix(NA, nrow=nind, ncol=max(dur))
weqsd <- matrix(NA, nrow=nind, ncol=max(dur))
n <- matrix(NA, nrow=nind, ncol=max(dur))
Y <- matrix(NA, nrow=nind, ncol=max(dur))
beta1 <- matrix(NA, nrow=nind, ncol=max(dur))
beta2 <- matrix(NA, nrow=nind, ncol=max(dur))
beta3 <- matrix(NA, nrow=nind, ncol=max(dur))

un.id <- unique(dat$animal_id)
for (i in 1:length(un.id)) {
  r <- dim(dat[dat$animal_id==un.id[i],])[1]
  prcp[i,1:r] <- dat[dat$animal_id==un.id[i],31]
  mtemp[i,1:r] <- dat[dat$animal_id==un.id[i],21]
  mintemp[i,1:r] <- dat[dat$animal_id==un.id[i],22]
  press[i,1:r] <- dat[dat$animal_id==un.id[i],30]
  omega[i,1:r] <- dat[dat$animal_id==un.id[i],24]
  clcov[i,1:r] <- dat[dat$animal_id==un.id[i],29]
  weqsd[i,1:r] <- dat[dat$animal_id==un.id[i],32]
  n[i,1:r] <- dat[dat$animal_id==un.id[i],15]
  Y[i,1:r] <- dat[dat$animal_id==un.id[i],17] # number feeding bursts
  beta1[i,1:r] <- rnorm(dur[i])
  beta2[i,1:r] <- rnorm(dur[i])
  beta3[i,1:r] <- rnorm(dur[i])
}

sink("R/dlm_ptf.txt")
cat("

   model {
    
    for (i in 1:nind) {
      beta0[i] ~ dnorm(0, 0.01)
      mu1[i] ~ dnorm(0, 0.01)
      mu2[i] ~ dnorm(0, 0.01)
      mu3[i] ~ dnorm(0, 0.01)
      tau.p1[i] ~ dgamma(0.001, 0.001)
      sd.q1[i] <- 1/sqrt(tau.p1[i])
      tau.p2[i] ~ dgamma(0.001, 0.001)
      sd.q2[i] <- 1/sqrt(tau.p2[i])
      tau.p3[i] ~ dgamma(0.001, 0.001)
      sd.q3[i] <- 1/sqrt(tau.p3[i])
    }
    
    for (i in 1:nind) {
      # phi1[i] ~ dnorm(0,1)
      # phi2[i] ~ dnorm(0,1)
      # phi3[i] ~ dnorm(0,1)
      
      tau.o[i] ~ dgamma(0.001, 0.001)
      # sd.r[i] <- 1/sqrt(tau.o[i])
      
      # Initialize
      beta1[i,1] <- mu1[i]
      beta2[i,1] <- mu2[i]
      beta3[i,1] <- mu3[i]
      
      predY[i,1] <- ilogit(beta0[i] + beta1[i,1]*clcov[i,1] + beta2[i,1]*omega[i,1] + beta3[i,1]*weqsd[i,1]) 
      Y[i,1] ~ dbinom(predY[i,1], n[i,1])
    }
    
    # Likelihood
    for (i in 1:nind) {
      for (j in 2:dur[i]) {
    
        predX1[i,j] <- 1*beta1[i,j-1]
        beta1[i,j] ~ dnorm(predX1[i,j], tau.p1[i])	# Process variation (coefficients for precipitation)
        
        predX2[i,j] <- 1*beta2[i,j-1]
        beta2[i,j] ~ dnorm(predX2[i,j], tau.p2[i])	# Process variation (coefficiencts for temperature)
        
        predX3[i,j] <- 1*beta3[i,j-1]
        beta3[i,j] ~ dnorm(predX3[i,j], tau.p3[i])	# Process variation (coefficients for prcp*temp)
        
        predY[i,j] <- ilogit(beta0[i] + beta1[i,j]*prate[i,j] + beta2[i,j]*temp[i,j] + beta3[i,j]*weqsd[i,j])
        Y[i,j] ~ dbinom(predY[i,j], n[i,j])	
      }
    }

    
}", fill=TRUE)
sink()

# Bundle data
jags.data <- list(Y=Y, clcov=clcov, omega=omega, weqsd=weqsd, nind=nind, dur=dur, n=n)

# Initial values

# Forced random walks
inits <- function() {list(mu0=rnorm(nind), mu1=rnorm(nind), mu2=rnorm(nind), mu3=rnorm(nind),
                          tau.p0=rep(1,nind), tau.p1=rep(1,nind), tau.p2=rep(1,nind), 
                          tau.o=rep(1,nind),
                          beta0=rnorm(nind), beta1=beta1, beta2=beta2, beta3=beta3)}

# Parameters monitored
params <- c("sd.r", "sd.q0", "sd.q1", "sd.q2", 
            'tau.p0', 'tau.p1', 'tau.p2', 
            "beta0", "beta1", "beta2", "beta3")

# MCMC settings
# na <- 5000
ni <- 120000
nb <- 80000
nt <- 1
nc <- 3

# Run model
out2 <- jags(jags.data, inits, params, "R/dlm_ptf.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, parallel=TRUE)

print(out2, digits=3)

range(out2$Rhat$beta1, na.rm=TRUE)
range(out2$Rhat$beta2, na.rm=TRUE)
range(out2$Rhat$beta3, na.rm=TRUE)

save(file="output/JAGS/PTF_clcov-omega-weqsd.Rdata", list="out2")

whiskerplot(out2, "beta2[3,]")

dat$mig <- ifelse(dat$fly/dat$total > 0.25, "y", "n")

un.id <- unique(dat$id_ind)
dat3 <- data.frame()
for(j in 1:length(un.id)) {
  upper1 <- lower1 <- upper2 <- lower2 <- upper3 <- lower3 <- numeric()
  for (i in 1:dur[j]) {
    lower1[i] <- quantile(out2$sims.list$beta1[,un.id[j],i], 0.025, na.rm=TRUE)
    upper1[i] <- quantile(out2$sims.list$beta1[,un.id[j],i], 0.975, na.rm=TRUE)
    lower2[i] <- quantile(out2$sims.list$beta2[,un.id[j],i], 0.025, na.rm=TRUE)
    upper2[i] <- quantile(out2$sims.list$beta2[,un.id[j],i], 0.975, na.rm=TRUE)
    lower3[i] <- quantile(out2$sims.list$beta3[,un.id[j],i], 0.025, na.rm=TRUE)
    upper3[i] <- quantile(out2$sims.list$beta3[,un.id[j],i], 0.975, na.rm=TRUE)
  }
  id1 <- data.frame(animal_id=dat[dat$id_ind==un.id[j],1], bird=dat[dat$id_ind==un.id[j],2],
                    julian=dat[dat$id_ind==un.id[j],5], date=dat[dat$id_ind==un.id[j],3], lat=dat[dat$id_ind==un.id[j],18],
                    lon=dat[dat$id_ind==un.id[j],19], total=dat[dat$id_ind==un.id[j],14], graze=dat[dat$id_ind==un.id[j],16],
                    mig=dat[dat$id_ind==un.id[j],32],
                    beta1=out2$mean$beta1[j,1:dur[j]], b1lower=lower1, b1upper=upper1,
                    beta2=out2$mean$beta2[j,1:dur[j]], b2lower=lower2, b2upper=upper2,
                    beta3=out2$mean$beta3[j,1:dur[j]], b3lower=lower3, b3upper=upper3)
  dat3 <- rbind(dat3, id1)
}

names(dat3)[16] <- "beta3"
write.csv(dat3, "output/modelcoefficients/PTF_clcov-omega-weqsd.csv")

ggplot(dat3, aes(x=julian, y=beta1, group=animal_id, color=factor(mig))) + coord_cartesian(ylim=c(-3,4)) +
  geom_hline(yintercept=0, color="gray40") + 
  geom_segment(aes(x=julian, xend=julian, y=b1lower, yend=b1upper, color=factor(mig))) + 
  geom_point() + theme_bw() + facet_wrap(~animal_id, ncol=5) + ylab("Effect of Precipitation Rate") + xlab("Julian Day") +
  scale_color_manual(values=c("black", "red"), name="Migration Day?", breaks=c(0,1), 
                     labels=c("No", "Yes"))  +
  theme(legend.justification=c(1,0), legend.position=c(1,0), 
        legend.title=element_text(face="bold"), legend.background=element_rect(fill=NA))

ggplot(dat3, aes(x=julian, y=beta2, group=animal_id, color=factor(mig))) + coord_cartesian(ylim=c(-2,2)) +
  geom_hline(yintercept=0, color="gray40") + 
  geom_segment(aes(x=julian, xend=julian, y=b2lower, yend=b2upper, color=factor(mig))) + 
  geom_point() + theme_bw() + facet_wrap(~animal_id, ncol=5) + ylab("Effect of Temperature") + xlab("Julian Day") +
  scale_color_manual(values=c("black", "red"), name="Migration Day?", breaks=c(0,1), 
                     labels=c("No", "Yes"))  +
  theme(legend.justification=c(1,0), legend.position=c(1,0), 
        legend.title=element_text(face="bold"), legend.background=element_rect(fill=NA))

ggplot(dat3, aes(x=julian, y=beta3, group=animal_id, color=factor(mig))) + coord_cartesian(ylim=c(-2,2)) +
  geom_hline(yintercept=0, color="gray40") + 
  geom_segment(aes(x=julian, xend=julian, y=b3lower, yend=b3upper, color=factor(mig))) + 
  geom_point() + theme_bw() + facet_wrap(~animal_id, ncol=5) + ylab("Effect of Temperature") + xlab("Julian Day") +
  scale_color_manual(values=c("black", "red"), name="Migration Day?", breaks=c(0,1), 
                     labels=c("No", "Yes"))  +
  theme(legend.justification=c(1,0), legend.position=c(1,0), 
        legend.title=element_text(face="bold"), legend.background=element_rect(fill=NA))



