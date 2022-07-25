
library(jagsUI)
library(tidyverse)


wd <- read.csv("output/weather_covars.csv", stringsAsFactors=FALSE)
wd <- wd[,-1]

dat1 <-read.csv("output/dlm_emm_data.csv", stringsAsFactors=FALSE)
dat1 <- dat1[,-1]

dat1 <- left_join(dat1, wd, by=c("animal_id", "date"))

# dat1 <- dat1[complete.cases(dat1),]

un.id <- unique(dat1$animal_id)

dat <- data.frame()
for (i in 1:length(un.id)) {
  temp <- dat1[dat1$animal_id==un.id[i],]
  if (temp$pop[1]=="GRLD") {
    temp2 <- subset(temp, rel.day<=45) # original 62 / 45
    dat <- rbind(dat, temp2)
  } else {
    temp2 <- subset(temp, rel.day<=89)  # original 93 / 89
    dat <- rbind(dat, temp2)
  }
}

# Fix attempt/defer classifications based on Tony's comments
dat$attempt[dat$animal_id=="2160" | dat$animal_id=="2176"] <- 0

dat <- subset(dat, pop=="NAMC")

# Determine duration of observation period for each bird
ranges <- aggregate(julian ~ animal_id, data=dat, FUN=range)
dr <- data.frame(id=ranges$animal_id, start=ranges$julian[,1], end=ranges$julian[,2])
dr$id <- as.character(dr$id)
dr <- mutate(dr, length=end-start)
# dr[dr$length==max(dr$length),]
dur <- dr$length + 1
un.dur <- unique(dur)
# un.dur = 62

nind <- length(unique(dat$animal_id))

# Calculate proportion of time feeding
dat <- mutate(dat, prop.graze=graze/total)
dat$prop.graze <- scale(dat$prop.graze, scale=TRUE, center=TRUE)

dat$prate <- scale(dat$prate, scale=TRUE, center=TRUE)


# Set up data matrices
Y <- matrix(NA,nrow=nind, ncol=max(dur))
prate <- matrix(NA,nrow=nind, ncol=max(dur))
intx <- matrix(NA,nrow=nind, ncol=max(dur))

un.id <- unique(dat$animal_id)
for (i in 1:length(un.id)) {
  r <- dim(dat[dat$animal_id==un.id[i],])[1]
  # Y[i,1:r] <- dat[dat$animal_id==un.id[i],13] # ODBA
  Y[i,1:r] <- dat[dat$animal_id==un.id[i],38] # PTF
  prate[i,1:r] <- dat[dat$animal_id==un.id[i],36]
  intx[i,1:r] <- dat[dat$animal_id==un.id[i],38]*dat[dat$animal_id==un.id[i],36]
}

dat2 <- dat[,c(1,4,24,11,9,12)]
dat2 <- distinct(dat2)

# dat2$year[dat2$year==2012] <- 1
# dat2$year[dat2$year==2013] <- 2
dat2$year[dat2$year==2017] <- 1
dat2$year[dat2$year==2018] <- 2

# dat2$pop <- ifelse(dat2$pop=="GRLD", 1, 2)

# ggplot(dat, aes(x=rev(rel.day), y=log.odba, group=animal_id, color=factor(attempt))) + geom_line() + theme_bw() 

ggplot(dat, aes(x=rev(rel.day), y=odba, group=animal_id, color=factor(attempt))) + 
  geom_line(size=1) + theme_bw() + scale_color_manual(values=c("black", "gray60"))

# id1 <- subset(dat, animal_id=="17777")

# par(mar=c(4.5,4.5, 1, 1))
# plot(rev(id1$rel.day), rev(id1$prop.graze), type="l", ylim=c(0,1), lwd=2, 
# xlab="Days to Breeding Period", ylab="Proportion of Time Feeding",
# cex.lab=1.8, cex.axis=1.5)


sink("R/ecomem_2.txt")
cat("
    model {
    
    ## Priors
    # Regression parameters
    beta0 ~ dnorm(0, 0.01)
    beta1 ~ dnorm(0, 0.01)
    beta2 ~ dnorm(0, 0.01)
    beta3 ~ dnorm(0, 0.01)
    beta4 ~ dnorm(0, 0.01)    

    # Dirichlet prior for daily ODBA weights
    for(j in 1:un.dur){
      delta[j] ~ dgamma(1,1)
      delta.prcp[j] ~ dgamma(1,1)
      delta.intx[j] ~ dgamma(1,1)
    }
    
    for (j in 1:un.dur) {
      weight[j] <- delta[j]/sumD
      w.prcp[j] <- delta.prcp[j]/sumDp
      w.intx[j] <- delta.intx[j]/sumDi
      
      for (i in 1:nind) {
        # For each time period into the past, compute weighted ODBA variable
        antX1[j,i] <- weight[j]*ptf[i,un.dur-j+1]
        antX2[j,i] <- w.prcp[j]*prate[i, un.dur-j+1]
        antX3[j,i] <- w.intx[j]*intx[i, un.dur-j+1]
      }
    
      # Reorder weights from recent to older
      weightOrdered[un.dur-j+1] <- weight[j]
      wO.prcp[un.dur-j+1] <- w.prcp[j]
      wO.intx[un.dur-j+1] <- w.intx[j]
    }
    
    # Compute sum of deltas (unnormalized weights)
    sumD <- sum(delta)
    sumDp <- sum(delta.prcp)
    sumDi <- sum(delta.intx)
    
    # Compute cumulative daily weights
    for (j in 1:un.dur) {
      cum.weight[j] <- sum(weightOrdered[1:j])
      cum.w.prcp[j] <- sum(wO.prcp[1:j])
      cum.w.intx[j] <- sum(wO.intx[1:j])
    }
    
    # Compute antecedent ODBA by summing weighted ODBA variable over past days
    for (i in 1:nind) {
      antPTF[i] <- sum(antX1[,i])
      antPRATE[i] <- sum(antX2[,i])
      antINTX[i] <- sum(antX3[,i])
    }
    
    ## Likelihood
    for (i in 1:nind) {
      ad[i] ~ dbern(p[i])
      logit(p[i]) <- beta0 + beta1*antPTF[i] + beta2*antPRATE[i] + beta3*antINTX[i] + beta4*year[i]
    }
    
    
    }", fill=TRUE)
sink()

# Bundle data
jags.data <- list(ad=dat2[,5], nind=nind, un.dur=un.dur, ptf=Y, prate=prate, intx=intx, year=dat2[,2])

# Initial values
inits <- function() {list(beta0=rnorm(1), beta1=rnorm(1), beta2=rnorm(1), beta3=rnorm(1), beta4=rnorm(1), 
                          delta=rep(1,un.dur), delta.prcp=rep(1,un.dur), delta.intx=rep(1,un.dur))}

# Parameters monitored
params <- c("beta0", "beta1", "beta2", "beta3", "beta4", 
            "delta", "delta.prcp", "delta.intx",
            "antPTF", "antPRATE", "antINTX",
            "weight", "w.prcp", "wO.intx", 
            "antX1", "antX2", "antX3", "weightOrdered", "wO.prcp", "wO.intx",
            "cum.weight", "cum.w.prcp", "cum.w.intx")

# MCMC settings
ni <- 5000  
nb <- 2500
nt <- 1
nc <- 3

# Run model
out2 <- jags(jags.data, inits, params, "R/ecomem_2.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, parallel=TRUE)

print(out2, digits=3)

par(mfrow=c(1,1))
whiskerplot(out2, c("beta0", "beta1", "beta2", "beta3", "beta4"))

par(mar=c(4.2,4.2,1,1), mfrow=c(3,1))
plot(out2$mean$weightOrdered, type="p", ylim=c(0,0.1), pch=20, ylab="Daily Weights", cex=2)
segments(x0=1:un.dur, x1=1:un.dur, y0=out2$q2.5$weightOrdered, y=out2$q97.5$weightOrdered)
abline(h=(1/45), col="red")

plot(out2$mean$wO.prcp, type="p", ylim=c(0,0.1), pch=20, ylab="Daily Weights", cex=2)
segments(x0=1:un.dur, x1=1:un.dur, y0=out2$q2.5$weightOrdered, y=out2$q97.5$weightOrdered)
abline(h=(1/45), col="red")

plot(out2$mean$wO.intx, type="p", ylim=c(0,0.1), pch=20, ylab="Daily Weights", cex=2)
segments(x0=1:un.dur, x1=1:un.dur, y0=out2$q2.5$weightOrdered, y=out2$q97.5$weightOrdered)
abline(h=(1/45), col="red")

# Violin plots
data_summary <- function(x) {
  m <- mean(x)
  mq <- quantile(x, probs=c(0.025, 0.975))
  ymin <- as.numeric(mq[1])
  ymax <- as.numeric(mq[2])
  return(c(y=m, ymin=ymin, ymax=ymax))
}

out2.df <- data.frame(antPTF=out2$sims.list$beta1, antPRATE=out2$sims.list$beta2, antINTX=out2$sims.list$beta3, Year=out2$sims.list$beta4)
out2.df <- gather(out2.df, key=beta, value=value, 1:4)
out2.df$beta <- factor(out2.df$beta, levels=c("antPTF", "antPRATE", "antINTX", "Year"))

ggplot(out2.df, aes(x=beta, y=value)) + geom_violin(fill="gray80", size=1, color="gray50") + geom_hline(yintercept=0, color="red") + 
  theme_classic() + xlab("Covariate") + ylab("Coefficient Posteriors") + stat_summary(fun.data=data_summary, size=1) +
  theme(axis.text=element_text(size=18), axis.title.y=element_text(size=20, face="bold"), 
        axis.title.x=element_blank(),
        panel.border=element_rect(color="black", fill=NA, size=0.5)) #+
 # geom_text(data=labels, aes(x=x, y=y, label=label), size=10, fontface="bold")

#### Now do with ODBA ####

# Set up data matrices
Y <- matrix(NA,nrow=nind, ncol=max(dur))
prate <- matrix(NA,nrow=nind, ncol=max(dur))
intx <- matrix(NA,nrow=nind, ncol=max(dur))

un.id <- unique(dat$animal_id)
for (i in 1:length(un.id)) {
  r <- dim(dat[dat$animal_id==un.id[i],])[1]
  Y[i,1:r] <- dat[dat$animal_id==un.id[i],13] # ODBA
  prate[i,1:r] <- dat[dat$animal_id==un.id[i],36]
  intx[i,1:r] <- dat[dat$animal_id==un.id[i],38]*dat[dat$animal_id==un.id[i],36]
}

dat2 <- dat[,c(1,4,24,11,9,12)]
dat2 <- distinct(dat2)

dat2$year[dat2$year==2012] <- 1
dat2$year[dat2$year==2013] <- 2
# dat2$year[dat2$year==2017] <- 1
dat2$year[dat2$year==2018] <- 3

# dat2$pop <- ifelse(dat2$pop=="GRLD", 1, 2)

# ggplot(dat, aes(x=rev(rel.day), y=log.odba, group=animal_id, color=factor(attempt))) + geom_line() + theme_bw() 

ggplot(dat, aes(x=rev(rel.day), y=odba, group=animal_id, color=factor(attempt))) + 
  geom_line(size=1) + theme_bw() + scale_color_manual(values=c("black", "gray60"))

# id1 <- subset(dat, animal_id=="17777")

# par(mar=c(4.5,4.5, 1, 1))
# plot(rev(id1$rel.day), rev(id1$prop.graze), type="l", ylim=c(0,1), lwd=2, 
# xlab="Days to Breeding Period", ylab="Proportion of Time Feeding",
# cex.lab=1.8, cex.axis=1.5)


sink("R/ecomem_2.txt")
cat("
    model {
    
    ## Priors
    # Regression parameters
    beta0 ~ dnorm(0, 0.01)
    beta1 ~ dnorm(0, 0.01)
    beta2 ~ dnorm(0, 0.01)
    beta3 ~ dnorm(0, 0.01)
    beta4 ~ dnorm(0, 0.01)    
    
    # Dirichlet prior for daily ODBA weights
    for(j in 1:un.dur){
    delta[j] ~ dgamma(1,1)
    delta.prcp[j] ~ dgamma(1,1)
    delta.intx[j] ~ dgamma(1,1)
    }
    
    for (j in 1:un.dur) {
    weight[j] <- delta[j]/sumD
    w.prcp[j] <- delta.prcp[j]/sumDp
    w.intx[j] <- delta.intx[j]/sumDi
    
    for (i in 1:nind) {
    # For each time period into the past, compute weighted ODBA variable
    antX1[j,i] <- weight[j]*odba[i,un.dur-j+1]
    antX2[j,i] <- w.prcp[j]*prate[i, un.dur-j+1]
    antX3[j,i] <- w.intx[j]*intx[i, un.dur-j+1]
    }
    
    # Reorder weights from recent to older
    weightOrdered[un.dur-j+1] <- weight[j]
    wO.prcp[un.dur-j+1] <- w.prcp[j]
    wO.intx[un.dur-j+1] <- w.intx[j]
    }
    
    # Compute sum of deltas (unnormalized weights)
    sumD <- sum(delta)
    sumDp <- sum(delta.prcp)
    sumDi <- sum(delta.intx)
    
    # Compute cumulative daily weights
    for (j in 1:un.dur) {
    cum.weight[j] <- sum(weightOrdered[1:j])
    cum.w.prcp[j] <- sum(wO.prcp[1:j])
    cum.w.intx[j] <- sum(wO.intx[1:j])
    }
    
    # Compute antecedent ODBA by summing weighted ODBA variable over past days
    for (i in 1:nind) {
    antODBA[i] <- sum(antX1[,i])
    antPRATE[i] <- sum(antX2[,i])
    antINTX[i] <- sum(antX3[,i])
    }
    
    ## Likelihood
    for (i in 1:nind) {
    ad[i] ~ dbern(p[i])
    logit(p[i]) <- beta0 + beta1*antODBA[i] + beta2*antPRATE[i] + beta3*antINTX[i] + beta4*year[i]
    }
    
    
    }", fill=TRUE)
sink()

# Bundle data
jags.data <- list(ad=dat2[,5], nind=nind, un.dur=un.dur, odba=Y, prate=prate, intx=intx, year=dat2[,2])

# Initial values
inits <- function() {list(beta0=rnorm(1), beta1=rnorm(1), beta2=rnorm(1), beta3=rnorm(1), beta4=rnorm(1), 
                          delta=rep(1,un.dur), delta.prcp=rep(1,un.dur), delta.intx=rep(1,un.dur))}

# Parameters monitored
params <- c("beta0", "beta1", "beta2", "beta3", "beta4", 
            "delta", "delta.prcp", "delta.intx",
            "antODBA", "antPRATE", "antINTX",
            "weight", "w.prcp", "wO.intx", 
            "antX1", "antX2", "antX3", "weightOrdered", "wO.prcp", "wO.intx",
            "cum.weight", "cum.w.prcp", "cum.w.intx")

# MCMC settings
ni <- 5000  
nb <- 2500
nt <- 1
nc <- 3

# Run model
out2 <- jags(jags.data, inits, params, "R/ecomem_2.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, parallel=TRUE)

print(out2, digits=3)

par(mfrow=c(1,1))
whiskerplot(out2, c("beta0", "beta1", "beta2", "beta3", "beta4"))

par(mar=c(4.2,4.2,1,1), mfrow=c(3,1))
plot(out2$mean$weightOrdered, type="p", ylim=c(0,0.1), pch=20, ylab="Daily Weights", cex=2)
segments(x0=1:un.dur, x1=1:un.dur, y0=out2$q2.5$weightOrdered, y=out2$q97.5$weightOrdered)
abline(h=(1/45), col="red")

plot(out2$mean$wO.prcp, type="p", ylim=c(0,0.1), pch=20, ylab="Daily Weights", cex=2)
segments(x0=1:un.dur, x1=1:un.dur, y0=out2$q2.5$weightOrdered, y=out2$q97.5$weightOrdered)
abline(h=(1/45), col="red")

plot(out2$mean$wO.intx, type="p", ylim=c(0,0.1), pch=20, ylab="Daily Weights", cex=2)
segments(x0=1:un.dur, x1=1:un.dur, y0=out2$q2.5$weightOrdered, y=out2$q97.5$weightOrdered)
abline(h=(1/45), col="red")

# Violin plots
data_summary <- function(x) {
  m <- mean(x)
  mq <- quantile(x, probs=c(0.025, 0.975))
  ymin <- as.numeric(mq[1])
  ymax <- as.numeric(mq[2])
  return(c(y=m, ymin=ymin, ymax=ymax))
}

out2.df <- data.frame(antODBA=out2$sims.list$beta1, antPRATE=out2$sims.list$beta2, antINTX=out2$sims.list$beta3, Year=out2$sims.list$beta4)
out2.df <- gather(out2.df, key=beta, value=value, 1:4)
out2.df$beta <- factor(out2.df$beta, levels=c("antPTF", "antPRATE", "antINTX", "Year"))

labels <- data.frame(
  label = "Greenland",
  x = 1.2,
  y=0.2
)

ggplot(out2.df, aes(x=beta, y=value)) + geom_violin(fill="gray80", size=1, color="gray50") + geom_hline(yintercept=0, color="red") + 
  theme_classic() + xlab("Covariate") + ylab("Coefficient Posteriors") + stat_summary(fun.data=data_summary, size=1) +
  theme(axis.text=element_text(size=18), axis.title.y=element_text(size=20, face="bold"), 
        axis.title.x=element_blank(),
        panel.border=element_rect(color="black", fill=NA, size=0.5)) #+
# geom_text(data=labels, aes(x=x, y=y, label=label), size=10, fontface="bold")
