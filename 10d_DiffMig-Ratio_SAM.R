

library(jagsUI)
library(tidyverse)
library(forecast)
library(zoo)

# Read in data
dat <-read.csv("output/dlm_emm_data3.csv", stringsAsFactors=FALSE)
dat <- dat[,-1]
names(dat)[1] <- "animal_id"
dat$tag[dat$animal_id=="2160"] <- "EOBS"
dat <- dat[dat$animal_id!="LM31F",] # Censor this individual (LM31F), 43 days with > 0.5 bursts missing

# Update breeding success
dat$attempt <- 0
dat$attempt[dat$animal_id=="1749" | dat$animal_id=="2820" |
              dat$animal_id=="2161" | dat$animal_id=="2838" |
              dat$animal_id=="17763" | dat$animal_id=="17777" |
              dat$animal_id=="17778" | dat$animal_id=="17780" |
              dat$animal_id=="LM17M" | 
              dat$animal_id=="RP01F" | dat$animal_id=="RP08F" | 
              dat$animal_id=="RP17F" | dat$animal_id=="RP22F"] <- 1

# Do some stuff because we skipped a few steps, and new data set-up
dat <- unite(dat, "key", c(1,5), sep="_", remove=FALSE)

dat$birdno <- as.numeric(factor(dat$key, labels=seq(1,36,1)))

# dat <- dat[dat$tag!="EOBS",]

ggplot(dat, aes(x=julian, y=birdno, color=pop)) + geom_point()

dat <- dat[dat$julian>=50,]
dat$timeperiod[dat$julian<100 & dat$timeperiod==0] <- 1

dat <- dat[dat$timeperiod!=0,]

dat$date <- as.Date(dat$date)
N.order <- order(dat$date, decreasing=FALSE)
dat <- dat[N.order,]

dat$rel.day <- NA
# dat$rev.rel.day <- NA

un.id <- unique(dat$animal_id)
for (i in 1:length(un.id)) {
  temp <- dat[dat$animal_id==un.id[i],]
  p4 <- temp[temp$timeperiod==4,]
  min_jul <- min(temp$julian)
  md <- max(p4$julian)
  dlen <- length(min_jul:md)
  days <- 1:dlen
  dat$rel.day[dat$id==un.id[i]] <- c(rev(days), rep(NA, (nrow(temp) - dlen)))
  # dat[dat$id==un.id[i],22] <- rev(dat[dat$id==un.id[i],21])
}

dat$timeperiod[dat$timeperiod==2 | dat$timeperiod==3] <- 2
dat$timeperiod[dat$timeperiod==4] <- 3

ggplot(dat, aes(x=julian, y=birdno, color=factor(timeperiod))) + geom_point() + theme_bw() 

# CALCULATE and STANDARDIZE VARIABLES
un.id <- unique(dat$animal_id)
for (i in 1:length(un.id)) {
  temp <- dat[dat$animal_id==un.id[i],]
  
  if (sum(is.na(temp$cum.odba))>0) {
    
    mp <- which(is.na(temp$cum.odba))
    
    if (length(mp)>1) {
      for (j in 1:length(mp)) {
        mpj <- mp[j]
        if (mpj==length(temp$cum.odba)) {
          mts <- zoo(temp$cum.odba)
          nv <- na.approx(mts, rule=2)
        } else {
          mts <- zoo(temp$cum.odba)
          nv <- na.approx(mts)
        }
        temp[mpj,15] <- nv[mpj]
      }
      dat[dat$animal_id==un.id[i],15] <- temp[,15]
        
    } else {
      if (mp==length(temp$cum.odba)) {
        mts <- zoo(temp$cum.odba)
        nv <- na.approx(mts, rule=2)
        dat[dat$animal_id==un.id[i],15] <- nv
      } #else {
      #   mts <- zoo(temp$cum.odba)
      #   nv <- na.approx(mts)
      #   dat[dat$animal_id==un.id[i],15] <- nv
      # }
    }
  }
} # interpolate the handful of missing values

sum(is.na(dat$cum.odba))

dat <- mutate(dat, ratio=g.odba/cum.odba)

dat[dat$ratio>1,]

# dat <- mutate(dat, ratio=g.odba/(f.odba+s.odba))



dat <- mutate(dat, ptf=graze/total)
dat$mean.odba <- log(dat$mean.odba)
dat$ratio <- scale(dat$ratio)
dat$ptf <- scale(dat$ptf)
dat$mean.odba <- scale(dat$mean.odba)

den <- dat[,c(1,2,5:8,16,23,24)]
den <- pivot_longer(den, cols=7:9, names_to="metric", values_to="values")

ggplot(den, aes(x=values, color=metric, fill=metric)) + geom_density(alpha=0.4) + 
  theme_bw() + ggtitle("variables standardized, odba log-transformed")

# calculate range 
range(dat$ratio)

ggplot(dat, aes(x=rel.day, y=ratio, color=animal_id)) +geom_point()

# Determine duration of observation period for each bird
ranges <- aggregate(julian ~ animal_id, data=dat, FUN=range)
dr <- data.frame(id=ranges$animal_id, start=ranges$julian[,1], end=ranges$julian[,2])
dr$id <- as.character(dr$id)
dr <- mutate(dr, length=end-start+1) # Calculate length of migration

un.len <- unique(dr$length)
plot(density(dr$length))

max.dur <- 90
ggplot(dat, aes(x=rel.day, y=birdno, color=pop)) + geom_point() + geom_vline(aes(xintercept=max.dur)) + theme_bw()

# reduce time series to only 90 days
dat <- dat[dat$rel.day<=90,]

N.order <- order(dat$id_ind, decreasing=FALSE)
dat <- dat[N.order,]

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

# order data by number of days in migration
n.order <- order(dr$length, decreasing=TRUE)
dr <- dr[n.order,]
ids.sorted <- unique(dr$id)

# Check how many bursts per day........................................................
dat <- dat %>% arrange(factor(animal_id, levels=ids.sorted))

as.data.frame(dat %>% group_by(animal_id) %>% summarise(ranges=range(total)))

dat$lth <- ifelse(dat$total<120,1,0)
as.data.frame(dat %>% group_by(animal_id) %>% summarise(sparse=sum(lth)))

ggplot(dat) +
  geom_line(aes(x=julian, y=total, color=animal_id)) +
  geom_point(aes(x=julian, y=total, color=animal_id, shape=tag)) +
  theme_bw()

ggplot(dat, aes(x=julian, y=ratio, color=animal_id, shape=factor(attempt))) + geom_line() + geom_point()
#......................................................................................

# Set up data matrices
Y <- matrix(NA,nrow=nind, ncol=max.dur)

un.id <- unique(dat$animal_id)
for (i in 1:length(un.id)) {
  r <- dim(dat[dat$animal_id==un.id[i],])[1]
  Y[i,1:r] <- dat[dat$animal_id==un.id[i], 22] # ratio
}

dat$birdno <- as.numeric(factor(dat$key, labels=seq(1,21,1)))

dat2 <- dat[,c(2,6,20,8,11,9)]
dat2 <- distinct(dat2)
dat2$pop <- as.numeric(factor(dat2$pop, levels=c("GRLD", "NAMC"), labels=c(1,2)))

dat2$yrnr <- ifelse(dat2$year==2018, 1, 2)

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

un.yr <- length(unique(dat2$yrnr))

# Model
sink("R/ecomem_2e.txt")
cat("
    model {
    
    ## Priors
    # Regression parameters
    for (i in 1:un.yr) {
        beta0[i] ~ dnorm(0, 0.01)
    }
    beta1 ~ dnorm(0, 0.01)
    beta2 ~ dnorm(0, 0.01)
    beta3 ~ dnorm(0, 0.01)

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
      logit(p[i]) <- beta0[year[i]] + beta1*antPTF[i] + beta2*pop[i] + beta3*antPTF[i]*pop[i]
    }

}", fill=TRUE)
sink()

# Bundle data
jags.data <- list(ad=dat2[,5], nind=nind, max.dur=max.dur, mig.len=mig.len, ptf=Y, year=dat2$yrnr, un.yr=un.yr, pop=dat2$pop)

# Initial values
inits <- function() {list(beta0=rnorm(un.yr), beta1=rnorm(1), beta2=rnorm(1), beta3=rnorm(1), delta=rep(1,max.dur), sig=1)}

# Parameters monitored
params <- c("beta0", "beta1", "beta2", "beta3", "p", "delta", "sig", "antPTF", "weight", "antX1", "weightOrdered", "cum.weight")

# MCMC settings
ni <- 5000  
nb <- 2500
nt <- 1
nc <- 3

# Run model
out2 <- jags(jags.data, inits, params, "R/ecomem_2e.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, parallel=TRUE)

print(out2, digits=3)

# smry <- as.data.frame(out2$summary)
# write.csv(smry, "output/modelcoefficients/ratio_ecomem_GRLD.csv")
# 
# save(file="output/JAGS/ratio_ecomem_GRLD.Rdata", list="out2")



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
mods

plot(days, wo, type="h", lwd=3, ylim=c(0,.1))
lines(days, fitted(p3), col="red")






























