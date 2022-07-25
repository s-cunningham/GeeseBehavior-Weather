
library(jagsUI)
library(tidyverse)
library(zoo)

# Read in data
dat <-read.csv("output/dlm_emm_data3.csv", stringsAsFactors=FALSE)
dat <- dat[,-1]
names(dat)[1] <- "animal_id"
dat$tag[dat$animal_id=="2160"] <- "EOBS"
dat <- dat[dat$animal_id!="LM31F",] # Censor this individual (LM31F), 43 days with > 0.5 bursts missing
# dat <- dat[dat$animal_id!="LM31F" & dat$animal_id!="LM17M" & dat$animal_id!="RP15F",]

# Update breeding success
dat$attempt <- 0
dat$attempt[dat$animal_id=="1749" | dat$animal_id=="2820" |
             dat$animal_id=="2161" | dat$animal_id=="2838" |
             dat$animal_id=="17763" | dat$animal_id=="17777" |
             dat$animal_id=="17778" | dat$animal_id=="17780" |
             dat$animal_id=="LM17M" |
             dat$animal_id=="RP01F" | dat$animal_id=="RP08F" |
             dat$animal_id=="RP17F" | dat$animal_id=="RP22F"] <- 1
# dat$attempt <- 0
# dat$attempt[dat$animal_id=="17763" | dat$animal_id=="17777" |
#               dat$animal_id=="17778" | dat$animal_id=="17780" |
#               dat$animal_id=="2161" | dat$animal_id=="2838" |
#               dat$animal_id=="RP01F" | dat$animal_id=="RP08F" | 
#               dat$animal_id=="RP17F" | dat$animal_id=="RP22F"] <- 1

# Do some stuff because we skipped a few steps
dat <- dat[dat$timeperiod>=1,]

dat <- unite(dat, "key", c(1,5), sep="_", remove=FALSE)

dat$birdno <- as.numeric(factor(dat$key, labels=seq(1,36,1)))

# dat <- subset(dat, pop=="GRLD")
N.order <- order(dat$julian, decreasing=FALSE)
dat <- dat[N.order,]

ggplot(dat, aes(x=julian, y=birdno, color=pop)) + geom_point()

dat$rel.day <- NA
dat$rev.rel.day <- NA

un.id <- unique(dat$id)
for (i in 1:length(un.id)) {
  temp <- dat[dat$id==un.id[i],]
  p4 <- temp[temp$timeperiod==4,]
  min_jul <- min(temp$julian)
  md <- max(p4$julian)
  dlen <- length(min_jul:md)
  days <- 1:dlen
  dat$rel.day[dat$id==un.id[i]] <- c(rev(days), rep(NA, (nrow(temp) - dlen)))
  dat$rev.rel.day[dat$id==un.id[i]] <- rev(dat$rel.day[dat$id==un.id[i]])
}

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
      } else {
        mts <- zoo(temp$cum.odba)
        nv <- na.approx(mts)
        dat[dat$animal_id==un.id[i],15] <- nv
      }
    }
  }
} # interpolate the handful of missing values  ?????


# Replace NAs with zeros when for flight graze or stationary bursts (based on total)



dat <- mutate(dat, prop.odba=g.odba/cum.odba)
# dat <- mutate(dat, prop.odba=g.odba/(f.odba+s.odba))
dat$prop.odba <- scale(dat$prop.odba)

# write.csv(dat, "output/dlm_emm_data-prop.csv")

# calculate range 
range(dat$prop.odba)

# Plot all possible metrics
# dat <- mutate(dat, ptf=graze/total)
# dat$mean.odba <- log(dat$mean.odba)
# dat$prop.odba <- scale(dat$prop.odba)
# dat$ptf <- scale(dat$ptf)
# dat$mean.odba <- scale(dat$mean.odba)
# 
# den <- dat[,c(1,2,5:8,16,24,25)]
# ggplot(den, aes(x=ptf, y=mean.odba, color=prop.odba)) + geom_point() + theme_bw()
# 
# den <- pivot_longer(den, cols=7:9, names_to="metric", values_to="values")
# 
# ggplot(den, aes(x=values, color=metric, fill=metric)) + geom_density(alpha=0.4) + 
#   theme_bw() + ggtitle("variables standardized, odba log-transformed")


# Determine duration of observation period for each bird
ranges <- aggregate(julian ~ animal_id, data=dat, FUN=range)
dr <- data.frame(id=ranges$animal_id, start=ranges$julian[,1], end=ranges$julian[,2])
dr$id <- as.character(dr$id)
dr <- mutate(dr, length=end-start+1) # Calculate length of migration

un.len <- unique(dr$length)
plot(density(dr$length))

max.dur <- c(51, 89)
ggplot(dat, aes(x=rel.day, y=birdno, color=pop)) + geom_point()


### reduce time series to minimum range
un.id <- unique(dat$animal_id)

dat1 <- dat
dat <- data.frame()
for (i in 1:length(un.id)) {
  temp <- dat1[dat1$animal_id==un.id[i],]
  if (temp$pop[1]=="GRLD") {
    temp2 <- subset(temp, rel.day<=51) 
    dat <- rbind(dat, temp2)
  } else {
    temp2 <- subset(temp, rel.day<=89)  
    dat <- rbind(dat, temp2)
  }
}

plot(temp$julian, temp$rel.day)

dat <- subset(dat, pop=="NAMC")
# dat <- subset(dat, pop=="GRLD")

N.order <- order(dat$id_ind, decreasing=FALSE)
dat <- dat[N.order,]

# Save values for model
ranges <- aggregate(julian ~ animal_id, data=dat, FUN=range)
dr <- data.frame(id=ranges$animal_id, start=ranges$julian[,1], end=ranges$julian[,2])
dr$id <- as.character(dr$id)
dr <- mutate(dr, length=end-start+1) # Calculate length of migration
dr <- mutate(dr, missing=max.dur-length)

mig.len <- dr$length
un.mig <- unique(mig.len)

# Number of individuals in model
nind <- length(unique(dat$animal_id))

# order data by number of days in migration
n.order <- order(dr$length, decreasing=TRUE)
dr <- dr[n.order,]
ids.sorted <- unique(dr$id)

#### Density plots of variables ####
dat <- mutate(dat, ptf=graze/total)
dat$mean.odba <- log(dat$mean.odba)
dat$prop.odba <- scale(dat$prop.odba)
dat$ptf <- scale(dat$ptf)
dat$mean.odba <- scale(dat$mean.odba)

den <- dat[,c(1,2,5:8,16,24,25)]
den <- pivot_longer(den, cols=7:9, names_to="metric", values_to="values")

ggplot(den, aes(x=values, color=metric, fill=metric)) + geom_density(alpha=0.4) + 
  theme_bw() + ggtitle("variables standardized, odba log-transformed")
#####

# Check how many bursts per day ####
# Quality control
dat <- dat %>% arrange(factor(animal_id, levels=ids.sorted))

as.data.frame(dat %>% group_by(animal_id) %>% summarise(ranges=range(total)))

dat$lth <- ifelse(dat$total<120,1,0)
as.data.frame(dat %>% group_by(animal_id) %>% summarise(sparse=sum(lth)))

ggplot(dat) +
  geom_line(aes(x=julian, y=total, color=animal_id)) +
  geom_point(aes(x=julian, y=total, color=animal_id, shape=tag)) +
  theme_bw()

# ggplot(dat, aes(x=julian, y=prop.odba, color=animal_id, shape=factor(attempt))) + geom_line() + geom_point()
#####

# Set up data matrices
Y <- matrix(NA,nrow=nind, ncol=mig.len)

un.id <- unique(dat$animal_id)
for (i in 1:length(un.id)) {
  r <- dim(dat[dat$animal_id==un.id[i],])[1]
  Y[i,1:r] <- dat$prop.odba[dat$animal_id==un.id[i]] # prop.odba
}

dat2 <- dat[,c(2,21,6,8,11,9)]
dat2 <- distinct(dat2)

# GRLD
# dat2$yrnr <- as.numeric(factor(dat2$year, levels=c(2012,2013,2018), labels=c(1,2,3)))

# NAMC
dat2$yrnr <- ifelse(dat2$year==2018, 1, 2)

yrs <- length(unique(dat2$yrnr))


### Run model
sink("R/ecomem_1.txt")
cat("
    model {
    
    ## Priors
    # Regression parameters
    for (i in 1:yrs) {
        beta0[i] ~ dnorm(0, 0.01)
    }
    beta1 ~ dnorm(0, 0.01)

    # Dirichlet prior for daily ODBA weights
    for(j in 1:mig.len){
      delta[j] ~ dgamma(1,1)
    }

    for (j in 1:mig.len) {
      weight[j] <- delta[j]/sumD
        for (i in 1:nind) {
        # For each time period into the past, compute weighted PTF variable
        antX1[j,i] <- weight[j]*ptf[i,mig.len-j+1]
        }
      # Reorder weights from recent to older
      weightOrdered[mig.len-j+1] <- weight[j]
    }
    
    # Compute sum of deltas (unnormalized weights)
    sumD <- sum(delta)
    
    # Compute cumulative daily weights
    for (j in 1:mig.len) {
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
jags.data <- list(ad=dat2[,4], nind=nind, mig.len=un.mig, ptf=Y, year=dat2$yrnr, yrs=yrs)

# Initial values
inits <- function() {list(beta0=rnorm(yrs), beta1=rnorm(1), delta=rep(1,un.mig), sig=1)}

# Parameters monitored
params <- c("beta0", "beta1", "p", "delta", "sig", "antPTF", "weight", "antX1", "weightOrdered", "cum.weight")

# MCMC settings
ni <- 5000  
nb <- 2500
nt <- 1
nc <- 3

# Run model
out1 <- jags(jags.data, inits, params, "R/ecomem_1.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, parallel=TRUE)

print(out1, digits=3)

range(out1$Rhat)

smry <- as.data.frame(out1$summary)
write.csv(smry, "output/modelcoefficients/1model_ecomem_NAMC.csv")

save(out1, file="output/modelcoefficients/1model_ecomem_NAMC.RData")

plot(out2$mean$cum.weight, type="n", ylab="Cumulative Weights")
segments(x0=1:un.mig, y0=out2$q2.5$cum.weight, x1=1:un.mig, y1=out2$q97.5$cum.weight, col="gray70")
points(out2$mean$cum.weight, pch=19)

# segments(x0=1, y0=(1/un.mig), x1=un.mig, y1=1, col="green", lwd=2)

plot(out2$mean$weightOrdered, ylim=c(0, .2), type="h")
par(new=TRUE)
antX1 <- out1$mean$antX1
matplot(antX1, type="l", col="gray70", lty=1)

dat2[,5]


plot(out2$mean$antPTF)

grld <- data.frame(antPTF=out2$mean$antPTF, attempt=dat2[,5], pop=rep("Greenland", nrow(dat2)), id=dat2[,2])

namc <- data.frame(antPTF=out1$mean$antPTF, attempt=dat2[,5], pop=rep("Midcontinent", nrow(dat2)), id=dat2[,2])

ant <- rbind(namc, grld)

ggplot(ant, aes(x=id, y=antPTF, color=factor(attempt), shape=factor(attempt))) + geom_point(size=3) + 
  theme_bw() +
  geom_vline(xintercept=26.5) +
  scale_color_manual(values=c("red", "blue")) + 
  guides(color=guide_legend(title="Success"), shape=guide_legend(title="Success")) +
  theme(legend.position=c(1,1), legend.justification=c(1,1),
      legend.title=element_text(size=16, face="bold"),
      legend.text=element_text(size=14),
      axis.text=element_text(size=14), axis.title=element_text(size=16))























