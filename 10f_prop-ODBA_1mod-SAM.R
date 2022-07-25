## This is the one for the manuscript!!

library(jagsUI)
library(tidyverse)
library(zoo)

# Read in data
dat <-read.csv("output/dlm_emm_data3.csv", stringsAsFactors=FALSE)
dat <- dat[,-1]
names(dat)[1] <- "animal_id"
dat$tag[dat$animal_id=="2160"] <- "EOBS"
dat <- dat[dat$animal_id!="LM31F" & dat$animal_id!="LM17M" & dat$animal_id!="RP15F",]

# Update breeding success
dat$attempt <- 0
dat$attempt[dat$animal_id=="17763" | dat$animal_id=="17777" |
              dat$animal_id=="17778" | dat$animal_id=="17780" |
              dat$animal_id=="2161" | dat$animal_id=="2838" |
              dat$animal_id=="RP01F" | dat$animal_id=="RP08F" | 
              dat$animal_id=="RP17F" | dat$animal_id=="RP22F"] <- 1

# Do some stuff because we skipped a few steps
dat <- dat[dat$timeperiod>=1,]

dat <- unite(dat, "key", c(1,5), sep="_", remove=FALSE)

dat$birdno <- as.numeric(factor(dat$key, labels=seq(1,34,1)))

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

dat <- mutate(dat, ptf=graze/total)
dat <- mutate(dat, odbaptf=g.odba*ptf)

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
} # interpolate the handful of missing values

dat$f.odba[is.na(dat$f.odba)] <- 0
dat$s.odba[is.na(dat$s.odba)] <- 0

# dat <- mutate(dat, prop.odba=g.odba/cum.odba)
dat <- mutate(dat, prop.odba=g.odba/(f.odba+s.odba))
# write.csv(dat, "output/dlm_emm_data-prop20210824.csv")

# calculate range 
range(dat$prop.odba)

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
    temp2 <- subset(temp, rel.day<=51) 
    dat <- rbind(dat, temp2)
}

plot(temp$julian, temp$rel.day)

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

m.odbag <- dat %>% group_by(animal_id) %>% summarize(mog=mean(prop.odba))
dat <- left_join(dat, m.odbag, by="animal_id")

dat <- mutate(dat, dif.og=prop.odba-mog)

ggplot(dat, aes(y=prop.odba, x=factor(attempt), fill=pop)) + geom_boxplot() +
  scale_fill_manual(values=c("#2166ac", "#b2182b")) +
  theme_classic() + theme(panel.border=element_rect(color="black", fill=NA, size=0.5),
                          legend.position="bottom",
                          legend.title=element_text(size=16, face="bold"),
                          legend.background=element_rect(fill="white", colour="black"))


ggplot(dat, aes(x=julian, y=dif.og, group=animal_id, color=animal_id)) + geom_line()
dat$dif.og <- scale(dat$dif.og)
dat$prop.odba <- scale(dat$prop.odba)


#### Density plots of variables ####
# dat <- mutate(dat, ptf=graze/total)
# dat$mean.odba <- log(dat$mean.odba)
# dat$prop.odba <- scale(dat$prop.odba)
# dat$ptf <- scale(dat$ptf)
# dat$mean.odba <- scale(dat$mean.odba)
# 
# den <- dat[,c(1,2,5:8,16,23,24)]
# den <- pivot_longer(den, cols=7:9, names_to="metric", values_to="values")
# 
# ggplot(den, aes(x=values, color=metric, fill=metric)) + geom_density(alpha=0.4) + 
#   theme_bw() + ggtitle("variables standardized, odba log-transformed")
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

dat2$yrnr <- as.numeric(factor(dat2$year, levels=c(2012,2013,2017,2018), labels=c(1,2,3,4)))
dat2$pop <- ifelse(dat2$pop=="GRLD", 0, 1)

yrs <- length(unique(dat2$yrnr))


### Run model
sink("R/ecomem_2.txt")
cat("
    model {
    
    ## Priors for regression parameters
    # Random year effect
    for (i in 1:yrs) {
        beta0[i] ~ dnorm(0, tau.alpha)  
    }
    tau.alpha <- 1/(sd.alpha*sd.alpha)
    sd.alpha ~ dunif(0,100) 
    
    mu ~ dnorm(0, 0.01)
    
    # Slope parameters
    beta1 ~ dnorm(0, 0.01)
    beta2 ~ dnorm(0, 0.01)
    beta3 ~ dnorm(0, 0.01)
    
    ### 
    # Dirichlet prior for daily ODBA weights
    for(j in 1:mig.len){
      delta[j] ~ dgamma(1,1)
    }

    for (j in 1:mig.len) {
      weight[j] <- delta[j]/sumD
        for (i in 1:nind) {
        
        # For each time period into the past, compute weighted ODBA variable
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

    # Compute antecedent ODBA by summing weighted ODBA variable over past days
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
jags.data <- list(ad=dat2[,5], nind=nind, mig.len=un.mig, ptf=Y, year=dat2$yrnr, yrs=yrs, pop=dat2[,4])

# Initial values
inits <- function() {list(beta1=rnorm(1), beta2=rnorm(1), beta3=rnorm(1),
                          delta=rep(1,un.mig), sig=1, mu=runif(1,0,2), sd.alpha=runif(1,0,0.1))}

# Parameters monitored
params <- c("sig", "sd.alpha",  "beta0", "beta1", "beta2", "beta3", "p", "delta", 
            "antPTF", "weight", "antX1", "weightOrdered", "cum.weight")

# MCMC settings
ni <- 10000  
nb <- 5000
nt <- 1
nc <- 3

# Run model
out1 <- jags(jags.data, inits, params, "R/ecomem_2.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, parallel=TRUE)

print(out1, digits=2)

range(out1$Rhat)
# 
# smry <- as.data.frame(out1$summary)
# write.csv(smry, "output/modelcoefficients/1model_ecomem_20210503.csv")
# 
# save(out1, file="output/modelcoefficients/1model_ecomem_20210503.RData")


whiskerplot(out1, parameters = c("beta1", "beta2", "beta3"))

load("output/modelcoefficients/1model_ecomem_20210503.RData")
dat2 <- cbind(dat2, out1$mean$antPTF)
names(dat2)[8] <- "antODBAg"
ggplot(dat2, aes(y=antODBAg, x=factor(attempt))) + geom_boxplot() 

dat2 %>% group_by(attempt) %>% summarize(mean(antODBAg))

par(mfrow=c(1,1))
plot(out1$mean$cum.weight, type="n", ylab="Cumulative Weights", main="difference from mean")
segments(x0=1:un.mig, y0=out1$q2.5$cum.weight, x1=1:un.mig, y1=out1$q97.5$cum.weight, col="gray70")
points(out1$mean$cum.weight, pch=19)
segments(x0=1, y0=(1/un.mig), x1=un.mig, y1=1, col="black", lwd=1)


cw <- as.data.frame(out1$mean$cum.weight)
names(cw)[1] <- "cumulative"
cw$days <- 1:un.mig
cw$q2.5 <- out1$q2.5$cum.weight
cw$q97.5 <- out1$q97.5$cum.weight

ggplot(cw) +
  geom_segment(aes(x=1, y=(1/51), xend=51, yend=1)) +
  geom_ribbon(aes(x=days, ymin=q2.5, ymax=q97.5), alpha=0.4) +
  geom_point(aes(x=days, y=cumulative)) + 
  theme_classic() +
  xlab("Days prior to end of migration") + ylab("Cumulative weight") +
  theme(panel.border=element_rect(color="black", fill=NA, size=0.5),
        axis.text=element_text(size=12), axis.title=element_text(size=14))


wo <- as.data.frame(out1$mean$weightOrdered)
wo$q97.5 <- out1$q97.5$weightOrdered
wo$q2.5 <- out1$q2.5$weightOrdered
names(wo)[1] <- "weight"
wo$days <- 1:un.mig

ggplot(wo) +
  geom_ribbon(aes(x=days, ymin=q2.5, ymax=q97.5), alpha=0.4) +
  geom_hline(aes(yintercept=(1/51)), color="red") + 
  geom_point(aes(x=days, y=weight)) +
  coord_cartesian(ylim=c(0,0.15)) +
  theme_classic() +
  xlab("Days prior to end of migration") + ylab("Daily weight") +
  theme(panel.border=element_rect(color="black", fill=NA, size=0.5),
        axis.text=element_text(size=12), axis.title=element_text(size=14))



plot(out1$mean$weightOrdered, ylim=c(0, .2), type="h")
par(new=TRUE)
antX1 <- out1$mean$antX1
matplot(antX1, type="l", col="gray70", lty=1)


whiskerplot(out1, parameters=c("beta1", "beta2", "beta3"))


plot(out1$mean$antPTF)

ant <- data.frame(antPTF=out1$mean$antPTF, attempt=dat2[,5], pop=dat2[,4], id=dat2[,2])
ant$attempt <- ifelse(ant$attempt==1, "Succeed", "Defer/Fail")

ggplot(ant, aes(x=id, y=antPTF, color=factor(attempt), shape=factor(attempt))) + geom_point(size=3) + 
  theme_bw() +
  geom_vline(xintercept=26.5) +
  labs(y=expression("Antecedent ODBA "[graze]), x="Goose ID") +
  scale_color_manual(values=c("red", "blue")) + 
  guides(color=guide_legend(title="Success"), shape=guide_legend(title="Success")) +
  theme(legend.position=c(1,1), legend.justification=c(1,1),
        legend.title=element_blank(),
        legend.text=element_text(size=14),
        axis.text=element_text(size=14), axis.title=element_text(size=16))


ant %>% group_by(attempt) %>% summarise(mean(antPTF))




