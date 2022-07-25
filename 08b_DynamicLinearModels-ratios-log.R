
library(tidyverse)
library(jagsUI)

# Read in data
dat <-read.csv("output/dlm_emm_data-prop20210824.csv", stringsAsFactors=FALSE)
dat <- dat[,-1]
dat <- dat[dat$animal_id!="LM17M" & dat$animal_id!="RP15F",]

dat$prop.odba <- log(dat$prop.odba)

# Match weather to data
dat$date <- as.Date(dat$date)

wd <- read.csv("output/weather_covars.csv", stringsAsFactors=FALSE)
wd <- wd[,-1]
wd$date <- as.Date(wd$date)

dat <- left_join(dat, wd, by=c("animal_id", "date"))

# miss <- dat[is.na(dat$prate),]
dat <- dat[!is.na(dat$prate),]

# Check for correlation
cor(dat[,c(25:ncol(dat))])

# Save values for model
nobs <- dim(dat)[1]
nind <- length(unique(dat$id_ind))

# What is the maximum duration of spring migration?
ranges <- aggregate(julian ~ animal_id + id_ind, data=dat, FUN=range)
dr <- data.frame(id=ranges$animal_id, id_ind=ranges$id_ind, start=ranges$julian[,1], end=ranges$julian[,2])
dr$id <- as.character(dr$id)
dr <- mutate(dr, length=end-start)
dr[dr$length==max(dr$length),]
dr$length <- dr$length + 1
# write.csv(dr, "output/migration_days.csv")

dat[,c(25:36)] <- scale(dat[,c(25:36)]) 

dat <- dat[,c(1:11,13,15,21,22,24,26,35,36)]

d2 <- pivot_longer(dat, 17:19, names_to="variable", values_to="value")
d2 <- d2[d2$variable=="weqsd" & d2$julian>=120,]
d2$year <- as.character(d2$year)

ggplot(d2, aes(x=julian, y=value, color=pop, group=animal_id)) + geom_point() + facet_wrap(.~year)


# Set up data matrices
prate <- matrix(NA, nrow=nind, ncol=max(dr$length))
mintemp <- matrix(NA, nrow=nind, ncol=max(dr$length))
weqsd <- matrix(NA, nrow=nind, ncol=max(dr$length))

Y <- matrix(NA, nrow=nind, ncol=max(dr$length))
beta1 <- matrix(NA, nrow=nind, ncol=max(dr$length))
beta2 <- matrix(NA, nrow=nind, ncol=max(dr$length))

for (i in 1:length(dr$id)) {
  r <- dim(dat[dat$animal_id==dr$id[i],])[1]
  prate[i,1:r] <- dat[dat$animal_id==dr$id[i],"prate"]
  mintemp[i,1:r] <- dat[dat$animal_id==dr$id[i],"mintemp"]
  
  Y[i,1:r] <- dat[dat$animal_id==dr$id[i],"prop.odba"] 
  beta1[i,1:r] <- rnorm(dr$length[i])
  beta2[i,1:r] <- rnorm(dr$length[i])
}


# Run each model individually
sink("R/dlm_prop.odba_ind_normal.txt")
cat("
    model {

    # Priors
    beta0 ~ dnorm(0, 0.01) # Intercept

    mu1 ~ dnorm(0, 0.01) # beta1[1]
    mu2 ~ dnorm(0, 0.01) # beta2[1]
    
    eta.p1 ~ dgamma(0.001, 0.001) # Process variance for covariate 1
    sd.q1 <- 1/sqrt(eta.p1)
    
    eta.p2 ~ dgamma(0.001, 0.001) # Process variance for covariate 1
    sd.q2 <- 1/sqrt(eta.p2)

    phi ~ dgamma(0.1, 0.1) # parameter for beta regression
    
    sigma ~ dunif(0,100)
    tau <- 1/(sigma*sigma)
    
    # Initialize (fill in first value)
    beta1[1] <- mu1
    beta2[1] <- mu2
    
    y[1] ~ dnorm(mu[1], tau)
    mu[1] <- beta0 + beta1[1]*prate[1] + beta2[1]*temp[1]

    
    # Likelihood
    for (j in 2:dur) {
      
      # Process model 
      predX1[j] <- 1*beta1[j-1]
      beta1[j] ~ dnorm(predX1[j], eta.p1)	# coefficients for precipitation rate
      
      predX2[j] <- 1*beta2[j-1]
      beta2[j] ~ dnorm(predX2[j], eta.p2)	# coefficiencts for temperature
      
      # Observation model
      y[j] ~ dnorm(mu[j], tau)
      mu[j] <- beta0 + beta1[j]*prate[j] + beta2[j]*temp[j]
    
    }

    
}", fill=TRUE)
sink()

# Run model for each bird

dr2 <- dr[dr$id=="17762" | dr$id=="17778",]
dr <- dr[dr$id!="17762" & dr$id!="17778",]

for (i in 1:nrow(dr)) {
  
  y <- Y[i,]
  p <- prate[i,]
  temp <- mintemp[i,]
  d <- dr$length[i]
  
  b1 <- beta1[i,]
  b2 <- beta2[i,]
  bird <- dr$id[i]
  
  # plot(y[!is.na(y)], type="l", ylab="ODBAgraze", xlab="migration day")
  # 
  # plot(p[!is.na(p)], type="l", ylab="Weather covariates", xlab="migration day", col="red", lwd=2, ylim=c(-2,4))
  # lines(temp[!is.na(temp)], col="blue", lwd=2)
  
  # Bundle data
  jags.data <- list(y=y, prate=p, temp=temp, dur=d)
  
  # Forced random walks
  inits <- function() {list(mu1=rnorm(1), mu2=rnorm(1), sigma=rlnorm(1),
                            eta.p1=1, eta.p2=1, phi=1,
                            beta0=rnorm(1), beta1=b1[1:d], beta2=b2[1:d])}
  
  # Parameters monitored
  params <- c("sd.q1", "sd.q2","phi","beta0", "beta1", "beta2")
  
  # MCMC settings
  na <- 2000
  ni <- 70000
  nb <- 35000
  nt <- 1
  nc <- 3
  # ni <- 35000
  # nb <- 10000
  # nt <- 1
  # nc <- 3
  
  # Run model
  out1 <- jags(jags.data, inits, params, "R/dlm_prop.odba_ind_normal.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, parallel=TRUE)
  
  traceplot(out1)
  whiskerplot(out1, parameters="beta2")
  
  print(out1, digits=3)
  print(range(out1$Rhat))
  
  filename = paste0("output/JAGS/ratioDLMs/20210824/ratio_mtemp-prate_", bird, ".Rdata" )
  # save(file=filename, list="out1")
  
  smry <- as.data.frame(out1$summary)
  
  # csvname = paste0("output/modelcoefficients/dlm_20210824/mtemp-prate_ratio_dlm_summary_", bird, ".csv" )
  # write.csv(smry, csvname)
  
}

files <- list.files("output/modelcoefficients/dlm_20210824", pattern=".csv", all.files=TRUE)
for (i in 1:length(files)) {
  
  temp <- read.csv(paste0("output/modelcoefficients/dlm_20210824/",files[i]))
  print(i)
  print(range(temp$Rhat))
  
}



# ## Update models that did not converge
# x <- 8
# ids <- c("17762", "17812", "17814", "2825", "2830", "RP01F", "RP20F", "RP22F")
# ids2 <- c("17812", "2825", "RP22F")
# for (i in 1:x) {
#   
#   bird <- ids2[i]
#   
#   filename = paste0("output/JAGS/ratio_weqsd-prate_", bird, ".Rdata" )
#   load(filename)
#   
#   out1 <- update(out1, parameters.to.save=params, n.adapt=2000, n.iter=20000, n.thin=1)
#   
#   print(bird)
#   print(range(out1$Rhat))
#   
#   save(file=filename, list="out1")
#   
#   smry <- as.data.frame(out1$summary)
#   
#   csvname = paste0("output/modelcoefficients/dlm/mtemp-prate_ratio_dlm_summary_", bird, ".csv" )
#   write.csv(smry, csvname)
# }

