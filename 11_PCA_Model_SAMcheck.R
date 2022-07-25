library(glmnet)
library(tidyverse)

dat1 <-read.csv("output/dlm_emm_data.csv", stringsAsFactors=FALSE)
dat1 <- dat1[,-1]

un.id <- unique(dat1$animal_id)

dat <- data.frame()
for (i in 1:length(un.id)) {
  temp <- dat1[dat1$animal_id==un.id[i],]
  if (temp$pop[1]=="GRLD") {
    temp2 <- subset(temp, rel.day<=45) # original 62
    dat <- rbind(dat, temp2)
  } else {
    temp2 <- subset(temp, rel.day<=89)  # original 93
    dat <- rbind(dat, temp2)
  }
}

dat <- subset(dat, pop=="GRLD")

# Determine duration of observation period for each bird
ranges <- aggregate(julian ~ animal_id, data=dat, FUN=range)
dr <- data.frame(id=ranges$animal_id, start=ranges$julian[,1], end=ranges$julian[,2])
dr$id <- as.character(dr$id)
dr <- mutate(dr, length=end-start)
# dr[dr$length==max(dr$length),]
dur <- dr$length + 1
un.dur <- unique(dur)

nind <- length(unique(dat$animal_id))

dat$l.odba.s <- scale(dat$log.odba, center=TRUE, scale=FALSE)

# Set up data matrices
data_mat <- matrix(NA,nrow=nind, ncol=max(dur))

un.id <- unique(dat$animal_id)
for (i in 1:length(un.id)) {
  r <- dim(dat[dat$animal_id==un.id[i],])[1]
  data_mat[i,1:r] <- dat[dat$animal_id==un.id[i],26] # ODBA (pre-centered)
  # data_mat[i,1:r] <- dat[dat$animal_id==un.id[i],14] # ODBA (not centered)
  # data_mat[i,1:r] <- dat[dat$animal_id==un.id[i],16] # PTF
}

dat2 <- dat[,c(1,4,24,11,9,12)]
dat2 <- distinct(dat2)

Y <- dat2[,5]

#### Calculate the PCA ####
## data_mat should be nxT and centered for svd 
A <- data_mat*(1/(sqrt(nrow(data_mat))-1))
E <- svd(A)
dataPCA <- E$v

#### Project the data into the PC space ####
X <- data_mat %*% dataPCA

#### Fit the GLM ####

lasso_fit <- cv.glmnet(X, Y, alpha=1, nfolds = 40, family="binomial")
            # check the glmnet help page for the defaults for fitting glmnet
            # some defaults are include an intercept and standardize the covariates
lasso_coefs <- predict(lasso_fit, lambda = lasso1$lambda.min, type="coefficients") %>% 
  as.numeric()

#### Pull out the significant PCs ####
sub <- dataPCA*(lasso_coefs[-1] != 0) #-1 for the intercept
b_line <- apply(X%*%t(sub), 1, sum)

#### Plot ####

plot(b_line,type="l")





