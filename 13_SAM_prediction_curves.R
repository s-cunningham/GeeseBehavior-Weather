##*
##*  Plotting response curves
##*  2022-08-25
##*   
##*

library(tidyverse)
library(boot)
# library(patchwork)

theme_set(theme_bw())

## Load data
dat <- read_csv("files_for_models/daily_odba_behavior.csv")
dat <- as.data.frame(dat)

# read in migration dates
mdates <- read_csv("files_for_models/migration_dates.csv")

# Read in attempt/defer
response <- read_csv("files_for_models/attempt_defer_collars.csv")
chars <- "MF"
pop <- ifelse(apply(response[,1], 1, sjmisc::str_contains, c("M", "F"), logic='or'), 1, 2)

# subset to migration dates
dat$RelDay <- NA
dat$RevRelDay <- NA
dat$migration <- NA
un.id <- unique(dat$animal_id)
for (i in 1:length(un.id)) {
  
  # subset migration dates
  md <- mdates[mdates$animal_id==un.id[i],]
  
  # Create vector of days
  mdays <- md$start:md$end
  
  # Add column, indicate when is migration
  dat$RelDay[dat$animal_id==un.id[i] & (dat$julian %in% mdays)] <- 1:length(mdays)
  dat$RevRelDay[dat$animal_id==un.id[i] & (dat$julian %in% mdays)] <- length(mdays):1
  dat$migration[dat$animal_id==un.id[i] & (dat$julian %in% mdays)] <- "yes"
  
}

# Subset to just migration
dat <- dat[!is.na(dat$migration), 1:20]

# subset to only collar birds
dat <- dat[dat$tag!="EOBS",]
un.id <- unique(dat$animal_id)
mdates <- mdates[mdates$animal_id %in% un.id,]

# Scale covariates
dat[,16] <- scale(dat[,16]) 

## Results of PTF as covariate and plot
load("results/PTF_sam.Rdata")

# Set up posterior samples
beta0 <- c(out$samples[[1]][,1], out$samples[[2]][,1], out$samples[[3]][,1])
beta1 <- c(out$samples[[1]][,2], out$samples[[2]][,2], out$samples[[3]][,2])
beta2 <- c(out$samples[[1]][,3], out$samples[[2]][,3], out$samples[[3]][,3])
beta3 <- c(out$samples[[1]][,4], out$samples[[2]][,4], out$samples[[3]][,4])

# Predict
pred_length <- 100
ptf_pred <- seq(-3.86,3.10,length.out=pred_length)

nmcmc <- out$mcmc.info$n.samples

ptf_gr <- matrix(, nmcmc, pred_length)
ptf_mc <- matrix(, nmcmc, pred_length)
for (i in 1:pred_length) {
  ptf_gr[,i] <- inv.logit(beta0 + beta1*ptf_pred[i] + beta2 + beta3*ptf_pred[i])
  ptf_mc[,i] <- inv.logit(beta0 + beta1*ptf_pred[i] )
}

ptf_gr_qt <- apply(ptf_gr, 2, quantile, probs=c(0.5, 0.1, 0.90))
ptf_mc_qt <- apply(ptf_mc, 2, quantile, probs=c(0.5, 0.1, 0.90))

df1 <- data.frame(y=ptf_gr_qt[1,], x=ptf_pred, up1=ptf_gr_qt[2,], lo1=ptf_gr_qt[3,], Population="Greenland")
df2 <- data.frame(y=ptf_mc_qt[1,], x=ptf_pred, up1=ptf_mc_qt[2,], lo1=ptf_mc_qt[3,], Population="Midcontinent")

df <- rbind(df1, df2)

ggplot(df, aes(color=Population, fill=Population)) + 
  geom_ribbon(aes(x=x, ymin=lo1, ymax=up1), alpha=0.2, linetype="dotted") +
  geom_line(aes(x=x, y=y), size=1) 


