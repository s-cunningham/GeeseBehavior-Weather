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
dat$median.odba <- log(dat$median.odba)
dat[,c(16,18)] <- scale(dat[,c(16,18)]) 
 
#### Plotting ln(median.ODBA) vs ptf by population ####
ggplot(dat, aes(x=ptf, y=median.odba)) + geom_point() + facet_grid(.~pop)


#### Results of PTF as covariate and plot ####
load("results/PTF_sam.Rdata")

# Set up posterior samples
alpha <- c(out$samples[[1]][,1], out$samples[[2]][,1], out$samples[[3]][,1])
beta1 <- c(out$samples[[1]][,2], out$samples[[2]][,2], out$samples[[3]][,2])
beta2_1 <- c(out$samples[[1]][,3], out$samples[[2]][,3], out$samples[[3]][,3])
beta2_2 <- c(out$samples[[1]][,4], out$samples[[2]][,4], out$samples[[3]][,4])
beta3_1 <- c(out$samples[[1]][,5], out$samples[[2]][,5], out$samples[[3]][,5])
beta3_2 <- c(out$samples[[1]][,6], out$samples[[2]][,6], out$samples[[3]][,6])

# Predict
pred_length <- 100
ptf_pred <- seq(-3.86,3.10,length.out=pred_length)

nmcmc <- out$mcmc.info$n.samples

ptf_gr <- matrix(, nmcmc, pred_length)
ptf_mc <- matrix(, nmcmc, pred_length)
for (i in 1:pred_length) {
  ptf_mc[,i] <- inv.logit(alpha + beta1*ptf_pred[i] + beta2_1 + beta3_1*ptf_pred[i])
  ptf_gr[,i] <- inv.logit(alpha + beta1*ptf_pred[i] + beta2_2 + beta3_2*ptf_pred[i])
}

ptf_gr_qt <- apply(ptf_gr, 2, quantile, probs=c(0.5, 0.1, 0.90))
ptf_mc_qt <- apply(ptf_mc, 2, quantile, probs=c(0.5, 0.1, 0.90))

df1 <- data.frame(y=ptf_gr_qt[1,], x=ptf_pred, up1=ptf_gr_qt[2,], lo1=ptf_gr_qt[3,], Population="Greenland")
df2 <- data.frame(y=ptf_mc_qt[1,], x=ptf_pred, up1=ptf_mc_qt[2,], lo1=ptf_mc_qt[3,], Population="Midcontinent")

df <- rbind(df1, df2)

ptf_plot <- ggplot(df, aes(color=Population, fill=Population)) + 
  geom_ribbon(aes(x=x, ymin=lo1, ymax=up1), alpha=0.2, linetype="dotted") +
  geom_line(aes(x=x, y=y), size=1) + ylab("Probability of Breeding Deferral") +
  xlab("Scaled/Centered Proportion time feeding") + 
  theme(legend.position="bottom")


#### Results of ODBA as covariate and plot ####
load("results/ODBA_sam.Rdata")

# Set up posterior samples
alpha <- c(out$samples[[1]][,1], out$samples[[2]][,1], out$samples[[3]][,1])
beta1 <- c(out$samples[[1]][,2], out$samples[[2]][,2], out$samples[[3]][,2])
beta2_1 <- c(out$samples[[1]][,3], out$samples[[2]][,3], out$samples[[3]][,3])
beta2_2 <- c(out$samples[[1]][,4], out$samples[[2]][,4], out$samples[[3]][,4])
beta3_1 <- c(out$samples[[1]][,5], out$samples[[2]][,5], out$samples[[3]][,5])
beta3_2 <- c(out$samples[[1]][,6], out$samples[[2]][,6], out$samples[[3]][,6])
# Predict
pred_length <- 100
odba_pred <- seq(0.026,1.55,length.out=pred_length)

nmcmc <- out$mcmc.info$n.samples

odba_gr <- matrix(, nmcmc, pred_length)
odba_mc <- matrix(, nmcmc, pred_length)
for (i in 1:pred_length) {
  odba_mc[,i] <- inv.logit(alpha + beta1*odba_pred[i] + beta2_1 + beta3_1*odba_pred[i])
  odba_gr[,i] <- inv.logit(alpha + beta1*odba_pred[i] + beta2_2 + beta3_2*odba_pred[i])
}

odba_gr_qt <- apply(odba_gr, 2, quantile, probs=c(0.5, 0.1, 0.90))
odba_mc_qt <- apply(odba_mc, 2, quantile, probs=c(0.5, 0.1, 0.90))

df1 <- data.frame(y=odba_gr_qt[1,], x=odba_pred, up1=odba_gr_qt[2,], lo1=odba_gr_qt[3,], Population="Greenland")
df2 <- data.frame(y=odba_mc_qt[1,], x=odba_pred, up1=odba_mc_qt[2,], lo1=odba_mc_qt[3,], Population="Midcontinent")

df <- rbind(df1, df2)

odba_plot <- ggplot(df, aes(color=Population, fill=Population)) + 
  geom_ribbon(aes(x=x, ymin=lo1, ymax=up1), alpha=0.2, linetype="dotted") +
  geom_line(aes(x=x, y=y), size=1) + ylab("Probability of Breeding Deferral") +
  xlab("Scaled/Centered ln(median daily ODBA)") + 
  theme(legend.position="bottom")



