##*
##*  Plotting response curves
##*  2022-08-25
##*   
##*

library(tidyverse)
library(boot)
library(patchwork)

theme_set(theme_classic())

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
  scale_color_manual(values=c("#2166ac","#b2182b")) +
  scale_fill_manual(values=c("#2166ac","#b2182b")) +
  xlab("Antecedent PTF") + 
  theme(legend.position="none",
        axis.text.x=element_text(size=12),
        axis.text.y=element_blank(),
        axis.title.x=element_text(size=12, face="bold"),
        axis.title.y=element_blank(),
        panel.border=element_rect(color="black", fill=NA, size=0.5))


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
  scale_color_manual(values=c("#2166ac","#b2182b")) +
  scale_fill_manual(values=c("#2166ac","#b2182b")) +
  xlab("Antecedent ODBA") + 
  theme(legend.position=c(0,0), legend.justification=c(0,0),
        legend.title=element_text(size=12, face="bold"), 
        legend.text=element_text(size=11), 
        legend.background=element_rect(fill=NA),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12, face="bold"),
        panel.border=element_rect(color="black", fill=NA, size=0.5))

## Plot
odba_plot + ptf_plot


#### Plotting (cumulative and daily) weights ####

scaleFUN <- function(x) sprintf("%.2f", x)

## ODBA model
load("results/ODBA_sam.Rdata")

odba.cw <- data.frame(day=1:58, cumulative=out$mean$cum.weight, q2.5=out$q2.5$cum.weight, q97.5=out$q97.5$cum.weight)

p1 <- ggplot(odba.cw) + geom_segment(aes(x=1, y=0, xend=58, yend=1), size=1, color="black") + 
  geom_ribbon(aes(x=day, ymin=q2.5, ymax=q97.5), alpha=0.4) +
  geom_point(aes(x=day, y=cumulative), shape=16, size=1) +
  ylab("Cumulative Weight (ODBA)") + xlab("Days to Arrival on Breeding Areas") +
  theme(axis.text=element_text(size=12),
        axis.title.y=element_text(size=12, face="bold"),
        axis.title.x=element_blank(),
        panel.border=element_rect(color="black", fill=NA, size=0.5))


odba.dw <- data.frame(index=seq(1,length(out$mean$weight),1), 
                      ODBA=out$mean$weight, lcri=out$q2.5$weight, ucri=out$q97.5$weight)

p3 <- ggplot(odba.dw, aes(x=index, y=ODBA)) + 
  geom_segment(aes(x=index, xend=index, y=lcri, yend=ucri)) +
  geom_point() +
  coord_cartesian(ylim=c(0,0.10)) +
  ylab("Daily Weight (ODBA)") +
  xlab("Day of Migration") +
  scale_y_continuous(labels=scaleFUN) +
  theme(axis.text=element_text(size=11),
        axis.title.y=element_text(size=12, face="bold"),
        axis.title.x=element_blank(),
        panel.border=element_rect(color="black", fill=NA, size=0.5))


## PTF model
load("results/PTF_sam.Rdata")

ptf.cw <- data.frame(day=1:58, cumulative=out$mean$cum.weight, q2.5=out$q2.5$cum.weight, q97.5=out$q97.5$cum.weight)

p2 <- ggplot(ptf.cw) + geom_segment(aes(x=1, y=0, xend=58, yend=1), size=1, color="black") + 
  geom_ribbon(aes(x=day, ymin=q2.5, ymax=q97.5), alpha=0.4) +
  geom_point(aes(x=day, y=cumulative), shape=16, size=1) +
  ylab("Cumulative Weight (PTF)") + xlab("Days to Arrival on Breeding Areas") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12, face="bold"),
        panel.border=element_rect(color="black", fill=NA, size=0.5))


ptf.dw <- data.frame(index=seq(1,length(out$mean$weight),1), 
                      PTF=out$mean$weight, lcri=out$q2.5$weight, ucri=out$q97.5$weight)

p4 <- ggplot(ptf.dw, aes(x=index, y=PTF)) + 
  geom_segment(aes(x=index, xend=index, y=lcri, yend=ucri)) +
  geom_point() +
  coord_cartesian(ylim=c(0,0.10)) +
  ylab("Daily Weight (PTF)") +
  xlab("Day of Migration") +
  scale_y_continuous(labels=scaleFUN) +
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=12, face="bold"),
        panel.border=element_rect(color="black", fill=NA, size=0.5))

## Print plots
# Cumulative weights
p1 / p2

# Daily weights
p3 / p4
