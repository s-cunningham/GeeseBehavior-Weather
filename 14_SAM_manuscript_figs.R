#**********************************************************************************************************************************
#**********************************************************************************************************************************

# Project: Thesis Chapter 1 Objective 2
# Date: 3 June 2019
# Author: Stephanie Cunningham
# Description: Converting Hypothesis 1 to time series format (dynamic linear model)

#**********************************************************************************************************************************
#**********************************************************************************************************************************


## 1 October 2019
## Author: Stephanie Cunningham

library(tidyverse)
library(gridExtra)
library(egg)
library(ggpmisc)

rm(list=ls())

# Load JAGS objects
load("output/ecomem_NAMCodba3.Rdata")  # out1
# print(out1, digits=3)
load("output/ecomem_NAMC3ptf.Rdata")  # out2
# print(out2, digits=3)
load("output/ecomem_GRLDodba3.Rdata")  # out3
# print(out3, digits=3)
load("output/ecomem_GRLD3ptf.Rdata")  # out4
# print(out4, digits=3)

#### Violin plots
data_summary <- function(x) {
  m <- mean(x)
  mq <- quantile(x, probs=c(0.025, 0.975))
  ymin <- as.numeric(mq[1])
  ymax <- as.numeric(mq[2])
  return(c(y=m, ymin=ymin, ymax=ymax))
}


df.out1 <- data.frame(beta0=out1$sims.list$beta0, beta1=out1$sims.list$beta1, beta2=out1$sims.list$beta2)
df.out1 <- gather(df.out1, key=parameter, value=value, 1:3)
df.out1$parameter[df.out1$parameter=="beta0"] <- "Intercept"
df.out1$parameter[df.out1$parameter=="beta1"] <- "Antecedent\nEffect"
df.out1$parameter[df.out1$parameter=="beta2"] <- "2017\nEffect"
df.out1$parameter <- factor(df.out1$parameter, levels=c("Intercept", "Antecedent\nEffect", "2017\nEffect"))

na.odba <- ggplot(df.out1, aes(x=parameter, y=value)) + 
  geom_hline(yintercept=0, color="red") + 
  geom_violin(fill="gray80", size=1, color="gray50") + 
  coord_cartesian(ylim=c(-40,40)) + 
  theme_classic() + ylab("Posterior Distribution") + 
  stat_summary(fun.data=data_summary, size=0.75) +
  theme(axis.text.x=element_text(size=14), axis.text.y=element_blank(),
        axis.title=element_blank(),
        panel.border=element_rect(color="black", fill=NA, size=0.5)) #+
  # geom_text(aes(x=0.6, y=40, label="D"), size=7)



df.out2 <- data.frame(beta0=out2$sims.list$beta0, beta1=out2$sims.list$beta1, beta2=out2$sims.list$beta2)
df.out2 <- gather(df.out2, key=parameter, value=value, 1:3)
df.out2$parameter[df.out2$parameter=="beta0"] <- "Intercept"
df.out2$parameter[df.out2$parameter=="beta1"] <- "Antecedent\nEffect"
df.out2$parameter[df.out2$parameter=="beta2"] <- "2017\nEffect"
df.out2$parameter <- factor(df.out2$parameter, levels=c("Intercept", "Antecedent\nEffect", "2017\nEffect"))


na.ptf <- ggplot(df.out2, aes(x=parameter, y=value)) + 
  geom_hline(yintercept=0, color="red") + 
  geom_violin(fill="gray80", size=1, color="gray50") + 
  coord_cartesian(ylim=c(-40,40)) + ggtitle("Midcontinent") +
  theme_classic() + ylab("Posterior Distribution") + 
  stat_summary(fun.data=data_summary, size=0.75) +
  theme(axis.text=element_blank(), 
        axis.title=element_blank(),plot.title=element_text(size=16, face="bold", hjust=0.5),
        panel.border=element_rect(color="black", fill=NA, size=0.5)) #+
  # geom_text(aes(x=0.6, y=42, label="B"), size=7)



df.out3 <- data.frame(beta0=out3$sims.list$beta0, beta1=out3$sims.list$beta1, beta2=out3$sims.list$beta2, beta3=out3$sims.list$beta3)
df.out3 <- gather(df.out3, key=parameter, value=value, 1:4)
df.out3$parameter[df.out3$parameter=="beta0"] <- "Intercept"
df.out3$parameter[df.out3$parameter=="beta1"] <- "Antecedent\nEffect"
df.out3$parameter[df.out3$parameter=="beta2"] <- "2012\nEffect"
df.out3$parameter[df.out3$parameter=="beta3"] <- "2013\nEffect"
df.out3$parameter <- factor(df.out3$parameter, levels=c("Intercept", "Antecedent\nEffect", "2012\nEffect", "2013\nEffect"))

gr.odba <- ggplot(df.out3, aes(x=parameter, y=value)) + 
  geom_hline(yintercept=0, color="red") + 
  geom_violin(fill="gray80", size=1, color="gray50") + 
  theme_classic() + ylab("Posterior Distribution (ODBA)") + 
  coord_cartesian(ylim=c(-40,40)) + 
  stat_summary(fun.data=data_summary, size=0.75) +
  theme(axis.text=element_text(size=14), axis.title.y=element_text(size=16, face="bold"), 
        axis.title.x=element_blank(),
        panel.border=element_rect(color="black", fill=NA, size=0.5)) #+
  # geom_text(aes(x=0.6, y=40, label="C"), size=7)


df.out4 <- data.frame(beta0=out4$sims.list$beta0, beta1=out4$sims.list$beta1, beta2=out4$sims.list$beta2, beta3=out4$sims.list$beta3)
df.out4 <- gather(df.out4, key=parameter, value=value, 1:4)
df.out4$parameter[df.out4$parameter=="beta0"] <- "Intercept"
df.out4$parameter[df.out4$parameter=="beta1"] <- "Antecedent\nEffect"
df.out4$parameter[df.out4$parameter=="beta2"] <- "2012\nEffect"
df.out4$parameter[df.out4$parameter=="beta3"] <- "2013\nEffect"
df.out4$parameter <- factor(df.out4$parameter, levels=c("Intercept", "Antecedent\nEffect", "2012\nEffect", "2013\nEffect"))

gr.ptf <- ggplot(df.out4, aes(x=parameter, y=value)) + 
  geom_hline(yintercept=0, color="red") + 
  geom_violin(fill="gray80", size=1, color="gray50") + 
  theme_classic() + ylab("Posterior Distribution (PTF)") + 
  coord_cartesian(ylim=c(-40,40)) + ggtitle("Greenland") +
  stat_summary(fun.data=data_summary, size=0.75) +
  theme(axis.text=element_text(size=14), axis.title.y=element_text(size=16, face="bold"), 
        axis.title.x=element_blank(), axis.text.x=element_blank(),
        plot.title=element_text(size=16, face="bold", hjust=0.5),
        panel.border=element_rect(color="black", fill=NA, size=0.5))# +
  # geom_text(aes(x=0.6, y=20, label="A"), size=7)

# grid.arrange(gr.ptf, gr.odba, nrow=2) # save as: 1100 x 745
# grid.arrange(na.ptf, na.odba, nrow=2) # save as: 733 x 745


lay <- rbind(c(1,1,1,2,2),
             c(1,1,1,2,2),
             c(3,3,3,4,4),
             c(3,3,3,4,4))

grid.arrange(gr.ptf,na.ptf,gr.odba, na.odba, layout_matrix=lay)




### Plot cumulative weights

# plot(out2$mean$cum.weight, pch=20, ylab="Cumulative Weights", main="")
# abline(h=0.9, col="gray90")
# segments(x0=1,x1=un.dur,y0=0,y1=1)
# segments(x0=1:un.dur, x1=1:un.dur, y0=out2$q2.5$cum.weight, y1=out2$q97.5$cum.weight, col="gray40")

cw.namc.ptf <- data.frame(day=1:88, cumulative=out2$mean$cum.weight, q2.5=out2$q2.5$cum.weight, q97.5=out2$q97.5$cum.weight)
cw.grld.ptf <- data.frame(day=1:50, cumulative=out4$mean$cum.weight, q2.5=out4$q2.5$cum.weight, q97.5=out4$q97.5$cum.weight)
cw.namc.odba <- data.frame(day=1:88, cumulative=out1$mean$cum.weight, q2.5=out1$q2.5$cum.weight, q97.5=out1$q97.5$cum.weight)
cw.grld.odba <- data.frame(day=1:50, cumulative=out3$mean$cum.weight, q2.5=out3$q2.5$cum.weight, q97.5=out3$q97.5$cum.weight)


n.ptf <- ggplot(cw.namc.ptf) + geom_segment(aes(x=1, y=0, xend=88, yend=1), size=1, color="black") + 
  # geom_hline(aes(yintercept=0.9), color="gray70") +
  geom_ribbon(aes(x=day, ymin=q2.5, ymax=q97.5), alpha=0.4) +
  geom_point(aes(x=day, y=cumulative), shape=16, size=1) + 
  # geom_segment(aes(x=1:89, xend=1:89, y=q2.5, yend=q97.5)) +
  ggtitle("Midcontinent") +
  theme_classic() + ylab("Cumulative Weight (PTF)") + xlab("Days to Arrival on Breeding Areas") +
  theme(axis.text.y=element_text(size=14), axis.text.x=element_blank(),
        axis.title=element_text(size=16, face="bold"), axis.title.x=element_blank(),
        plot.title=element_text(size=16, face="bold", hjust=0.5),
        panel.border=element_rect(color="black", fill=NA, size=0.5)) #+
  # geom_text(aes(x=0, y=1, label="A"), size=5)

g.ptf <- ggplot(cw.grld.ptf) + geom_segment(aes(x=1, y=0, xend=50, yend=1), size=1, color="black") + 
  # geom_hline(aes(yintercept=0.9), color="gray70") +
  geom_ribbon(aes(x=day, ymin=q2.5, ymax=q97.5), alpha=0.4) +
  geom_point(aes(x=day, y=cumulative), shape=16, size=1) + 
  ggtitle("Greenland") +
  theme_classic() + xlab("Days to Arrival on Breeding Areas") +
  theme(axis.text=element_blank(), 
        # axis.title=element_text(size=16, face="bold"), 
        axis.title.x=element_blank(),
        plot.title=element_text(size=16, face="bold", hjust=0.5),
        axis.title.y=element_blank(), panel.border=element_rect(color="black", fill=NA, size=0.5)) #+
  # geom_text(aes(x=0, y=1, label="B"), size=5)

n.odba <- ggplot(cw.namc.odba) + geom_segment(aes(x=1, y=0, xend=88, yend=1), size=1, color="black") + 
  # geom_hline(aes(yintercept=0.9), color="gray70") +
  geom_ribbon(aes(x=day, ymin=q2.5, ymax=q97.5), alpha=0.4) +
  geom_point(aes(x=day, y=cumulative), shape=16, size=1) + 
  theme_classic() + ylab("Cumulative Weight (ODBA)") + xlab("Days to Arrival on Breeding Areas") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16, face="bold"), 
        panel.border=element_rect(color="black", fill=NA, size=0.5)) #+
  # geom_text(aes(x=0, y=1, label="C"), size=5)

g.odba <- ggplot(cw.grld.odba) + geom_segment(aes(x=1, y=0, xend=50, yend=1), size=1, color="black") + 
  # geom_hline(aes(yintercept=0.9), color="gray70") +
  geom_ribbon(aes(x=day, ymin=q2.5, ymax=q97.5), alpha=0.4) +
  geom_point(aes(x=day, y=cumulative), shape=16, size=1) + 
  theme_classic() + ylab("Cumulative Weight") + xlab("Days to Arrival on Breeding Areas") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16, face="bold"), 
        axis.title.y=element_blank(),axis.ticks.y=element_blank(), axis.text.y=element_blank(),
        panel.border=element_rect(color="black", fill=NA, size=0.5)) #+
  # geom_text(aes(x=0, y=1, label="D"), size=5)

# ggarrange(n.ptf, g.ptf, n.odba, g.odba, nrow=2, widths=c(2,2,2,2))
lay <- rbind(c(1,1,1,1,2,2,2),
             c(1,1,1,1,2,2,2),
             c(3,3,3,3,4,4,4),
             c(3,3,3,3,4,4,4))

grid.arrange(n.ptf, g.ptf, n.odba, g.odba, layout_matrix=lay)


## Correlation plot of ODBA vs PTF
dat <-read.csv("output/dlm_emm_data.csv", stringsAsFactors=FALSE)
dat <- dat[,-1]
dat <- mutate(dat, prop.graze=graze/total)
dat <- dat[dat$animal_id!="RP08F" & dat$animal_id!="RP15F" & dat$animal_id!="2164"
             & dat$animal_id!="2176" & dat$animal_id!="2160" & dat$animal_id!="2167",]

plot(dat$prop.graze, dat$odba)
abline(lm(odba~prop.graze, data=dat), col="red")
cor(dat$odba, dat$prop.graze)
cor.test(dat$odba, dat$prop.graze, method = "pearson")


ggplot(dat, aes(x=prop.graze, y=odba)) + geom_point(size=2, shape=16, alpha=0.5) + theme_classic() +
  xlab("Proportion of Time Feeding (PTF)") + ylab("Overall Dynamic Body Acceleration (ODBA)") +
  geom_smooth(method='lm',formula=y~x, color="red", size=1) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16, face="bold"), 
        panel.border=element_rect(color="black", fill=NA, size=0.5)) + 
  geom_text(aes(x=0.75, y=1.75, label=""))

m1 <- lm(odba ~ prop.graze, data=dat)
summary(m1)


#### Figure S2. Temporal Variation in weather conditions

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

dat <- dat[dat$animal_id!="RP08F" & dat$animal_id!="RP15F" & dat$animal_id!="2164"
             & dat$animal_id!="2176" & dat$animal_id!="2160" & dat$animal_id!="2167",]

mtemp <- aggregate(mintemp ~ julian + year + pop, data=dat, FUN=mean)
mtemp.q <- as.data.frame(aggregate(mintemp ~ julian + year + pop, data=dat, FUN=quantile, probs=c(0.025,0.975)))
mtemp <- cbind(mtemp, mtemp.q$mintemp)
names(mtemp)[4:6] <- c("mean", "lcl", "ucl")
mtemp$covar <- "mintemp"

prate <- aggregate(prate ~ julian + year + pop, data=dat, FUN=mean)
prate.q <- as.data.frame(aggregate(prate ~ julian + year + pop, data=dat, FUN=quantile, probs=c(0.025,0.975)))
prate <- cbind(prate, prate.q$prate)
names(prate)[4:6] <- c("mean", "lcl", "ucl")
prate$covar <- "prate"

B <- ggplot(mtemp, aes(x=julian)) + geom_line(aes(y=mean, color=pop), size=1) + 
  geom_ribbon(aes(ymin=lcl, ymax=ucl, color=pop, fill=pop), alpha=0.4) +
  theme_classic() +
  xlab("Julian Day") + ylab("Minimum Temperature (C)") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16, face="bold"),
        strip.text=element_blank(),
        legend.position = "none",
        panel.border=element_rect(color="black", fill=NA, size=0.5)) + 
  scale_color_manual(values=c("#2166ac", "#b2182b")) +
  scale_fill_manual(values=c("#2166ac", "#b2182b")) +
  labs(fill="Population", color="Population") +
  facet_grid(.~year)

A <- ggplot(prate, aes(x=julian)) + geom_line(aes(y=mean, color=pop), size=1) + 
  geom_ribbon(aes(ymin=lcl, ymax=ucl, color=pop, fill=pop), alpha=0.4) +
  theme_classic() +
  xlab("Julian Day") + ylab("Precipitation Rate (mm/hr)") +
  theme(axis.text.y=element_text(size=14), axis.title.y=element_text(size=16, face="bold"),
        axis.text.x=element_blank(), axis.title.x=element_blank(),
        strip.text=element_text(size=14, face="bold"),
        legend.title=element_text(size=16, face="bold"),
        legend.position=c(0.08,0.85), legend.text=element_text(size=14),
        panel.border=element_rect(color="black", fill=NA, size=0.5)) + 
  scale_color_manual(values=c("#2166ac", "#b2182b")) +
  scale_fill_manual(values=c("#2166ac", "#b2182b")) +
  labs(fill="Population", color="Population") +
  facet_grid(.~year)

gA <- ggplotGrob(A)
gB <- ggplotGrob(B)
maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
gA$widths[2:5] <- as.list(maxWidth)
gB$widths[2:5] <- as.list(maxWidth)
grid.arrange(gA, gB, ncol=1)


#### Daily weights ####

# Load priors
load("output/ecomem_NAMCodba3_priors.Rdata")  # out1
load("output/ecomem_NAMC3ptf_priors.Rdata")  # out2
load("output/ecomem_GRLDodba3_priors.Rdata")  # out3
load("output/ecomem_GRLD3ptf_priors.Rdata")  # out4

priors.g <- data.frame(index=seq(1,length(out3p$mean$weight),1), 
                       ODBA=out3p$mean$weight, ODBA.l=out3p$q2.5$weight, ODBA.u=out3p$q97.5$weight,
                       PTF=out4p$mean$weight, PTF.l=out4p$q2.5$weight, PTF.u=out4p$q97.5$weight,
                       pop=rep("GRLD", length(out3p$mean$weight)))

priors.n <- data.frame(index=seq(1,length(out1p$mean$weight),1), 
                   ODBA=out1p$mean$weight, ODBA.l=out1p$q2.5$weight, ODBA.u=out1p$q97.5$weight,
                   PTF=out2p$mean$weight, PTF.l=out2p$q2.5$weight, PTF.u=out2p$q97.5$weight,
                   pop=rep("NAMC", length(out1p$mean$weight)))


priors <- rbind(priors.g, priors.n)
op <- priors[,c(1:4,8)]
op$variable <- "ODBA"
names(op)[2:4] <- c("mean", "lcri", "ucri")
pp <- priors[,c(1,5:8)]
pp$variable <- "PTF"
names(pp)[2:4] <- c("mean", "lcri", "ucri")

priors <- rbind(op, pp)

# Posterior values
grld <- data.frame(index=seq(1,length(out3$mean$weight),1), 
                   ODBA=out3$mean$weight, ODBA.l=out3$q2.5$weight, ODBA.u=out3$q97.5$weight,
                   PTF=out4$mean$weight, PTF.l=out4$q2.5$weight, PTF.u=out4$q97.5$weight,
                   pop=rep("GRLD", length(out3$mean$weight)))

namc <- data.frame(index=seq(1,length(out1$mean$weight),1), 
                   ODBA=out1$mean$weight, ODBA.l=out1$q2.5$weight, ODBA.u=out1$q97.5$weight,
                   PTF=out2$mean$weight, PTF.l=out2$q2.5$weight, PTF.u=out2$q97.5$weight,
                   pop=rep("NAMC", length(out1$mean$weight)))

weights <- rbind(grld, namc)

ow <- weights[,c(1:4,8)]
ow$variable <- "ODBA"
names(ow)[2:4] <- c("mean", "lcri", "ucri")
pw <- weights[,c(1,5:8)]
pw$variable <- "PTF"
names(pw)[2:4] <- c("mean", "lcri", "ucri")

weights <- rbind(ow, pw)

my.formula <- y ~ x
meanw <- data.frame(mean=as.factor(c("ODBA","PTF","ODBA","PTF")), pop=as.factor(c("GRLD", "GRLD", "NAMC", "NAMC")), h=c(1/50, 1/50, 1/88, 1/88))

ggplot(weights, aes(x=index, y=mean)) + 
  geom_ribbon(data=priors, aes(x=index,ymin=lcri, ymax=ucri), fill="grey80") +
  geom_hline(data=meanw, aes(yintercept=h), col="red", size=1) +
  geom_line(data=priors, aes(x=index, y=mean)) +
  geom_point() +
  ylab("Weight") +
  xlab("Day of Migration") + 
  geom_segment(aes(x=index, xend=index, y=lcri, yend=ucri)) +
  # stat_summary(fun.data=mean_cl_normal) + geom_smooth(method='lm', formula=my.formula) +
  # stat_poly_eq(formula = my.formula, 
               # aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               # parse = TRUE) + 
  facet_grid(variable~pop, scales="free") + theme_classic() +
  theme(panel.border=element_rect(color="black", fill=NA, size=0.5),
        strip.text=element_text(size=14, face="bold"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14, face="bold"))


pnorm(mean=0, sd=0.01)

#### Antecedent values
datn <- read.csv("output/samdata_namc.csv")
datn <- datn[,-1]

datg <- read.csv("output/samdata_grld.csv")
datg <- datg[,-1]


namc <- data.frame(pop=rep("NAMC", 9), ind=1:9, attempt=datn$attempt, antPTF=out2$mean$antPTF, antODBA=out1$mean$antODBA)

namc <- gather(namc, key="variable", value="value", 4:5)

grld <- data.frame(pop=rep("GRLD", 22), ind=1:22, attempt=datg$attempt, antPTF=out4$mean$antPTF, antODBA=out3$mean$antODBA)
grld <- gather(grld, key="variable", value="value", 4:5)

n <-ggplot(namc, aes(x=ind, y=value)) + theme_classic() + 
  geom_point(aes(color=factor(attempt), shape=factor(attempt)), size=4) + 
  scale_color_manual(values=c("#b2182b", "#2166ac")) +
  scale_shape_manual(values=c("D","A")) + ylab("Midcontinent") + 
  theme(panel.border=element_rect(color="black", fill=NA, size=0.5),
        legend.justification=c(0,0), legend.position=c(0,0), legend.text=element_text(size=14), 
        legend.title=element_text(size=14, face="bold"), strip.text=element_text(size=14, face="bold"), 
        legend.background=element_rect(fill=NA, colour=NA)) + facet_grid(~variable)

g <- ggplot(grld, aes(x=ind, y=value)) + theme_classic() + 
  geom_point(aes(color=factor(attempt), shape=factor(attempt)), size=4) + 
  scale_color_manual(values=c("#b2182b", "#2166ac")) +
  scale_shape_manual(values=c("D","A")) + ylab("Greenland") + 
  theme(panel.border=element_rect(color="black", fill=NA, size=0.5),
        legend.justification=c(0,0), legend.position=c(0,0), legend.text=element_text(size=14), 
        legend.title=element_text(size=14, face="bold"), strip.text=element_text(size=14, face="bold"), 
        legend.background=element_rect(fill=NA, colour=NA)) + facet_grid(~variable)

lay <- rbind(c(1,1),
             c(2,2))

grid.arrange(n, g, layout_matrix=lay)
