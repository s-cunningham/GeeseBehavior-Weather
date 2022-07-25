
library(tidyverse)
library(gridExtra)
library(ggpmisc)
library(boot)
library(DMwR)
library(facetscales)

rm(list=ls())

# Load JAGS objects
load("output/ecomem_NAMCodba4.Rdata")  # out1
# print(out1, digits=3)
load("output/ecomem_NAMC4ptf.Rdata")  # out2
# print(out2, digits=3)
load("output/ecomem_GRLDodba4.Rdata")  # out3
# print(out3, digits=3)
load("output/ecomem_GRLD4ptf.Rdata")  # out4
# print(out4, digits=3)

#### Plot cumulative weights ####

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


#### Daily weights ####
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
  geom_hline(data=meanw, aes(yintercept=h), col="red", size=1) +
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




#### Violin Plots ####
#### Violin plots
data_summary <- function(x) {
  m <- mean(x)
  mq <- quantile(x, probs=c(0.025, 0.975))
  ymin <- as.numeric(mq[1])
  ymax <- as.numeric(mq[2])
  return(c(y=m, ymin=ymin, ymax=ymax))
}

df.out1 <- data.frame(beta01=out1$sims.list$beta0[,1], beta02=out1$sims.list$beta0[,2], beta1=out1$sims.list$beta1)
df.out1 <- gather(df.out1, key=parameter, value=value, 1:3)
df.out1$parameter[df.out1$parameter=="beta01"] <- "2018 Intercept"
df.out1$parameter[df.out1$parameter=="beta02"] <- "2017 Intercept"
df.out1$parameter[df.out1$parameter=="beta1"] <- "Antecedent\nEffect"
df.out1$parameter <- factor(df.out1$parameter, levels=c("2018 Intercept", "2017 Intercept", "Antecedent\nEffect"))

na.odba <- ggplot(df.out1, aes(x=parameter, y=value)) + 
  geom_hline(yintercept=0, color="red") + 
  geom_violin(fill="gray80", size=1, color="gray50") + 
  coord_cartesian(ylim=c(-40,40)) + 
  theme_classic() + ylab("Posterior Distribution") + 
  stat_summary(fun.data=data_summary, size=0.75) +
  theme(axis.text.x=element_text(size=14), axis.text.y=element_blank(),
        axis.title=element_blank(),
        panel.border=element_rect(color="black", fill=NA, size=0.5))

df.out2 <- data.frame(beta01=out2$sims.list$beta0[,1], beta02=out2$sims.list$beta0[,2], beta1=out2$sims.list$beta1)
df.out2 <- gather(df.out2, key=parameter, value=value, 1:3)
df.out2$parameter[df.out2$parameter=="beta01"] <- "2018 Intercept"
df.out2$parameter[df.out2$parameter=="beta02"] <- "2017 Intercept"
df.out2$parameter[df.out2$parameter=="beta1"] <- "Antecedent\nEffect"
df.out2$parameter <- factor(df.out2$parameter, levels=c("2018 Intercept", "2017 Intercept", "Antecedent\nEffect"))

na.ptf <- ggplot(df.out2, aes(x=parameter, y=value)) + 
  geom_hline(yintercept=0, color="red") + 
  geom_violin(fill="gray80", size=1, color="gray50") + 
  coord_cartesian(ylim=c(-40,40)) + ggtitle("Midcontinent") +
  theme_classic() + ylab("Posterior Distribution") + 
  stat_summary(fun.data=data_summary, size=0.75) +
  theme(axis.text=element_blank(), 
        axis.title=element_blank(),plot.title=element_text(size=16, face="bold", hjust=0.5),
        panel.border=element_rect(color="black", fill=NA, size=0.5))

df.out3 <- data.frame(beta01=out3$sims.list$beta0[,1], beta02=out3$sims.list$beta0[,2],beta03=out3$sims.list$beta0[,3], beta1=out3$sims.list$beta1)
df.out3 <- gather(df.out3, key=parameter, value=value, 1:4)
df.out3$parameter[df.out3$parameter=="beta01"] <- "2018 Intercept"
df.out3$parameter[df.out3$parameter=="beta02"] <- "2013 Intercept"
df.out3$parameter[df.out3$parameter=="beta03"] <- "2012 Intercept"
df.out3$parameter[df.out3$parameter=="beta1"] <- "Antecedent\nEffect"
df.out3$parameter <- factor(df.out3$parameter, levels=c("2018 Intercept","2013 Intercept","2012 Intercept","Antecedent\nEffect"))

gr.odba <- ggplot(df.out3, aes(x=parameter, y=value)) + 
  geom_hline(yintercept=0, color="red") + 
  geom_violin(fill="gray80", size=1, color="gray50") + 
  theme_classic() + ylab("Posterior Distribution (ODBA)") + 
  coord_cartesian(ylim=c(-40,40)) + 
  stat_summary(fun.data=data_summary, size=0.75) +
  theme(axis.text=element_text(size=14), axis.title.y=element_text(size=16, face="bold"), 
        axis.title.x=element_blank(),
        panel.border=element_rect(color="black", fill=NA, size=0.5))

df.out4 <- data.frame(beta01=out4$sims.list$beta0[,1], beta02=out4$sims.list$beta0[,2],beta03=out4$sims.list$beta0[,3], beta1=out4$sims.list$beta1)
df.out4 <- gather(df.out4, key=parameter, value=value, 1:4)
df.out4$parameter[df.out4$parameter=="beta01"] <- "2018 Intercept"
df.out4$parameter[df.out4$parameter=="beta02"] <- "2013 Intercept"
df.out4$parameter[df.out4$parameter=="beta03"] <- "2012 Intercept"
df.out4$parameter[df.out4$parameter=="beta1"] <- "Antecedent\nEffect"
df.out4$parameter <- factor(df.out4$parameter, levels=c("2018 Intercept","2013 Intercept","2012 Intercept","Antecedent\nEffect"))

gr.ptf <- ggplot(df.out4, aes(x=parameter, y=value)) + 
  geom_hline(yintercept=0, color="red") + 
  geom_violin(fill="gray80", size=1, color="gray50") + 
  theme_classic() + ylab("Posterior Distribution (PTF)") + 
  coord_cartesian(ylim=c(-40,40)) + ggtitle("Greenland") +
  stat_summary(fun.data=data_summary, size=0.75) +
  theme(axis.text=element_text(size=14), axis.title.y=element_text(size=16, face="bold"), 
        axis.title.x=element_blank(), axis.text.x=element_blank(),
        plot.title=element_text(size=16, face="bold", hjust=0.5),
        panel.border=element_rect(color="black", fill=NA, size=0.5))

lay <- rbind(c(1,1,1,2,2),
             c(1,1,1,2,2),
             c(3,3,3,4,4),
             c(3,3,3,4,4))

grid.arrange(gr.ptf,na.ptf,gr.odba, na.odba, layout_matrix=lay)


#### Response Curves ####

## NAMC ODBA
nmcmc <- out1$mcmc.info$n.samples

# Set up samples
beta01 <- c(out1$samples[[1]][,1], out1$samples[[2]][,1], out1$samples[[3]][,1])
beta02 <- c(out1$samples[[1]][,2], out1$samples[[2]][,2], out1$samples[[3]][,2])
beta0 <- apply(cbind(beta01, beta02), 1, median)
beta1 <- c(out1$samples[[1]][,3], out1$samples[[2]][,3], out1$samples[[3]][,3])

# set up predictions
pred_length <- 100
antODBA_pred <- seq(-2.94,3.56,length.out=pred_length)

mc.odba <- matrix(, nmcmc, pred_length)
for (j in 1:pred_length) {
  mc.odba[,j] <- inv.logit(beta0 + beta1*antODBA_pred[j])
}

mc.odba.qt <- apply(mc.odba, 2, quantile, probs=c(.5, .025, .975))

# unscale data
antODBA <- unscale(antODBA_pred, mcODBA)

# Set up data frame
df2 <- data.frame(y=mc.odba[1,], x.s=antODBA_pred, x=exp(antODBA), up1=mc.odba.qt[2,], lo1=mc.odba.qt[3,], pop="NAMC", var="ODBA")

ggplot(df2, aes(x=x, y=y)) + geom_ribbon(aes(x=x, ymin=lo1, ymax=up1), alpha=0.4, fill="red") + 
  geom_line(color="red", size=1)  +
  theme_classic() + ylab("Predicted Probability of Breeding Attempt") + xlab("Antecedent ODBA") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16, face="bold"),
        panel.border=element_rect(color="black", fill=NA, size=0.5))

## NAMC PTF
nmcmc <- out2$mcmc.info$n.samples

# Set up samples
beta01 <- c(out2$samples[[1]][,1], out2$samples[[2]][,1], out2$samples[[3]][,1])
beta02 <- c(out2$samples[[1]][,2], out2$samples[[2]][,2], out2$samples[[3]][,2])
beta0 <- apply(cbind(beta01, beta02), 1, median)
beta1 <- c(out2$samples[[1]][,3], out2$samples[[2]][,3], out2$samples[[3]][,3])

# set up predictions
pred_length <- 100
antPTF_pred <- seq(-3.39,2.71,length.out=pred_length)

mc.ptf <- matrix(, nmcmc, pred_length)
for (j in 1:pred_length) {
  mc.ptf[,j] <- inv.logit(beta0 + beta1*antPTF_pred[j])
}

mc.ptf.qt <- apply(mc.ptf, 2, quantile, probs=c(.5, .025, .975))

# unscale data
antPTF <- unscale(antPTF_pred, mcPTF)

# Set up data frame
df1 <- data.frame(y=mc.ptf[1,], x.s=antPTF_pred, x=antPTF, up1=mc.ptf.qt[2,], lo1=mc.ptf.qt[3,], pop="NAMC", var="PTF")

ggplot(df1, aes(x=x, y=y)) + geom_ribbon(aes(x=x, ymin=lo1, ymax=up1), alpha=0.4, fill="red") + 
  geom_line(color="red", size=1)  +
  theme_classic() + ylab("Predicted Probability of Breeding Attempt") + xlab("Antecedent Proportion of Time Feeding") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16, face="bold"),
        panel.border=element_rect(color="black", fill=NA, size=0.5))

## GRLD ODBA

beta01 <- c(out1$samples[[1]][,1], out1$samples[[2]][,1], out1$samples[[3]][,1])
beta02 <- c(out1$samples[[1]][,2], out1$samples[[2]][,2], out1$samples[[3]][,2])
beta03 <- c(out1$samples[[1]][,3], out1$samples[[2]][,3], out1$samples[[3]][,3])

#### Antecedent variable plots ####

datn <- read.csv("output/samdata_namc.csv")
datn <- datn[,-1]

datg <- read.csv("output/samdata_grld.csv")
datg <- datg[,-1]

ids <- read.csv("output/corresponding_ids.csv")
ids <- ids[,-1]

dat <- rbind(datn, datg)
dat <- left_join(dat, ids[,1:3], by="animal_id")

namc <- data.frame(pop=rep("NAMC", 9), birdno=c(35,34,33,29,37,36,28,27,30), 
                   attempt=datn$attempt, antPTF=out2$mean$antPTF, ptfLCRI=out2$q2.5$antPTF, ptfUCRI=out2$q97.5$antPTF,
                   antODBA=out1$mean$antODBA, odbaLCRI=out1$q2.5$antODBA, odbaUCRI=out1$q97.5$antODBA)
namc$antPTF <- unscale(namc$antPTF, mcPTF)
namc$ptfLCRI <- unscale(namc$ptfLCRI, mcPTF)
namc$ptfUCRI <- unscale(namc$ptfUCRI, mcPTF)
namc$antODBA <- unscale(namc$antODBA, mcODBA)
namc$odbaLCRI <- unscale(namc$odbaLCRI, mcODBA)
namc$odbaUCRI <- unscale(namc$odbaUCRI, mcODBA)

mcptf <- namc[,1:6]
mcptf$variable <- "PTF"
names(mcptf)[4:6] <- c("mean", "LCRI", "UCRI")

mcodba <- namc[,c(1,2,3,7:9)]
mcodba$variable <- "ODBA"
names(mcodba)[4:6] <- c("mean", "LCRI", "UCRI")

namc <- rbind(mcptf, mcodba)

grld <- data.frame(pop=rep("GRLD", 22), birdno=c(4,6,7,8,10,11,12,2,3,5,9,25,26,1,17,14,22,23,24,19,20,21), 
                   attempt=datg$attempt, antPTF=out4$mean$antPTF, ptfLCRI=out4$q2.5$antPTF, ptfUCRI=out4$q97.5$antPTF,
                   antODBA=out3$mean$antODBA, odbaLCRI=out3$q2.5$antODBA, odbaUCRI=out3$q97.5$antODBA)
grld$antPTF <- unscale(grld$antPTF, grPTF)
grld$ptfLCRI <- unscale(grld$ptfLCRI, grPTF)
grld$ptfUCRI <- unscale(grld$ptfUCRI, grPTF)
grld$antODBA <- unscale(grld$antODBA, grODBA)
grld$odbaLCRI <- unscale(grld$odbaLCRI, grODBA)
grld$odbaUCRI <- unscale(grld$odbaUCRI, grODBA)

grptf <- grld[,1:6]
grptf$variable <- "PTF"
names(grptf)[4:6] <- c("mean", "LCRI", "UCRI")

grodba <- grld[,c(1,2,3,7:9)]
grodba$variable <- "ODBA"
names(grodba)[4:6] <- c("mean", "LCRI", "UCRI")

grld <- rbind(grptf, grodba)

all <- rbind(namc, grld)
all <- left_join(all, dat[,c(1,3,7:8)], by="birdno")

all$attempt <- ifelse(all$attempt==0, "Defer", "Attempt")
all$pop <- ifelse(all$pop=="GRLD", "Greenland", "Midcontinent")

means <- as.data.frame(all %>% group_by(pop, variable, attempt) %>% summarize(mean(mean)))
names(means)[4] <- "mean.val"
means$xstart <- c(12,1,12,1,27,23,27,23)
means$xend <- c(22,11,22,11,31,26,31,26)

scales_y <- list(
  'ODBA' = scale_y_continuous(limits=c(-1.38,-0.18), breaks=seq(-1.38,-0.18,length=5)),
  'PTF' = scale_y_continuous(limits=c(0.35,0.75), breaks=seq(0.35,0.75,0.1))
)

scales_x <- list(
  'Greenland' = scale_x_continuous(limits=c(1,22), breaks=seq(1,22,1)),
  'Midcontinent' = scale_x_continuous(limits=c(23,31), breaks=seq(22,31,1))
)

ggplot(all) + theme_classic() + 
  geom_segment(data=means, aes(y=mean.val, yend=mean.val, x=xstart, xend=xend), color="gray70", size=1) +
  geom_point(aes(x=plot_id, y=mean, color=factor(attempt), shape=factor(attempt)), size=4) + 
  geom_errorbar(aes(x=plot_id, ymin=LCRI, ymax=UCRI, color=attempt), width=.2) +
  facet_grid_sc(variable~pop, scales=list(y=scales_y, x=scales_x), space="free_x") +
  scale_x_continuous(breaks=seq(1,31,1)) +
  xlab("Individual ID") + ylab("Antecedent Value") +
  theme(axis.text.y=element_text(size=14), axis.title=element_text(size=16, face="bold"),
        axis.text.x=element_text(size=10),
        panel.border=element_rect(color="black", fill=NA, size=0.5),
        # legend.justification=c(1,1), legend.position=c(1,1), 
        legend.position="bottom",
        legend.text=element_text(size=14), 
        legend.title=element_text(size=14, face="bold"), strip.text=element_text(size=14, face="bold"), 
        legend.background=element_rect(fill=NA, colour=NA)) +
  scale_color_manual(values=c("#2166ac", "#b2182b")) + 
  guides(color=guide_legend(title="Breeding Status"), shape=guide_legend(title="Breeding Status"))
  
  
