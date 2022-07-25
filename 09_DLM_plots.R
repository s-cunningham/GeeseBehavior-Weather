
library(tidyverse)


# Plotting dynamic linear models
# 6 August 2019

dat1 <- read.csv("output/dlm_emm_data.csv", stringsAsFactors=FALSE)
dat1 <- dat1[,-c(1,19:22,25,26)]
dat1$date <- as.Date(dat1$date)

wd <- read.csv("output/weather_covars.csv", stringsAsFactors=FALSE)
wd <- wd[,-1]
wd$date <- as.Date(wd$date)

dat <- left_join(dat1, wd, by=c("animal_id", "date"))

dat2 <- read.csv("output/modelcoefficients/PTF_prcp-mintemp-press.csv", stringsAsFactors=FALSE)
dat2 <- dat2[,-1]
dat2$date <- as.Date(dat2$date)

dat <- left_join(dat, dat2, by=c("animal_id", "date"))
dat <- dat[,-c(32:37)]
names(dat)[c(5,14,16)] <- c("julian","total","graze")

# remove(dat1, dat2)

ggplot(dat, aes(x=julian, y=beta1, group=animal_id, color=factor(mig))) + coord_cartesian(ylim=c(-1,4)) +
  geom_hline(yintercept=0, color="gray40") + 
  geom_segment(aes(x=julian, xend=julian, y=b1lower, yend=b1upper, color=factor(mig))) + 
  geom_point() + theme_bw() + facet_wrap(~animal_id, ncol=5) + ylab("Effect of Precipitation Rate") + xlab("Julian Day") +
  scale_color_manual(values=c("black", "red"), name="Migration Day?", breaks=c(0,1), 
                     labels=c("No", "Yes"))  +
  theme(legend.justification=c(1,0), legend.position=c(1,0), 
        legend.title=element_text(face="bold"), legend.background=element_rect(fill=NA))

ggplot(dat, aes(x=rel.day, y=beta1, group=animal_id)) + coord_cartesian(ylim=c(-3,3)) +
  geom_vline(xintercept=14, color="red") +
  geom_hline(yintercept=0, color="gray40") + 
  geom_segment(aes(x=rel.day, xend=rel.day, y=b1lower, yend=b1upper, color=pop), size=1, alpha=0.3) +
  geom_point(aes(color=pop), size=1.5) + theme_classic() + facet_grid(.~year) + ylab("Effect of Precipitation Rate") + 
  xlab("Day to End of Migration") +
  scale_color_manual(values=c( "#984ea3", "#4daf4a", "#e41a1c", "#377eb8"), name="Population") +
  theme(legend.justification=c(0,1), legend.position=c(0,1), 
        legend.title=element_text(face="bold", size=16), legend.background=element_rect(fill=NA),
        legend.text=element_text(size=14), strip.text=element_text(size=14),
        axis.text=element_text(size=14), axis.title=element_text(size=16, face="bold"), 
        panel.border=element_rect(color="black", fill=NA, size=0.5))

dat <- dat[dat$year==2018,]
ggplot(dat, aes(x=rev.rel.day, y=beta1, group=animal_id, color=factor(attempt))) + coord_cartesian(ylim=c(-4,4)) +
  # geom_vline(xintercept=c(54, 101), color="black", linetype=2, size=1) +
  # geom_hline(yintercept=0, color="gray40") + 
  geom_segment(aes(x=rev.rel.day, xend=rev.rel.day, y=b1lower, yend=b1upper), size=1, alpha=0.3) +
  geom_line(size=1.5, alpha=0.7) + 
  theme_classic() + facet_grid(.~pop) + 
  ylab("Effect of Precipitation Rate") + 
  xlab("Day to End of Migration") +
  scale_color_manual(values=c("#f1a340", "#998ec3"), name="Repro. Status", labels=c("Defer", "Attempt")) +
  theme(legend.position="none",
        legend.title=element_blank(), legend.background=element_rect(fill=NA),
        legend.text=element_text(size=16), strip.text=element_text(size=18, face="bold"),
        axis.text=element_text(size=16), axis.title=element_text(size=18, face="bold"), 
        panel.border=element_rect(color="black", fill=NA, size=0.5))


# Minimum temperature
ggplot(dat, aes(x=rel.day, y=beta2, group=animal_id)) + coord_cartesian(ylim=c(-3,2)) +
  geom_hline(yintercept=0, color="gray40") + 
  geom_segment(aes(x=rel.day, xend=rel.day, y=b2lower, yend=b2upper, color=factor(year)), size=1, alpha=0.3) +
  geom_point(aes(color=factor(year)), size=1.5) + theme_classic() + facet_grid(pop ~ .) + ylab("Effect of Minimum Temperature") + 
  xlab("Day to End of Migration") +
  # scale_color_manual(values=c("black", "red"), name="Migration Day?", breaks=c(0,1), 
                     # labels=c("No", "Yes"))  +
  scale_color_manual(values=c( "#984ea3", "#4daf4a", "#e41a1c", "#377eb8"), name="Year") +
  theme(legend.justification=c(1,1), legend.position=c(1,1), 
        legend.title=element_text(face="bold", size=16), legend.background=element_rect(fill=NA),
        legend.text=element_text(size=14), strip.text=element_text(size=14),
        axis.text=element_text(size=14), axis.title=element_text(size=16, face="bold"), 
        panel.border=element_rect(color="black", fill=NA, size=0.5))


ggplot(dat, aes(x=rel.day, y=beta2, group=animal_id)) + coord_cartesian(ylim=c(-3,2)) +
  geom_vline(xintercept=14, color="red") +
  geom_hline(yintercept=0, color="gray40") + 
  geom_segment(aes(x=rel.day, xend=rel.day, y=b2lower, yend=b2upper, color=pop), size=1, alpha=0.3) +
  geom_point(aes(color=pop), size=1.5) + theme_classic() + facet_grid(.~year) + ylab("Effect of Minimum Temperature") + 
  xlab("Day to End of Migration") +
  scale_color_manual(values=c( "#984ea3", "#4daf4a", "#e41a1c", "#377eb8"), name="Population") +
  # scale_shape_manual(name="Reproductive\nStatus", labels=c("Defer", "Attempt"), values=c(16,17)) +
  theme(legend.justification=c(1,1), legend.position=c(1,1), 
        legend.title=element_text(face="bold", size=16), legend.background=element_rect(fill=NA),
        legend.text=element_text(size=14), strip.text=element_text(size=14),
        axis.text=element_text(size=14), axis.title=element_text(size=16, face="bold"), 
        panel.border=element_rect(color="black", fill=NA, size=0.5))

# dat <- dat[dat$year==2018,]
ggplot(dat, aes(x=rev.rel.day, y=beta2, group=animal_id)) + coord_cartesian(ylim=c(-4,4)) +
  geom_vline(xintercept=c(54, 101), color="black", linetype=2, size=1) +
  geom_hline(yintercept=0, color="gray40") + 
  geom_segment(aes(x=rev.rel.day, xend=rev.rel.day, y=b2lower, yend=b2upper, color=pop), size=1, alpha=0.3) +
  geom_line(aes(color=pop), size=1.5) + theme_classic() + facet_grid(.~pop) + ylab("Effect of Minimum Temperature") + 
  xlab("Day to End of Migration") +
  scale_color_manual(values=c("#ca0020", "#0571b0"), name="Population") +
  theme(legend.position="none", 
        legend.title=element_blank(), legend.background=element_rect(fill=NA),
        legend.text=element_text(size=16), strip.text=element_text(size=18, face="bold"),
        axis.text=element_text(size=16), axis.title=element_text(size=18, face="bold"), 
        panel.border=element_rect(color="black", fill=NA, size=0.5))



# pressure
ggplot(dat2, aes(x=julian, y=beta3, group=animal_id, color=factor(mig))) + coord_cartesian(ylim=c(-1,1)) +
  geom_hline(yintercept=0, color="gray40") + 
  geom_segment(aes(x=julian, xend=julian, y=b3lower, yend=b3upper, color=factor(mig))) + 
  geom_point() + theme_bw() + facet_wrap(~animal_id, ncol=5) + ylab("Effect of Pressure") + xlab("Julian Day") +
  scale_color_manual(values=c("black", "red"), name="Migration Day?", breaks=c(0,1), 
                     labels=c("No", "Yes"))  +
  theme(legend.justification=c(1,0), legend.position=c(1,0), 
        legend.title=element_text(face="bold"), legend.background=element_rect(fill=NA))

dat <- dat[dat$year==2018,]
ggplot(dat, aes(x=rev.rel.day, y=beta3, group=animal_id)) + coord_cartesian(ylim=c(-4,4)) +
  geom_vline(xintercept=c(54, 101), color="black", linetype=2, size=1) +
  geom_hline(yintercept=0, color="gray40") + 
  geom_segment(aes(x=rev.rel.day, xend=rev.rel.day, y=b3lower, yend=b3upper, color=pop), size=1, alpha=0.3) +
  geom_line(aes(color=pop), size=1.5) + theme_classic() + facet_grid(.~pop) + ylab("Effect of Pressure") + 
  xlab("Day to End of Migration") +
  scale_color_manual(values=c("#ca0020", "#0571b0"), name="Population") +
  theme(legend.position="none", 
        legend.title=element_blank(), legend.background=element_rect(fill=NA),
        legend.text=element_text(size=16), strip.text=element_text(size=18, face="bold"),
        axis.text=element_text(size=16), axis.title=element_text(size=18, face="bold"), 
        panel.border=element_rect(color="black", fill=NA, size=0.5))

aggregate(rev.rel.day ~ pop, data=dat, FUN=max)


# Subset to 1 for presentation plot
dat1 <- dat[dat$animal_id==17777,]

dat1 <- mutate(dat1, prop.graze=graze/total)


ggplot(dat1, aes(x=rev.rel.day)) + 
  geom_line(aes(y=prop.graze), color="darkgreen")








