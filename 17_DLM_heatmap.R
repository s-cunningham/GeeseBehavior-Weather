library(tidyverse)
library(gridExtra)
library(pracma)

options(scipen=999, digits=3)

#### ODBA ####
# Read in calculated proportions
b1.odba <- read.csv("output/ODBA_prate_proportions.csv")
b1.odba <- b1.odba[,-1]

b2.odba <- read.csv("output/ODBA_mintemp_proportions.csv")
b2.odba <- b2.odba[,-1]

# read data for bird ID numbers
bird <- read.csv("output/dlm_emm_data.csv", stringsAsFactors=FALSE)
bird <- bird[,-c(1,19:22,25,26)]
bird$date <- as.Date(bird$date)

# add IDs 
ids <- unique(bird$animal_id)
colnames(b1.odba) <- ids
colnames(b2.odba) <- ids

b1.odba <- gather(b1.odba, key="animal_id", value="PRATE.prop.g0", 1:37)
b2.odba <- gather(b2.odba, key="animal_id", value="MINTE.prop.g0", 1:37)
odba <- data.frame(animal_id=b1.odba$animal_id, PRATE.prop=b1.odba$PRATE.prop.g0, MINTE.prop=b2.odba$MINTE.prop.g0)
odba <- odba[complete.cases(odba),]
odba$rev.rel.day <- NA

un.id <- unique(odba$animal_id)
for (i in 1:length(un.id)) {
  odba[odba$animal_id==un.id[i],4] <- seq(1:nrow(odba[odba$animal_id==un.id[i],]))
}

# remove birds with not enough data
bird <- bird[bird$animal_id!="RP08F" & bird$animal_id!="RP15F" & bird$animal_id!="2164"
             & bird$animal_id!="2176" & bird$animal_id!="2160" & bird$animal_id!="2167",]

# Assign new numbers to reflect removal of shortened winters, and to make pop/reproductive status sequential
bird$pop2 <- 1
bird$pop2[bird$pop=="NAMC"] <- 2
N.order <- order(bird$year, decreasing = FALSE)
bird <- bird[N.order,]
N.order <- order(bird$attempt, decreasing=FALSE)
bird <- bird[N.order,]
N.order <- order(bird$pop2, decreasing = FALSE)
bird <- bird[N.order,]
id <- unique(bird$animal_id)
plot_id <- numeric(dim(bird)[1])
for (i in 1:dim(bird)[1]) {
  plot_id[i] <- which(id == bird$animal_id[i])
}
bird <- data.frame(bird, plot_id)
bird <- bird[,-20]

ids <- as.data.frame(unique(cbind(bird$id_ind, bird$animal_id, bird$plot_id, bird$year, bird$attempt)))
names(ids) <- c("id_ind", "animal_id", "plot_id", "year", "attempt")
# write.csv(ids, "output/corresponding_ids.csv")

# Join to posterior summary
dat <- left_join(bird, odba, by=c("animal_id", "rev.rel.day"))
dat <- dat[!is.na(dat$animal_id),]

# Add negative to ones with mean below 0
dat$attempt <- factor(dat$attempt, levels=c("0", "1"))

# Subset by covariate
odba.na <- dat[dat$pop=="NAMC",]
odba.gr <- dat[dat$pop=="GRLD",]

names(dat)[21:22] <- c("PRATE", "TEMP")

dat <- gather(dat, 21:22, key="variable", value="value")

dat$variable <- ifelse(dat$variable=="TEMP", "Temperature", "Precipitation Rate")
dat$pop <- ifelse(dat$pop=="GRLD", "Greenland", "North America") 
dat$attempt <- ifelse(dat$attempt==1, "Attempt", "Defer")

ggplot(dat, aes(x=value, group=animal_id, color=attempt, fill=attempt)) + 
  geom_density(size=0.6, alpha=0.1) + 
  facet_grid(variable~pop) + 
  theme_classic() + ylab("Density") + xlab("Proportion of Posterior Samples") + 
  scale_color_manual(values=c("#2166ac", "#b2182b")) + 
  scale_fill_manual(values=c("#2166ac", "#b2182b")) + 
  theme(legend.justification=c(1,1.05),legend.title=element_text(size=16, face="bold"), 
        legend.position=c(1,1), legend.text=element_text(size=14), 
        panel.border=element_rect(color="black", fill=NA, size=0.5),
        axis.text=element_text(size=14), axis.title=element_text(size=16, face="bold"),
        strip.text=element_text(size=14, face="bold"),
        plot.title=element_text(size=16, face="bold", hjust=0.5)) +
  guides(fill=guide_legend(title="Reproductive\nStatus"), 
         color=guide_legend(title="Reproductive\nStatus")) 

# Plot 
repro_list <- list('0'="Defer", '1'="Attempt")  
repro_labeller <- function(variable,value) {
  return(repro_list[value])
}

na.prate.odba <- ggplot(odba.na, aes(julian, as.factor(plot_id))) + geom_tile(aes(fill=PRATE.prop), colour = "black") + 
  scale_fill_gradient2(midpoint=0.5, low="#2166ac", mid="white",high="#b2182b", space ="Lab", limits=c(0,1)) +
  # scale_fill_gradient2(midpoint=0, low="#053061", mid="#f7f7f7",high="#67001f", space ="Lab") + 
  coord_cartesian(xlim=c(35,155)) +
  theme_classic() + xlab("Julian Day") + ylab("Individual ID") + #ggtitle("Precipitation Rate (mm/hr)") +
  guides(fill = guide_colourbar(title="Posterior\nProportion")) +
  theme(legend.position = 'none',
        # legend.justification=c(1,1.05),legend.title=element_text(size=16, face="bold"), 
        # legend.position=c(1,1), legend.text=element_text(size=14), 
        panel.border=element_rect(color="black", fill=NA, size=0.5),
        axis.text=element_text(size=14), axis.title=element_text(size=16, face="bold"),
        strip.text.y=element_text(size=14, face="bold"),
        plot.title=element_text(size=16, face="bold", hjust=0.5)) +
  facet_grid(attempt~., space="free_y", scales="free_y", labeller=repro_labeller)  

na.mtemp.odba <- ggplot(odba.na, aes(julian, as.factor(plot_id))) + geom_tile(aes(fill=MINTE.prop), colour = "black") + 
  scale_fill_gradient2(midpoint=0.5, low="#2166ac", mid="white",high="#b2182b", space ="Lab", limits=c(-1,1)) +
  # scale_fill_gradient2(midpoint=0, low="#053061", mid="#f7f7f7",high="#67001f", space ="Lab") + 
  coord_cartesian(xlim=c(35,175)) +
  theme_classic() + xlab("Julian Day") + ylab("Individual ID") + # ggtitle("Minimum Temperature (C)") +
  guides(fill = guide_colourbar(title="Posterior\nProbability")) +
  theme(legend.position="none",
        # legend.justification=c(1,1.05),legend.title=element_text(size=16, face="bold"), 
        # legend.position=c(1,1), legend.text=element_text(size=14), 
        panel.border=element_rect(color="black", fill=NA, size=0.5),
        axis.text.x=element_text(size=14), axis.title.x=element_text(size=16, face="bold"),
        axis.text.y=element_blank(), axis.title.y=element_blank(),
        strip.text.y=element_text(size=14, face="bold"),
        plot.title=element_text(size=16, face="bold", hjust=0.5)) +
  facet_grid(attempt~., space="free_y", scales="free_y", labeller=repro_labeller)  

# grid.arrange(na.prate.odba, na.mtemp.odba, ncol=2)

gr.prate.odba <- ggplot(odba.gr, aes(julian, as.factor(plot_id))) + geom_tile(aes(fill=PRATE.prop), colour = "black") + 
  scale_fill_gradient2(midpoint=0.5, low="#2166ac", mid="white",high="#b2182b", space ="Lab", limits=c(0,1)) +
  # scale_fill_gradient2(midpoint=0, low="#053061", mid="#f7f7f7",high="#67001f", space ="Lab") + 
  coord_cartesian(xlim=c(75,150)) +
  theme_classic() + xlab("Julian Day") + ylab("Individual ID") + ggtitle("Precipitation Rate (mm/hr)") +
  guides(fill = guide_colourbar(title="Posterior\nProbability")) +
  theme(legend.position = 'none',
        # legend.justification=c(1,1.05),legend.title=element_text(size=16, face="bold"), 
        # legend.position=c(1,1), legend.text=element_text(size=14), 
        panel.border=element_rect(color="black", fill=NA, size=0.5),
        axis.text.y=element_text(size=14), axis.title.y=element_text(size=16, face="bold"),
        axis.text.x=element_text(size=14), axis.title.x=element_blank(),
        strip.text.y=element_text(size=14, face="bold"),
        plot.title=element_text(size=16, face="bold", hjust=0.5)) +
  facet_grid(attempt~., space="free_y", scales="free_y", labeller=repro_labeller)  

gr.mtemp.odba <- ggplot(odba.gr, aes(julian, as.factor(plot_id))) + geom_tile(aes(fill=MINTE.prop), colour = "black") + 
  scale_fill_gradient2(midpoint=0.5, low="#2166ac", mid="white",high="#b2182b", space ="Lab", limits=c(0,1)) +
  # scale_fill_gradient2(midpoint=0, low="#053061", mid="#f7f7f7",high="#67001f", space ="Lab") + 
  coord_cartesian(xlim=c(75,160)) +
  theme_classic() + xlab("Julian Day") + ylab("Individual ID") + ggtitle("Minimum Temperature (C)") +
  guides(fill = guide_colourbar(title="Proportion\nSamples > 0")) +
  theme(legend.justification=c(1,1),legend.title=element_text(size=16, face="bold"), 
        legend.position=c(1,1), legend.text=element_text(size=14), 
        legend.background=element_rect(fill=NA),
        panel.border=element_rect(color="black", fill=NA, size=0.5),
        axis.text.x=element_text(size=14), axis.title=element_blank(),
        axis.text.y=element_blank(), 
        strip.text.y=element_text(size=14, face="bold"),
        plot.title=element_text(size=16, face="bold", hjust=0.5)) +
  facet_grid(attempt~., space="free_y", scales="free_y", labeller=repro_labeller)  
  
# grid.arrange(gr.prate.odba, gr.mtemp.odba, ncol=2)

lay <- rbind(c(1,1,1,2,2,2),
             c(1,1,1,2,2,2),
             c(1,1,1,2,2,2),
             c(1,1,1,2,2,2),
             c(1,1,1,2,2,2),
             c(1,1,1,2,2,2),
             c(3,3,3,4,4,4),
             c(3,3,3,4,4,4),
             c(3,3,3,4,4,4),
             c(3,3,3,4,4,4))
grid.arrange(gr.prate.odba, gr.mtemp.odba, na.prate.odba, na.mtemp.odba, layout_matrix=lay)


# Calculate geometric mean proportion of posterior samples greater than 0
gm.PRATE <- numeric(31)
gm.MINTE <- numeric(31)
un.id <- unique(dat$animal_id)
for (i in 1:length(un.id)) {
  temp <- dat[dat$animal_id==un.id[i],]
  
  gm.PRATE[i] <- nthroot(prod(temp$PRATE.prop), nrow(temp))
  gm.MINTE[i] <- nthroot(prod(temp$MINTE.prop), nrow(temp))
  
  # temp$PRATE.prop[temp$PRATE.prop==0] <- 0.0000000000000000000000000000000000001
  # logPR <- log(temp$PRATE.prop)
  # gm.PRATE[i] <- exp((1/nrow(temp))*sum(logPR[1:nrow(temp)]))
  # 
  # temp$MINTE.prop[temp$MINTE.prop==0] <- 0.0000000000000000000000000000000000001
  # logMT <- log(temp$MINTE.prop)
  # gm.MINTE[i] <- exp((1/nrow(temp))*sum(logMT[1:nrow(temp)]))
}

hist(gm.MINTE)
hist(gm.PRATE)


odba.gm <- data.frame(MINTE=gm.MINTE, PRATE=gm.PRATE, pop=unique(dat$id_ind))
odba.gm$pop <- ifelse(odba.gm$pop<=22,"Greenland", "Midcontinent")
odba.gm <- gather(odba.gm, key="var", value="value", 1:2)




### Count number of days with "extreme" proportion above or below 0


for (i in 1:length(un.id)) {
  temp <- temp <- dat[dat$animal_id==un.id[i],]
  id <- un.id[i]
  id.no <- temp$plot_id[1]
  n.below <- sum(temp$PRATE.prop<(0.05))
  n.above <- sum(temp$PRATE.prop>0.95)
  n.days <- nrow(temp)
  pct.below <- n.below/n.days * 100
  pct.above <- n.above/n.days * 100
  print(cbind(id, id.no, pct.below, pct.above))
}

#### PTF ####
# Read in calculated proportions
b1.ptf <- read.csv("output/PTF_prate_proportions.csv")
b1.ptf <- b1.ptf[,-1]

b2.ptf <- read.csv("output/PTF_mintemp_proportions.csv")
b2.ptf <- b2.ptf[,-1]

# read data for bird ID numbers
bird <- read.csv("output/dlm_emm_data.csv", stringsAsFactors=FALSE)
bird <- bird[,-c(1,19:22,25,26)]
bird$date <- as.Date(bird$date)

# add IDs 
ids <- unique(bird$animal_id)
colnames(b1.ptf) <- ids
colnames(b2.ptf) <- ids

b1.ptf <- gather(b1.ptf, key="animal_id", value="PRATE.prop.g0", 1:37)
b2.ptf <- gather(b2.ptf, key="animal_id", value="MINTE.prop.g0", 1:37)
ptf <- data.frame(animal_id=b1.ptf$animal_id, PRATE.prop=b1.ptf$PRATE.prop.g0, MINTE.prop=b2.ptf$MINTE.prop.g0)
ptf <- ptf[complete.cases(ptf),]
ptf$rev.rel.day <- NA

un.id <- unique(ptf$animal_id)
for (i in 1:length(un.id)) {
  ptf[ptf$animal_id==un.id[i],4] <- seq(1:nrow(ptf[ptf$animal_id==un.id[i],]))
}

# remove birds with not enough data
bird <- bird[bird$animal_id!="RP08F" & bird$animal_id!="RP15F" & bird$animal_id!="2164"
             & bird$animal_id!="2176" & bird$animal_id!="2160" & bird$animal_id!="2167",]

# Assign new numbers to reflect removal of shortened winters, and to make pop/reproductive status sequential
bird$pop2 <- 1
bird$pop2[bird$pop=="NAMC"] <- 2
N.order <- order(bird$year, decreasing = FALSE)
bird <- bird[N.order,]
N.order <- order(bird$attempt, decreasing=FALSE)
bird <- bird[N.order,]
N.order <- order(bird$pop2, decreasing = FALSE)
bird <- bird[N.order,]
id <- unique(bird$animal_id)
plot_id <- numeric(dim(bird)[1])
for (i in 1:dim(bird)[1]) {
  plot_id[i] <- which(id == bird$animal_id[i])
}
bird <- data.frame(bird, plot_id)
bird <- bird[,-20]

# unique(cbind(bird$id_ind, bird$animal_id, bird$plot_id))

# Join to posterior summary
dat <- left_join(bird, ptf, by=c("animal_id", "rev.rel.day"))
dat <- dat[!is.na(dat$animal_id),]

# Add negative to ones with mean below 0
dat$attempt <- factor(dat$attempt, levels=c("0", "1"))

# Subset by covariate
ptf.na <- dat[dat$pop=="NAMC",]
ptf.gr <- dat[dat$pop=="GRLD",]


gr.prate.ptf <- ggplot(ptf.gr, aes(julian, as.factor(plot_id))) + geom_tile(aes(fill=PRATE.prop), colour = "black") + 
  scale_fill_gradient2(midpoint=0.5, low="#2166ac", mid="white",high="#b2182b", space ="Lab", limits=c(0,1)) +
  # scale_fill_gradient2(midpoint=0, low="#053061", mid="#f7f7f7",high="#67001f", space ="Lab") + 
  coord_cartesian(xlim=c(75,150)) +
  theme_classic() + xlab("Julian Day") + ylab("Individual ID") + ggtitle("Precipitation Rate (mm/hr)") +
  theme(legend.position = 'none',
        # legend.justification=c(1,1.05),legend.title=element_text(size=16, face="bold"), 
        # legend.position=c(1,1), legend.text=element_text(size=14), 
        panel.border=element_rect(color="black", fill=NA, size=0.5),
        axis.title.y=element_text(size=16, face="bold"),
        axis.text=element_text(size=14),#axis.title.x=element_text(size=16, face="bold"),
        axis.title.x=element_blank(),
        strip.text.y=element_text(size=14, face="bold"),
        plot.title=element_text(size=16, face="bold", hjust=0.5)) +
  facet_grid(attempt~., space="free_y", scales="free_y", labeller=repro_labeller)  

gr.mtemp.ptf <- ggplot(ptf.gr, aes(julian, as.factor(plot_id))) + geom_tile(aes(fill=MINTE.prop), colour = "black") + 
  scale_fill_gradient2(midpoint=0.5, low="#2166ac", mid="white",high="#b2182b", space ="Lab", limits=c(0,1)) +
  # scale_fill_gradient2(midpoint=0, low="#053061", mid="#f7f7f7",high="#67001f", space ="Lab") + 
  coord_cartesian(xlim=c(75,160)) +
  theme_classic() + xlab("Julian Day") + ylab("Individual ID") + ggtitle("Minimum Temperature (C)") +
  guides(fill = guide_colourbar(title="Proportion\nSamples > 0")) +
  theme(legend.justification=c(1,1),legend.title=element_text(size=16, face="bold"), 
        legend.position=c(1,1), legend.text=element_text(size=14), 
        legend.background=element_rect(fill=NA),
        panel.border=element_rect(color="black", fill=NA, size=0.5),
        axis.text.x=element_text(size=14), #axis.title.x=element_text(size=16, face="bold"),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(), axis.title.y=element_blank(),
        strip.text.y=element_text(size=14, face="bold"),
        plot.title=element_text(size=16, face="bold", hjust=0.5)) +
  facet_grid(attempt~., space="free_y", scales="free_y", labeller=repro_labeller)  

# grid.arrange(gr.prate.ptf, gr.mtemp.ptf, ncol=2)

na.prate.ptf <- ggplot(ptf.na, aes(julian, as.factor(plot_id))) + geom_tile(aes(fill=PRATE.prop), colour = "black") + 
  scale_fill_gradient2(midpoint=0.5, low="#2166ac", mid="white",high="#b2182b", space ="Lab", limits=c(0,1)) +
  # scale_fill_gradient2(midpoint=0, low="#053061", mid="#f7f7f7",high="#67001f", space ="Lab") + 
  coord_cartesian(xlim=c(35,155)) +
  theme_classic() + xlab("Julian Day") + ylab("Individual ID") + #ggtitle("Precipitation Rate (mm/hr)") +
  theme(legend.position = 'none',
        # legend.justification=c(1,1.05),legend.title=element_text(size=16, face="bold"), 
        # legend.position=c(1,1), legend.text=element_text(size=14), 
        panel.border=element_rect(color="black", fill=NA, size=0.5),
        axis.text=element_text(size=14), axis.title=element_text(size=16, face="bold"),
        strip.text.y=element_text(size=14, face="bold"),
        plot.title=element_text(size=16, face="bold", hjust=0.5)) +
  facet_grid(attempt~., space="free_y", scales="free_y", labeller=repro_labeller)  

na.mtemp.ptf <- ggplot(ptf.na, aes(julian, as.factor(plot_id))) + geom_tile(aes(fill=MINTE.prop), colour = "black") + 
  scale_fill_gradient2(midpoint=0.5, low="#2166ac", mid="white",high="#b2182b", space ="Lab", limits=c(0,1)) +
  # scale_fill_gradient2(midpoint=0, low="#053061", mid="#f7f7f7",high="#67001f", space ="Lab") + 
  coord_cartesian(xlim=c(35,175)) +
  theme_classic() + xlab("Julian Day") + ylab("Individual ID") + #ggtitle("Minimum Temperature (C)") +
  theme(legend.position = 'none',
        # legend.justification=c(1,1.05),legend.title=element_text(size=16, face="bold"), 
        # legend.position=c(1,1), legend.text=element_text(size=14), 
        panel.border=element_rect(color="black", fill=NA, size=0.5),
        axis.text.x=element_text(size=14), axis.title.x=element_text(size=16, face="bold"),
        axis.text.y=element_blank(), axis.title.y=element_blank(),
        strip.text.y=element_text(size=14, face="bold"),
        plot.title=element_text(size=16, face="bold", hjust=0.5)) +
  facet_grid(attempt~., space="free_y", scales="free_y", labeller=repro_labeller)  

# grid.arrange(gr.prate.ptf, gr.mtemp.ptf, na.prate.ptf, na.mtemp.ptf, ncol=2)

lay <- rbind(c(1,1,1,2,2,2),
             c(1,1,1,2,2,2),
             c(1,1,1,2,2,2),
             c(1,1,1,2,2,2),
             c(1,1,1,2,2,2),
             c(1,1,1,2,2,2),
             c(3,3,3,4,4,4),
             c(3,3,3,4,4,4),
             c(3,3,3,4,4,4),
             c(3,3,3,4,4,4))
grid.arrange(gr.prate.ptf, gr.mtemp.ptf, na.prate.ptf, na.mtemp.ptf, layout_matrix=lay)


unique(cbind(dat$id_ind, dat$animal_id, dat$plot_id))

# Calculate geometric mean proportion of posterior samples greater than 0
gm.PRATE <- numeric(31)
gm.MINTE <- numeric(31)
un.id <- unique(dat$animal_id)
for (i in 1:length(un.id)) {
  temp <- dat[dat$animal_id==un.id[i],]
  
  gm.PRATE[i] <- nthroot(prod(temp$PRATE.prop), nrow(temp))
  gm.MINTE[i] <- nthroot(prod(temp$MINTE.prop), nrow(temp))
  
  # temp$PRATE.prop[temp$PRATE.prop==0] <- 0.0000000000000000000000000000000000001
  # logPR <- log(temp$PRATE.prop)
  # gm.PRATE[i] <- exp((1/nrow(temp))*sum(logPR[1:nrow(temp)]))
  # 
  # temp$MINTE.prop[temp$MINTE.prop==0] <- 0.0000000000000000000000000000000000001
  # logMT <- log(temp$MINTE.prop)
  # gm.MINTE[i] <- exp((1/nrow(temp))*sum(logMT[1:nrow(temp)]))
}


ptf.gm <- data.frame(MINTE=gm.MINTE, PRATE=gm.PRATE, pop=unique(dat$id_ind))
ptf.gm$pop <- ifelse(ptf.gm$pop<=22,"Greenland", "Midcontinent")
ptf.gm <- gather(ptf.gm, key="var", value="value", 1:2)
ptf.gm$response <- "PTF"
odba.gm$response <- "ODBA"


gm <- rbind(ptf.gm, odba.gm)
gm$var <- ifelse(gm$var=="MINTE", "Min. Temp.", "Precip. Rate")

ggplot(gm, aes(value)) + 
  geom_histogram(aes(color=var, fill=var), alpha=0.5, binwidth = 0.05) +
  ylab("Number of Individuals") + xlab("Geometric Mean (Proportion of Posterior Samples > 0)") +
  theme_classic() + 
  theme(legend.title=element_text(size=16, face="bold"),
        legend.position=c(0.85,0.7), legend.text=element_text(size=14),
        panel.border=element_rect(color="black", fill=NA, size=0.5),
        axis.text=element_text(size=14), axis.title=element_text(size=16, face="bold"),
        strip.text=element_text(size=14, face="bold")) +
  scale_color_manual(values=c("#2166ac", "#b2182b")) +
  scale_fill_manual(values=c("#2166ac", "#b2182b")) +
  labs(fill="Covariates", color="Covariates") +
  facet_grid(response~pop, space="free_y", scales="free_y")




un.id <- unique(ptf.b2.na$animal_id)

for (i in 1:length(un.id)) {
  temp <- ptf.b2.na[ptf.b2.na$animal_id==un.id[i],]
  id <- un.id[i]
  id.no <- temp$id_ind[1]
  n.below <- sum(temp$f<(-0.95))
  n.above <- sum(temp$f>0.95)
  n.days <- nrow(temp)
  pct.below <- round(n.below/n.days * 100, digits=3)
  pct.above <- round(n.above/n.days * 100, digits=3)
  print(cbind(id, id.no, pct.below, pct.above))
}


