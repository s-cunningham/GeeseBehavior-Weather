library(tidyverse)
library(gridExtra)
library(pracma)

options(scipen=999, digits=3)

# Read in calculated proportions
prate <- read.csv("output/20210901prate_ptail.csv")
prate <- prate[,-1]
names(prate)[1:26] <- str_extract(names(prate)[1:26], "[:digit:]+")

mintemp <- read.csv("output/20210901mintemp_ptail.csv")
mintemp <- mintemp[,-1]
names(mintemp)[1:26] <- str_extract(names(mintemp)[1:26], "[:digit:]+")

# Read in individual data
dat1 <-read.csv("output/dlm_emm_data-prop.csv", stringsAsFactors=FALSE)
dat1 <- dat1[,c(2:10,12,22,23,25)]
dat1 <- dat1[dat1$animal_id!="LM17M" & dat1$animal_id!="RP15F",]

dr <- read.csv("output/migration_days.csv") 
dr <- dr[,-1]

# Make sure migration days add up
un.id <- unique(dat1$animal_id)

dat <- data.frame()
for (i in 1:length(un.id)) {
  
  dr.1 <- dr[dr$id==un.id[i],]
  
  temp <- dat1[dat1$animal_id==un.id[i] & (dat1$julian>=dr.1$start & dat1$julian<=dr.1$end),]
  
  dat <- rbind(dat, temp)
  
}
rm(dat1)

# add posterior proportions 
dat$prate.ptail <- NA
dat$mintemp.ptail <- NA

for (i in 1:length(un.id)) {
  
  pr <- prate[,un.id[i]]
  pr <- pr[!is.na(pr)]

  mt <- mintemp[,un.id[i]]
  mt <- mt[!is.na(mt)]

  dat$prate.ptail[dat$animal_id==un.id[i]] <- pr
  dat$mintemp.ptail[dat$animal_id==un.id[i]] <- mt

  if (length(pr) != nrow(dat[dat$animal_id==un.id[i],])) {
    print(un.id[i])
    print(length(pr))
    print(nrow(dat[dat$animal_id==un.id[i],]))
  }
}


dat$attempt <- 0
dat$attempt[dat$animal_id=="17763" | dat$animal_id=="17777" |
              dat$animal_id=="17778" | dat$animal_id=="17780" |
              dat$animal_id=="2161" | dat$animal_id=="2838" |
              dat$animal_id=="RP01F" | dat$animal_id=="RP08F" | 
              dat$animal_id=="RP17F" | dat$animal_id=="RP22F"] <- 1

# write.csv(dat, "output/20210901dlm_results_ratio.csv")

dat$birdno <- as.numeric(factor(dat$key, labels=seq(1,34,1)))


# pivot longer
dat <- pivot_longer(dat, cols=14:15, names_to="variable", values_to="ptail")
dat <- as.data.frame(dat)

# Plot 
repro_list <- list('0'="Defer/Fail", '1'="Succeed")  
repro_labeller <- function(variable,value) {
  return(repro_list[value])
}


dat$labels <- ifelse(dat$attempt==1, "Succeed", "Defer/Fail")
dat$variable <- ifelse(dat$variable=="mintemp.ptail", "Minimum Temperature (C)", "Precipitation Rate (mm/hr)")

# indicate if overlaps 0 (confusing..."y" and "n" are backwards?) 
dat$overlap0 <- ifelse((dat$ptail<=0.025 | dat$ptail>=0.975), "y", "n")

grld <- dat[dat$pop=="GRLD",]
namc <- dat[dat$pop=="NAMC",]

grld2 <- grld[grld$overlap0=="y",]
namc2 <- namc[namc$overlap0=="y",]

gr.plot <- ggplot(grld, aes(julian, as.factor(birdno))) + geom_tile(aes(fill=ptail), colour="black") +
  scale_fill_gradient2(midpoint=0.5, low="#2166ac", mid="white",high="#b2182b", space ="Lab", limits=c(0,1)) +
  geom_point(data=grld2, aes(julian, as.factor(birdno)), shape=15, color="black") +
  coord_cartesian(xlim=c(60,150)) +
  theme_classic() + xlab("Date") + ylab("Goose ID (Greenland)") + 
  scale_x_continuous(breaks=c(75,100,125,150), labels=c("16-Mar","10-Apr","05-May","30-May")) +
  guides(fill = guide_colourbar(title="Proportion\nSamples >0"), color="none") +
  theme(legend.position=c(0.01,1), legend.justification=c(0.01,1),
        legend.background=element_blank(),
        panel.border=element_rect(color="black", fill=NA, size=0.5),
        axis.text=element_text(size=12), axis.title.y=element_text(size=14),
        axis.title.x=element_blank(),
        legend.text=element_text(size=12), legend.title=element_text(size=12, face="bold"),
        strip.text=element_text(size=14, face="bold")) +
  facet_grid(labels~variable, space="free_y", scales="free", labeller=label_value)  

na.plot <- ggplot(namc, aes(julian, as.factor(birdno))) +
  geom_tile(aes(fill=ptail), colour = "black") +
  geom_point(data=namc2, aes(julian, as.factor(birdno)), shape=15, color="black") +
  scale_fill_gradient2(midpoint=0.5, low="#2166ac", mid="white",high="#b2182b", space ="Lab", limits=c(0,1)) +
  theme_classic() + xlab("Date") + ylab("Goose ID (Midcon.)") + 
  scale_x_continuous(breaks=c(30,60,90,120,150), labels=c("30-Jan","01-Mar","30-Mar","30-Apr","30-May")) +
  theme(legend.position = 'none',
        panel.border=element_rect(color="black", fill=NA, size=0.5),
        axis.text=element_text(size=12), axis.title=element_text(size=14),
        strip.text.y=element_text(size=12, face="bold"),
        strip.text.x=element_blank(),
        plot.title=element_text(size=16, face="bold", hjust=0.5)) +
  facet_grid(labels~variable, space="free_y", scales="free", labeller=label_value) 


lay <- rbind(c(1),
             c(1),
             c(1),
             c(1),
             c(1),
             c(1),
             c(1),
             c(1),
             c(1),
             c(1),
             c(1),
             c(1),
             c(2),
             c(2),
             c(2),
             c(2),
             c(2))
grid.arrange(gr.plot, na.plot, layout_matrix=lay)








