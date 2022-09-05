## 2022-08-30

library(tidyverse)
library(patchwork)

options(scipen=999, digits=3)

theme_set(theme_classic())

#### Read in bird data ####

# Read in weather covariates
wdat <- read_csv("files_for_models/weather_covars.csv")

# Read in ACC data
dat <- read_csv("files_for_models/daily_odba_behavior.csv")

# Join weather and ACC data
dat <- left_join(dat, wdat, by=c("animal_id", "date"))
dat <- as.data.frame(dat)

# read in migration dates
mdates <- read_csv("files_for_models/migration_dates.csv")

# save maximum duration of migration period
dur <- max(mdates$duration) 

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
dat <- dat[!is.na(dat$migration), ]

# log odba
dat$lnODBAmedian <- log(dat$median.odba)

# Reorganize columns
dat <- dat[,c(1,3,4,23,24,5:8,10,11,16,18,19,21)]

#### Reading in and organizing posteriors ####
# Read in calculated proportions for ODBA
b1.odba <- read_csv("results/ODBAprcp_ptail.csv")
b1.odba <- as.data.frame(b1.odba)
b1.odba$RelDay <- 1:120
b1.odba <- b1.odba[,c(36,1:35)]
b1.odba <- pivot_longer(b1.odba, 2:36, names_to="animal_id", values_to="ODBA_prcp")

b2.odba <- read_csv("results/ODBAmintemp_ptail.csv")
b2.odba <- as.data.frame(b2.odba)
b2.odba$RelDay <- 1:120
b2.odba <- b2.odba[,c(36,1:35)]
b2.odba <- pivot_longer(b2.odba, 2:36, names_to="animal_id", values_to="ODBA_temp")

# Read in calculated proportions for ODBA
b1.ptf <- read_csv("results/PTFprcp_ptail.csv")
b1.ptf <- as.data.frame(b1.ptf)
b1.ptf$RelDay <- 1:120
b1.ptf <- b1.ptf[,c(36,1:35)]
b1.ptf <- pivot_longer(b1.ptf, 2:36, names_to="animal_id", values_to="PTF_prcp")

b2.ptf <- read_csv("results/PTFmintemp_ptail.csv")
b2.ptf <- as.data.frame(b2.ptf)
b2.ptf$RelDay <- 1:120
b2.ptf <- b2.ptf[,c(36,1:35)]
b2.ptf <- pivot_longer(b2.ptf, 2:36, names_to="animal_id", values_to="PTF_temp")

# Join posterior summary to data
dat <- left_join(dat, b1.odba, by=c("animal_id", "RelDay"))
dat <- left_join(dat, b2.odba, by=c("animal_id", "RelDay"))
dat <- left_join(dat, b1.ptf, by=c("animal_id", "RelDay"))
dat <- left_join(dat, b2.ptf, by=c("animal_id", "RelDay"))

# pivot longer
dat <- pivot_longer(dat, 16:19, names_to="model", values_to="ptail")
dat <- separate(dat, 16, into=c("response", "covariate"), sep="_", remove=FALSE) %>% as.data.frame()

#### Create heatmap ####
dat$birdno <- as.character(factor(dat$animal_id, levels=unique(dat$animal_id),
                                labels=c("M12GR1", "F18GR1", "F18GR2", "F18GR3",
                                         "F18GR4", "F18GR5", "F18GR6", "F18GR7",
                                         "F18GR8", "F18GR9", "F18GR10", "M12GR2",
                                         "M12GR3", "M12GR4", "M12GR5", "M12GR6",
                                         "M12GR7", "M13GR1", "M13GR2", "M13GR3",
                                         "M13GR4", "M13GR5", "M13GR6", "M13GR7",
                                         "M13GR8", "M17MC1", "F18MC6", "F17MC1",
                                         "F17MC2", "F18MC3", "F18MC1",  "F18MC2",
                                         "F17MC3", "F18MC4", "F18MC5")))

dat$birdno <- factor(dat$birdno, levels=c("M12GR1", "M12GR2",
                              "M12GR3", "M12GR4", "M12GR5", "M12GR6",
                              "M12GR7", "M13GR1", "M13GR2", "M13GR3",
                              "M13GR4", "M13GR5", "M13GR6", "M13GR7",
                              "M13GR8", 
                              "F18GR1", "F18GR2", "F18GR3",
                              "F18GR4", "F18GR5", "F18GR6", "F18GR7",
                              "F18GR8", "F18GR9", "F18GR10",
                              "M17MC1", "F17MC1",
                              "F17MC2", "F18MC3", "F18MC1",  "F18MC2",
                              "F17MC3", "F18MC4", "F18MC5", "F18MC6"))

# Split by response
odba <- dat[dat$response=="ODBA",]
ptf <- dat[dat$response=="PTF",]

# Save colors
color_breaks <- c(0, 0.15, 0.3, 0.5, 0.7, 0.85, 1)
colors <- c("#2166ac","#4393c3", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#d6604d", "#b2182b")

# Facet names
var_names <- c(prcp="Precipitation (mm)",
               temp="Minimum Temperature (C)")

# Plot
ggplot(odba, aes(x=julian, y=factor(birdno))) + geom_tile(aes(fill=ptail), colour = "black") + 
  scale_fill_gradientn(limits=c(0,1), colors=colors[c(1, seq_along(colors), length(colors))],
                       values=c(scales::rescale(color_breaks, from=c(0,1)))) +
  xlab("Date") +
  scale_x_continuous(breaks=c(30,60,90,120,150), labels=c("30-Jan","01-Mar","30-Mar","30-Apr","30-May")) +
  facet_grid(.~covariate, labeller=as_labeller(var_names)) + 
  guides(fill=guide_colourbar(title="Proportion\nSamples >0")) +
  theme(legend.justification=c(0,0),
        legend.position=c(0,0.01), 
        legend.title=element_text(size=12, face="bold"), 
        legend.text=element_text(size=10), 
        legend.background=element_rect(fill=NA),
        panel.border=element_rect(color="black", fill=NA, size=0.5),
        axis.text=element_text(size=10), 
        axis.title.y=element_blank(),
        axis.title.x=element_text(size=12, face="bold"),
        strip.text.x=element_text(size=12, face="bold"),
        strip.background=element_rect(fill="white"))
  
  
ggplot(ptf, aes(x=julian, y=factor(birdno))) + geom_tile(aes(fill=ptail), colour = "black") + 
  scale_fill_gradientn(limits=c(0,1), colors=colors[c(1, seq_along(colors), length(colors))],
                       values=c(scales::rescale(color_breaks, from=c(0,1)))) +
  xlab("Date") +
  scale_x_continuous(breaks=c(30,60,90,120,150), labels=c("30-Jan","01-Mar","30-Mar","30-Apr","30-May")) +
  facet_grid(.~covariate, labeller=as_labeller(var_names)) + 
  guides(fill=guide_colourbar(title="Proportion\nSamples >0")) +
  theme(legend.justification=c(0,0),
        legend.position=c(0,0.01), 
        legend.title=element_text(size=12, face="bold"), 
        legend.text=element_text(size=10), 
        legend.background=element_rect(fill=NA),
        panel.border=element_rect(color="black", fill=NA, size=0.5),
        axis.text=element_text(size=10), 
        axis.title.y=element_blank(),
        axis.title.x=element_text(size=12, face="bold"),
        strip.text.x=element_text(size=12, face="bold"),
        strip.background=element_rect(fill="white"))


## Combine into a single plot
p1 <- ggplot(odba, aes(x=julian, y=factor(birdno))) + geom_tile(aes(fill=ptail), colour = "black") + 
  scale_fill_gradientn(limits=c(0,1), colors=colors[c(1, seq_along(colors), length(colors))],
                       values=c(scales::rescale(color_breaks, from=c(0,1)))) +
  xlab("Date") +
  scale_x_continuous(breaks=c(30,60,90,120,150), labels=c("30-Jan","01-Mar","30-Mar","30-Apr","30-May")) +
  facet_grid(.~covariate, labeller=as_labeller(var_names)) + 
  guides(fill=guide_colourbar(title="Proportion\nSamples >0")) +
  theme(legend.position="none",
        panel.border=element_rect(color="black", fill=NA, size=0.5),
        axis.text.y=element_text(size=8), 
        axis.text.x=element_blank(), 
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        strip.text.x=element_text(size=10, face="bold"),
        strip.background=element_rect(fill="white"))


p2 <- ggplot(ptf, aes(x=julian, y=factor(birdno))) + geom_tile(aes(fill=ptail), colour = "black") + 
  scale_fill_gradientn(limits=c(0,1), colors=colors[c(1, seq_along(colors), length(colors))],
                       values=c(scales::rescale(color_breaks, from=c(0,1)))) +
  xlab("Date") +
  scale_x_continuous(breaks=c(30,60,90,120,150), labels=c("30-Jan","01-Mar","30-Mar","30-Apr","30-May")) +
  facet_grid(.~covariate, labeller=as_labeller(var_names)) + 
  guides(fill=guide_colourbar(title="Proportion\nSamples >0")) +
  theme(legend.justification=c(0,0),
        legend.position=c(0,0.01), 
        legend.title=element_text(size=10, face="bold"), 
        legend.text=element_text(size=10), 
        legend.background=element_rect(fill=NA),
        panel.border=element_rect(color="black", fill=NA, size=0.5),
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=8), 
        axis.title.y=element_blank(),
        axis.title.x=element_text(size=10, face="bold"),
        strip.text.x=element_blank())

patchwork <- p1 / p2
patchwork + plot_annotation(tag_levels="a", tag_prefix="(", tag_suffix=")") 



