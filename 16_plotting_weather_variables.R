
## 2022-08-31

library(tidyverse)

options(scipen=999, digits=3)

theme_set(theme_classic())

#### Read in bird data ####

# Read in weather covariates
wdat <- read_csv("files_for_models/weather_covars.csv")
range(wdat$prcp, na.rm=TRUE)
wdat$prcp[wdat$prcp==-99999] <- NA

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

#### Plot weather variables ####

dat <- pivot_longer(dat, 14:15, names_to="variable", values_to="values") %>% as.data.frame()


# Facet names
var_names <- c(prcp="Precipitation\n(mm)",
               mintemp="Minimum\nTemperature (C)",
               GRLD="Greenland", 
               NAMC="Midcontinent")

ggplot(dat, aes(x=julian, y=values, group=animal_id, color=factor(year))) + 
  geom_line(alpha=0.5, size=1) +
  scale_x_continuous(breaks=c(46,60,74,90,105,120,135), 
                     labels=c("15-Feb","1-Mar","15-Mar","30-Mar","15-Apr","30-Apr","15-May")) +
  scale_color_manual(values=c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c")) +
  facet_grid(variable~pop, scales="free",labeller=as_labeller(var_names)) +
  xlab("Date") +
  guides(color=guide_legend(title="Year")) +
  theme(legend.justification=c(0,1),
        legend.position=c(0,0.5), 
        legend.title=element_text(size=13, face="bold"), 
        legend.text=element_text(size=11), 
        legend.background=element_rect(fill=NA),
        panel.border=element_rect(color="black", fill=NA, size=0.5),
        axis.text=element_text(size=12), 
        axis.title.y=element_blank(),
        axis.title.x=element_text(size=13, face="bold"),
        strip.text=element_text(size=13, face="bold"),
        strip.background=element_rect(fill="white"))






