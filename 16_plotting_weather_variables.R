
## 2022-08-31

library(tidyverse)
library(patchwork)

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

# Weather summary states
# dat %>% group_by(pop, variable) %>% summarize(range(values))

#### Plot weather variables ####

dat <- pivot_longer(dat, 14:15, names_to="variable", values_to="values") %>% as.data.frame()

prcp <- dat %>% filter(variable=="prcp")
temp <- dat %>% filter(variable=="mintemp")

# Plot precipitation
prcp_plot <- ggplot() + 
              geom_line(data=prcp, aes(x=julian, y=values, group=animal_id, color=pop), alpha=0.5, size=1) +
              scale_x_continuous(breaks=c(60,91,121,152), 
                                 labels=c("1-Mar","1-Apr","1-May","1-Jun")) +
              scale_color_manual(values=c("#2166ac","#b2182b"), labels=c("Greenland", "Midcontinent")) +
              facet_grid(.~year, scales="free_x") +
              xlab("Date") + ylab("Precipitation (mm)") +
              guides(color=guide_legend(title="Population")) +
              annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
              annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
              theme(legend.position='bottom', 
                    legend.title=element_text(size=12), 
                    legend.text=element_text(size=12), 
                    legend.background=element_rect(fill=NA),
                    panel.border=element_blank(),
                    axis.line=element_line(),
                    axis.text.y=element_text(size=12), 
                    axis.text.x=element_blank(),
                    axis.title.y=element_text(size=12),
                    axis.title.x=element_blank(),
                    strip.text=element_text(size=12),
                    strip.background=element_rect(fill="white", color=NA))

temp_plot <- ggplot() + 
  geom_line(data=temp, aes(x=julian, y=values, group=animal_id, color=pop), alpha=0.5, size=1) +
  scale_x_continuous(breaks=c(60,91,121,152), 
                     labels=c("1-Mar","1-Apr","1-May","1-Jun")) +
  scale_color_manual(values=c("#2166ac","#b2182b"), labels=c("Greenland", "Midcontinent")) +
  facet_grid(.~year, scales="free_x") +
  xlab("Date") + ylab("Temperature (C)") +
  guides(color=guide_legend(title="Population")) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
  theme(legend.position='bottom', 
        legend.title=element_text(size=12), 
        legend.text=element_text(size=12), 
        legend.background=element_rect(fill=NA),
        panel.border=element_blank(),
        axis.line=element_line(),
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12),
        strip.text=element_blank(),
        strip.background=element_rect(fill="white"))


(prcp_plot / temp_plot + plot_layout(guides="collect")) + 
  plot_annotation(tag_levels="a", tag_prefix="(", tag_suffix=")") & 
  theme(legend.position = 'bottom')

