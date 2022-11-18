## Adding weather predictors to data frame
## 24 Aug 2022

library(tidyverse)

# Read in daily info
new <- read_csv("files_for_models/temperature_meta.csv")

# Add in new weather data (format: list of vectors)
load("output/maxtemp_all.Rdata")
load("output/mintemp_all.Rdata")
load("output/meantemp_all.Rdata")

un.id <- unique(new$animal_id) 

new$date <- as.Date(new$date)

wdat <- data.frame()
for (j in 1:length(un.id)) {
  dat <- new[new$animal_id==un.id[j], ]
  
  # Temperature variables
  dat$mtemp <- airt[[j]]
  dw <- aggregate(mtemp ~ animal_id + date, data=dat, FUN=mean)  # Mean temperature (deg K)
  dw$mtemp <- dw$mtemp - 273.15 # convert to degrees C
  
  mint2 <- data.frame(animal_id=dat$animal_id, date=dat$date, mint=mint[[j]])  # Minimum temperature (deg K)
  mint2$mint <- mint2$mint - 273.15
  dmint <- aggregate(mint ~ animal_id + date, data=mint2, FUN=mean)
  
  tmax <- data.frame(animal_id=dat$animal_id, date=dat$date, maxt=maxt[[j]])  # Maximum temperature (deg K)
  tmax$maxt <- tmax$maxt - 273.15
  dmaxt <- aggregate(maxt ~ animal_id + date, data=tmax, FUN=mean)
  
  dw <- cbind(dw, dmint$mint, dmaxt$maxt)
  names(dw)[4:5] <- c("mintemp", "maxtemp")
  
  wdat <- rbind(wdat, dw)
}

# Read in precipitation data
precip <- read_csv("files_for_models/daily_precip.csv")

# join precipitation and temperature data
wdat <- left_join(precip, wdat, by=c("animal_id", "date"))

# save precipitation and weather data
# write_csv(wdat, "files_for_models/weather_covars.csv")
wdat <- read_csv("files_for_models/weather_covars.csv")

## Check correlation between minimum temperature and precipitation
cor.test(wdat$prcp, wdat$mintemp, alternative="two.sided", method="pearson", conf.level=0.95)

cor(wdat[,c(3,5)])
