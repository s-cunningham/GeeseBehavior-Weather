## Adding weather predictors to data frame
## 25 Jul 2019

library(tidyverse)
library(lubridate)

# Read in all GPS data (1 point per day)
dat <- read.csv("output/dlm_emm_data.csv", stringsAsFactors=FALSE)
dat <- dat[,-1]


dat$date <- as.Date(dat$date)
dat$timestamp <- as.POSIXct(dat$timestamp, tz="UTC")

un.id <- unique(dat$animal_id)

# loop over each individual 
new <- data.frame()
for (j in 1:length(un.id)) {
  temp1 <- subset(dat, animal_id==un.id[j])
  days <- unique(temp1$date)
  temp2 <- data.frame()
  for (i in 1:length(days)) {
    temp <- subset(temp1, date==days[i])
    early <- min(hour(temp$timestamp))
    erpt <- temp[(hour(temp$timestamp)==early),][1,]
    temp2 <- rbind(temp2, erpt)
  }
  if (temp2$pop[1]=="GRLD") {
    expanded <- temp2[rep(row.names(temp2), 4),]
    rownames(expanded) <- seq(1,nrow(expanded),1)
    tot <- nrow(expanded)/4
    expanded[(tot+1):(tot+tot),5] <- expanded[1:tot, 5] - hours(12) 
    expanded[((2*tot)+1):((2*tot)+tot),5] <- expanded[1:tot, 5] - hours(6) # 
    expanded[((3*tot)+1):((3*tot)+tot),5] <- expanded[1:tot, 5] + hours(6) # 
    new <- rbind(new, expanded)
  } else if (temp2$pop[1]=="NAMC") {
    expanded <- temp2[rep(row.names(temp2), 4),]
    rownames(expanded) <- seq(1,nrow(expanded),1)
    tot <- nrow(expanded)/4
    expanded[(tot+1):(tot+tot),5] <- expanded[1:tot, 5] - hours(10) 
    expanded[((2*tot)+1):((2*tot)+tot),5] <- expanded[1:tot, 5] - hours(5) # 
    expanded[((3*tot)+1):((3*tot)+tot),5] <- expanded[1:tot, 5] + hours(1) # 
    new <- rbind(new, expanded)
  }
}  

# Add in new weather data (format: list of vectors)
load("data/weather_data/maxtemp_all.Rdata")
load("data/weather_data/mintemp_all.Rdata")
load("data/weather_data/meantemp_all.Rdata")
load("data/weather_data/prate_all.Rdata")
load("data/weather_data/weasd.sfc_all.Rdata")
load("data/weather_data/press.sfc_all.Rdata")
load("data/weather_data/tcdc.eatm_all.Rdata")
load("data/weather_data/omega.sig995_all.Rdata")
load("data/weather_data/uwind500_all.Rdata")
load("data/weather_data/uwind850_all.Rdata")
load("data/weather_data/vwind500_all.Rdata")
load("data/weather_data/vwind850_all.Rdata")


un.id <- unique(new$animal_id) 

new$date <- as.Date(new$date)

wdat <- data.frame()
for (j in 1:length(un.id)) {
  dat <- new[new$animal_id==un.id[j], c(1,2,24,3:17,22,23)]
  
  # Temperature variables
  dat$mtemp <- airt1[[j]]
  dw <- aggregate(mtemp ~ animal_id + date, data=dat, FUN=mean)  # Mean temperature (deg K)
  dw$mtemp <- dw$mtemp - 273.15 # convert to degrees C
  
  mint2 <- data.frame(animal_id=dat$animal_id, date=dat$date, mint=mint[[j]])  # Minimum temperature (deg K)
  mint2$mint <- mint2$mint - 273.15
  dmint <- aggregate(mint ~ animal_id + date, data=mint2, FUN=mean)
  
  tmax <- data.frame(animal_id=dat$animal_id, date=dat$date, maxt=maxt[[j]])  # Maximum temperature (deg K)
  tmax$maxt <- tmax$maxt - 273.15
  dmaxt <- aggregate(maxt ~ animal_id + date, data=tmax, FUN=mean)
  
  # Wind Variables
  vertw <- data.frame(animal_id=dat$animal_id, date=dat$date, omega=omega[[j]])  # Vertical velocity (Pascal/s)
  dvertw <- aggregate(omega ~ animal_id + date, data=vertw, FUN=mean)
  
  u500 <- data.frame(animal_id=dat$animal_id, date=dat$date, uwind.500=uwind1[[j]])  # u-wind component at 500 millibars pressure (m/s)
  du500 <- aggregate(uwind.500 ~ animal_id + date, data=u500, FUN=mean)
  
  v500 <- data.frame(animal_id=dat$animal_id, date=dat$date, vwind.500=vwind1[[j]])  # v-wind component at 500 millibars pressure (m/s)
  dv500 <- aggregate(vwind.500 ~ animal_id + date, data=v500, FUN=mean)
  
  u850 <- data.frame(animal_id=dat$animal_id, date=dat$date, uwind.850=uwind2[[j]])  # u-wind component at 850 millibars pressure (m/s)
  du850 <- aggregate(uwind.850 ~ animal_id + date, data=u850, FUN=mean)
  
  v850 <- data.frame(animal_id=dat$animal_id, date=dat$date, vwind.850=vwind2[[j]])  # v-wind component at 850 millibars pressure (m/s)
  dv850 <- aggregate(vwind.850 ~ animal_id + date, data=v850, FUN=mean)
  
  # Atmosphere variables
  tcdc <- data.frame(animal_id=dat$animal_id, date=dat$date, clcov=clcov[[j]])  # % cloud cover
  dtcdc <- aggregate(clcov ~ animal_id + date, data=tcdc, FUN=mean)
  
  sfcprs <- data.frame(animal_id=dat$animal_id, date=dat$date, pressure=press[[j]]) # Pressure measured at surface (Pascals)
  dsfcprs <- aggregate(pressure ~ animal_id + date, data=sfcprs, FUN=mean)
  
  # Precipitation Variables
  prcp <- data.frame(animal_id=dat$animal_id, date=dat$date, prate=prcp1[[j]])  # Precipitation rate (Kg/m^2/s)
  prcp$prate <- prcp$prate * 3600 # convert to mm/hr
  dp <- aggregate(prate ~ animal_id + date, data=prcp, FUN=mean)
  
  h2osd <- data.frame(animal_id=dat$animal_id, date=dat$date, weqsd=weqsd[[j]]) # water equivalent of snow depth at surface (kg/m^2, basically equal to mm)
  dh2osd <- aggregate(weqsd ~ animal_id + date, data=h2osd, FUN=mean)
  
  dw <- cbind(dw, dmint$mint, dmaxt$maxt, dvertw$omega, du500$uwind.500, dv500$vwind.500, du850$uwind.850, dv850$vwind.850, 
              dtcdc$clcov, dsfcprs$pressure, dp$prate, dh2osd$weqsd)
  names(dw)[4:14] <- c("mintemp", "maxtemp", "omega", "uwind.500", "vwind.500", "uwind.850", "vwind.850", "cloudcov", "pressure.sfc", "prate", "weqsd")
  
  wdat <- rbind(wdat, dw)
}

write.csv(wdat, "output/weather_covars.csv")
