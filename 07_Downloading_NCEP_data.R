
library(RNCEP)
library(tidyverse)
library(raster)
library(lubridate)

rm(list=ls())

options(scipen=999)

# Read in all GPS data (1 point per day)
dat <- read.csv("output/dlm_emm_data.csv", stringsAsFactors=FALSE)
dat <- dat[,-1]

dat$date <- as.Date(dat$date)
dat$timestamp <- as.POSIXct(dat$timestamp, tz="UTC")

un.id <- unique(dat$animal_id)

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

airt1 <- list()
maxt <- list()
mint <- list()
press <- list()
omega <- list()
weqsd <- list()
prcp1 <- list()
clcov <- list()
uwind1 <- list()
vwind1 <- list()
uwind2 <- list()
vwind2 <- list()
for (i in 1:length(un.id)) {
  temp <- subset(new, animal_id==un.id[i])
  prcp.dat <- NCEP.interp(variable='prate.sfc', level='gaussian', lat=temp$latitude, lon=temp$longitude,
                        dt=temp$timestamp, reanalysis2=TRUE, interpolate.space=TRUE, interp='linear',
                        return.units=TRUE, status.bar=TRUE)
  airt.dat <- NCEP.interp(variable='air.2m', level='gaussian', lat=temp$latitude, lon=temp$longitude,
                  dt=temp$timestamp, reanalysis2=TRUE, interpolate.space=TRUE, interp='linear', return.units=TRUE)
  maxt.dat <- NCEP.interp(variable='tmax.2m', level='gaussian', lat=temp$latitude, lon=temp$longitude,
                  dt=temp$timestamp, reanalysis2=TRUE, interpolate.space=TRUE, interp='linear', return.units=TRUE)
  mint.dat <- NCEP.interp(variable='tmin.2m', level='gaussian', lat=temp$latitude, lon=temp$longitude,
                  dt=temp$timestamp, reanalysis2=TRUE, interpolate.space=TRUE, interp='linear', return.units=TRUE)
  press.dat <- NCEP.interp(variable='pres.sfc', level='surface', lat=temp$latitude, lon=temp$longitude,
                  dt=temp$timestamp, reanalysis2=TRUE, interpolate.space=TRUE, interp='linear', return.units=TRUE)
  omega.dat <- NCEP.interp(variable='omega.sig995', level='surface', lat=temp$latitude, lon=temp$longitude,
                  dt=temp$timestamp, reanalysis2=FALSE, interpolate.space=TRUE, interp='linear', return.units=TRUE)
  weqsd.dat <- NCEP.interp(variable='weasd.sfc', level='gaussian', lat=temp$latitude, lon=temp$longitude,
                  dt=temp$timestamp, reanalysis2=TRUE, interpolate.space=TRUE, interp='linear', return.units=TRUE)
  clcov.dat <- NCEP.interp(variable='tcdc.eatm', level='gaussian', lat=temp$latitude, lon=temp$longitude,
                  dt=temp$timestamp, reanalysis2=TRUE, interpolate.space=TRUE, interp='linear', return.units=TRUE)
  uwind500.dat <- NCEP.interp(variable='uwnd', level=500, lat=temp$latitude, lon=temp$longitude, 
			dt=temp$timestamp, reanalysis2=TRUE, interp='linear', return.units=TRUE)
  vwind500.dat <- NCEP.interp(variable='vwnd', level=500, lat=temp$latitude, lon=temp$longitude, 
			dt=temp$timestamp, reanalysis2=TRUE, interp='linear', return.units=TRUE)
  uwind850.dat <- NCEP.interp(variable='uwnd', level=850, lat=temp$latitude, lon=temp$longitude, 
			dt=temp$timestamp, reanalysis2=TRUE, interp='linear', return.units=TRUE)
  vwind850.dat <- NCEP.interp(variable='vwnd', level=850, lat=temp$latitude, lon=temp$longitude, 
			dt=temp$timestamp, reanalysis2=TRUE, interp='linear', return.units=TRUE)
  airt1[[i]] <- airt.dat
  prcp1[[i]] <- prcp.dat
  maxt[[i]] <- maxt.dat
  mint[[i]] <- mint.dat
  press[[i]] <- press.dat
  omega[[i]] <- omega.dat
  weqsd[[i]] <- weqsd.dat
  clcov[[i]] <- clcov.dat
  uwind1[[i]] <- uwind500.dat
  vwind1[[i]] <- vwind500.dat
  uwind2[[i]] <- uwind850.dat
  vwind2[[i]] <- vwind850.dat
}
save(file="C:/Users/sacdc5/Desktop/WeatherData/prate_all.Rdata", list="prcp1")
save(file="C:/Users/sacdc5/Desktop/WeatherData/meantemp_all.Rdata", list="airt1")
save(file="C:/Users/sacdc5/Desktop/WeatherData/maxtemp_all.Rdata", list="maxt")
save(file="C:/Users/sacdc5/Desktop/WeatherData/mintemp_all.Rdata", list="mint")
save(file="C:/Users/sacdc5/Desktop/WeatherData/press.sfc_all.Rdata", list="press")
save(file="C:/Users/sacdc5/Desktop/WeatherData/omega.sig995_all.Rdata", list="omega")
save(file="C:/Users/sacdc5/Desktop/WeatherData/weasd.sfc_all.Rdata", list="weqsd")
save(file="C:/Users/sacdc5/Desktop/WeatherData/tcdc.eatm_all.Rdata", list="clcov")
save(file="C:/Users/sacdc5/Desktop/WeatherData/uwind500_all.Rdata", list="uwind1")
save(file="C:/Users/sacdc5/Desktop/WeatherData/vwind500_all.Rdata", list="vwind1")
save(file="C:/Users/sacdc5/Desktop/WeatherData/uwind850_all.Rdata", list="uwind2")
save(file="C:/Users/sacdc5/Desktop/WeatherData/vwind850_all.Rdata", list="vwind2")


