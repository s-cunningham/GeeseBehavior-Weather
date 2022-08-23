
library(RNCEP)
library(tidyverse)
library(raster)
library(lubridate)

options(scipen=999)

# Read in all GPS data (1 point per day)
dat <- read_csv("files_for_models/daily_odba_behavior.csv")

# Create timestamp column
dat$time <- "16:00:00"
dat$time[dat$pop=="NAMC"] <- "21:00:00"
dat <- unite(dat, "timestamp", c(3,19), sep=" ", remove=FALSE)
dat <- dat[,-20]

# Convert date and timestamp format
dat$date <- as.Date(dat$date)
dat$timestamp <- as.POSIXct(dat$timestamp, tz="UTC")

# Convert to data.frame
dat <- as.data.frame(dat)

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

# Set up lists to store weather data
# airt <- list()
# maxt <- list()
# mint <- list()

# Load partial lists
load(file="output/meantemp_all.Rdata")
load(file="output/maxtemp_all.Rdata")
load(file="output/mintemp_all.Rdata")

for (i in 18:length(un.id)) {
  
  # Subset to individual
  temp <- subset(new, animal_id==un.id[i])
 
  # Download temp data
  airt.dat <- NCEP.interp(variable='air.2m', level='gaussian', lat=temp$latitude, lon=temp$longitude,
                  dt=temp$timestamp, reanalysis2=TRUE, interpolate.space=TRUE, interp='linear', return.units=TRUE)
  maxt.dat <- NCEP.interp(variable='tmax.2m', level='gaussian', lat=temp$latitude, lon=temp$longitude,
                  dt=temp$timestamp, reanalysis2=TRUE, interpolate.space=TRUE, interp='linear', return.units=TRUE)
  mint.dat <- NCEP.interp(variable='tmin.2m', level='gaussian', lat=temp$latitude, lon=temp$longitude,
                  dt=temp$timestamp, reanalysis2=TRUE, interpolate.space=TRUE, interp='linear', return.units=TRUE)
  
  # Save data
  airt[[i]] <- airt.dat
  maxt[[i]] <- maxt.dat
  mint[[i]] <- mint.dat

}

# Write to file
save(file="output/meantemp_all.Rdata", list="airt")
save(file="output/maxtemp_all.Rdata", list="maxt")
save(file="output/mintemp_all.Rdata", list="mint")


