#**********************************************************************************************************************************
#**********************************************************************************************************************************

# Project: 
# Date: 
# Author: Stephanie Cunningham
# Description: 

#**********************************************************************************************************************************
#**********************************************************************************************************************************

library(move)
library(tidyverse)

# Read data (subset, does not include summer breeding season)
# dat <- read.csv("output/combined_gps_acc_subset.csv", stringsAsFactors=FALSE)
# dat <- dat[,-1]

# Read in daily GPS data
setwd("data/CSV_1ppd")
file.list <- list.files(path="./", pattern=".csv", all.files=TRUE, full.names=FALSE)
file.list <- lapply(file.list, FUN=read.csv, header=TRUE, stringsAsFactors=FALSE)
dat <- do.call("rbind", file.list)
setwd("G:/ResearchProjects/GeeseBehavior-Weather")

dat <- dat[,-1]
dat <- dat[dat$key!="502_2018" & dat$key!="523_2018" & dat$key!="527_2018" & dat$key!="530_2018" & 
             dat$key!="Charley_2017" & dat$key!="Charley_2018" & dat$key!="Daisy2_2017" & dat$key!="Daisy2_2018"
           & dat$key!="DeltaDoc_2017" & dat$key!="DWFLeslie_2017" & dat$key!="Frank_2016" & dat$key!="LM03F_2017" 
           & dat$key!="LM16F_2017" & dat$key!="LM29F_2018" & dat$key!="LM30F_2018" & dat$key!="LPI2_2016" 
           & dat$key!="LPI2_2017" & dat$key!="Quint_2016" & dat$key!="RP01F_2016" & dat$key!="RP02F_2016"
           & dat$key!="RP04F_2016" & dat$key!="RP04F_2017" & dat$key!="RP06F_2017" & dat$key!="RP10F_2017"
           & dat$key!="RP12F_2017" & dat$key!="RP15F_2018" & dat$key!="X4T_2017" & dat$key!="RP04F_2017" 
           & dat$key!="2176_2013" & dat$key!="2167_2013" & dat$key!="2164_2013" & dat$key!="X9Z_2017"
           & dat$animal_id!="LM31F",]

dat$timestamp <- as.POSIXct(dat$timestamp, tz="UTC")
dat$date <- as.Date(dat$date)

dat <- dat[,c(1:2,4,6:8,13,14)]

# Set up missing dates
un.id <- unique(dat$animal_id)

dat1 <- data.frame()
for (i in 1:length(un.id)) {

  temp <- dat[dat$animal_id==un.id[i],]
  dr <- range(temp$date)
  dse <- seq(as.Date(dr[1]),as.Date(dr[2]), "1 day")
  dats <- data.frame(date=dse)
  temp <- left_join(dats, temp, by="date")
  temp[is.na(temp$key),c(2,3,7,8)] <- temp[1,c(2,3,7,8)]

  temp[is.na(temp$latitude),4] <- paste0(temp$date[is.na(temp$latitude)], " 16:00:05")
  dat1 <- rbind(dat1, temp)

}
dat <- dat1
rm(dat1)

dat$pop <- "GRLD"
dat$pop[str_detect(dat$animal_id, "[A-Z]")] <- "NAMC"

dat$missing <- ifelse(is.na(dat$latitude),"y","n")

m <- dat[is.na(dat$latitude),]
unique(m$animal_id)

un.id <- unique(dat$animal_id)

# par(mfrow=c(3,5))
for (i in 1:length(un.id)) {
  t <- subset(dat, animal_id==un.id[i])

  nmiss <- sum(is.na(t$latitude))
  print(paste(un.id[i], " ", nmiss))
  
  if (nmiss > 0) {
    t <- t[t$missing=="n",]
    
    date_range <- seq(min(t$date), max(t$date), by=1)
    dates <- data.frame(date=date_range[!date_range %in% t$date], time=NA)
    
    dates$time <- ifelse(t$pop[1]=="GRLD", "16:00:00", "21:00:00")
    dates <- unite(dates, "timestamp", 1:2, sep=" ", remove=FALSE)
    miss <- as.POSIXct(dates$timestamp, tz="UTC")
   
    # Create move object
    tmove <- move(t$longitude, t$latitude, time=as.POSIXct(t$timestamp, tz="UTC"), data=t, proj=CRS("+proj=longlat +ellps=WGS84"), animal=t$animal_id[1])

    interp <- interpolateTime(tmove, time=miss, spaceMethod='greatcircle')
    plot(dat$longitude[dat$animal_id==un.id[i]], dat$latitude[dat$animal_id==un.id[i]], type="b", pch=16, 
         ylab="Latitude", xlab="Longitude", main=t$animal_id[1])
    points(interp, col="red", pch=17)
    legend("bottomleft", c("GPS points", "Interpolated"), pch=c(16,17), col=c("black", "red"), bty="n")

    dat[dat$animal_id==un.id[i] & is.na(dat$latitude),"latitude"] <- interp@coords[,2]
    dat[dat$animal_id==un.id[i] & is.na(dat$longitude),"longitude"] <- interp@coords[,1]
    dat[dat$animal_id==un.id[i] & is.na(dat$date),"date"] <- dates$date
    # dat[dat$animal_id==un.id[i] & is.na(dat$time),20] <- dates$time
    dat[dat$animal_id==un.id[i] & is.na(dat$timestamp),"timestamp"] <- as.POSIXct(dates$timestamp, tz="UTC")
  }
}


dat$julian <- as.numeric(format(dat$date, "%j"))

ggplot(dat, aes(x=julian, y=animal_id, shape=missing, color=missing)) + geom_point()
# write.csv(dat, "output/interp-gps-data_full.csv")

# Plot 


# Checking dates of end of "arrival" period
# mjd <- a <- numeric()
# for(i in 1:length(un.id)) {
#   temp <- subset(dat, animal_id==un.id[i])
#   mjd[i] <- max(temp$julian)
#   a[i] <- unique(temp$attempt)
# }
# 
# df <- data.frame(id=un.id, max.day=mjd, attempt=a)


