## Determining reproductive attempts or deferrals
## 2022-08-18
## Author: S. Cunningham

library(tidyverse)
library(sp)
library(adehabitatLT)

#### Point-specific GPS locations ####

## Ornitela: Read files and summarize
orn.files <- list.files(path="data/GPSdata/Ornitela/", pattern=".csv", all.files=TRUE, full.names=TRUE)
orn <- lapply(orn.files, FUN=read.csv, header=TRUE, stringsAsFactors=FALSE)

for (i in 1:length(orn)) {
  
  temp <- orn[[i]]
  temp <- temp[,-c(1,3)]

  # Remove missing points
  temp <- temp[temp$Latitude>0,]
  
  # Change time zone for North America
  if (temp$device_id[1]==17701 | temp$device_id[1]==17709 | temp$device_id[1]==17719 |
      temp$device_id[1]==17744 | temp$device_id[1]==17828 | temp$device_id[1]==17829) {
    
    # Timestamp
    temp <- unite(temp, "timestamp", 2:3, sep=" ", remove=TRUE)
    
    # Convert to Central Time
    temp$timestamp <- as.POSIXct(temp$timestamp, tz="UTC", format="%Y-%m-%d %H:%M:%S")
    temp$timestamp <- format(temp$timestamp, tz="America/Chicago", usetz=TRUE)
    
    # pull out dates and times
    temp$timestamp <- as.character(temp$timestamp)
    tdmat <- as.data.frame(matrix(unlist(strsplit(temp$timestamp, " ")), ncol=3, byrow=TRUE))[,1:2] 
    names(tdmat) <- c("date", "time")
    
    temp <- cbind(temp, tdmat)
    temp <- temp[,c(1,15,16,9,10)]
    
  } else {
    names(temp)[2:3] <- c("date", "time")
    
    temp <- temp[,c(1:3,10,11)]
  }
  
  # Subset to ~31 July
  temp$julian <- as.numeric(format(as.Date(temp$date), "%j"))
  temp <- temp[temp$julian>=121 & temp$julian<=212,]
  
  # Convert to equidistant projection
  xy <- temp[,c(5,4)]
  spdf <- SpatialPointsDataFrame(coords=xy, data=temp, proj4string=CRS("+proj=longlat +datum=WGS84"))
  spdf <- spTransform(spdf, CRS("+proj=aeqd +lat_0=90 +lon_0=0"))
  temp <- data.frame(spdf)
  names(temp)[7:8] <- c("x_coord", "y_coord")
  
  # Calculate median latitude and longitude
  medY <- temp %>% group_by(date) %>% summarise(medlat=median(y_coord))
  
  # Join median y coord
  temp <- left_join(temp, medY, by="date")
  
  # Store in the list
  orn[[i]] <- temp 
}

# Convert list to data frame
orn <- do.call(rbind, orn)

# Rename Jay's birds
orn$device_id <- as.character(orn$device_id)
orn$device_id[orn$device_id=="17701"] <- "RP20F"
orn$device_id[orn$device_id=="17709"] <- "RP19F"
orn$device_id[orn$device_id=="17719"] <- "RP17F"
orn$device_id[orn$device_id=="17744"] <- "LM39F"
orn$device_id[orn$device_id=="17828"] <- "RP23F"
orn$device_id[orn$device_id=="17829"] <- "RP22F"

## CTT: Read files and summarize
ctt.files <- list.files(path="data/GPSdata/CTT/", pattern=".csv", all.files=TRUE, full.names=TRUE)
ctt <- lapply(ctt.files, FUN=read.csv, header=TRUE, stringsAsFactors=FALSE)

for (i in 1:length(ctt)) {
  
  temp <- ctt[[i]]
  temp <- temp[,c(22, 4:6)]
  
  # Timestamp
  names(temp) <- c("device_id", "timestamp", "Latitude", "Longitude")
  temp$device_id <- gsub("_", "", temp$device_id)
  
  # Convert to Central Time
  temp$timestamp <- as.POSIXct(temp$timestamp, tz="UTC", format="%Y-%m-%d %H:%M:%S")
  temp$timestamp <- format(temp$timestamp, tz="America/Chicago", usetz=TRUE)
  
  N.order <- order(temp$timestamp, decreasing=FALSE)
  temp <- temp[N.order,]
  
  # pull out dates and times
  temp$timestamp <- as.character(temp$timestamp)
  tdmat <- as.data.frame(matrix(unlist(strsplit(temp$timestamp, " ")), ncol=3, byrow=TRUE))[,1:2] 
  names(tdmat) <- c("date", "time")
  
  temp <- cbind(temp, tdmat)
  temp <- temp[,c(1,5,6,3,4)]
  
  # Subset to ~31 July
  temp$julian <- as.numeric(format(as.Date(temp$date), "%j"))
  temp <- temp[temp$julian>=121 & temp$julian<=212,]
  
  # Convert to equidistant projection
  xy <- temp[,c(5,4)]
  spdf <- SpatialPointsDataFrame(coords=xy, data=temp, proj4string=CRS("+proj=longlat +datum=WGS84"))
  spdf <- spTransform(spdf, CRS("+proj=aeqd +lat_0=90 +lon_0=0"))
  temp <- data.frame(spdf)
  names(temp)[7:8] <- c("x_coord", "y_coord")
  
  # Calculate median latitude and longitude
  medY <- temp %>% group_by(date) %>% summarise(medlat=median(y_coord))
  
  # Join median y coord
  temp <- left_join(temp, medY, by="date")
  
  # Store in the list
  ctt[[i]] <- temp 
}

# Convert list to data frame
ctt <- do.call(rbind, ctt)

gps <- rbind(orn, ctt)
names(gps)[1] <- "animal_id"

gps <- gps[,-9]

# Create year column and remove individuals that have data in >1 year
gps$year <- format(as.Date(gps$date), "%Y")
gps <- unite(gps, "key", c(1,10), sep="_", remove=FALSE)

gps <- gps[gps$key!="RP01F_2016" & gps$key!="RP15F_2018",]

# Calculate difference from each point to the median point
gps <- mutate(gps, latdiff=abs(y_coord-medlat))
sdLat <- gps %>% group_by(animal_id, date) %>% summarize(dld=sd(latdiff))

# Plot difference from daily median point
un.id <- unique(sdLat$animal_id)
for (i in 1:length(un.id)) {
  temp <- sdLat[sdLat$animal_id==un.id[i],]
  plot(as.Date(temp$date), temp$dld, type="l", pch=16, main=un.id[i], ylim=c(0, 2500))
  abline(h=25.4, col="red")
  # abline(h=89.4, col="red", lty=3)
}

## Examine daily median ODBA ##

# Read in summarized ACC data
acc <- read.csv("files_for_models/daily_odba_behavior.csv")

# get rid of extra columns
acc <- acc[,c(1:4, 18)]

# subset to May-July
acc <- acc[acc$julian>=121 & acc$julian<=212,]

# Plot median ODBA
un.id <- unique(acc$animal_id)
for (i in 1:length(un.id)) {
  temp <- acc[acc$animal_id==un.id[i],]
  plot(as.Date(temp$date), temp$median.odba, type="l", pch=16, main=un.id[i], ylim=c(0, 1))
  # abline(h=25.4, col="red")
}

# Join ACC and latitude SD
sdLat <- left_join(sdLat, acc, by=c("animal_id", "date"))
sdLat <- sdLat[,c(1,4,2,5,3,6)]
names(sdLat)[5] <- "sdDIST"

# Log-transform ODBA and SD of latitude
sdLat$lnSDdist <- log(sdLat$sdDIST)
sdLat$lnOBDA <- log(sdLat$median.odba)

# Replicate Fig. 4 from Shreven et al. 2021
un.id <- unique(sdLat$animal_id)
for (i in 1:length(un.id)) {
  temp <- sdLat[sdLat$animal_id==un.id[i],]
  p <- ggplot(temp, aes(x=lnSDdist, y=lnOBDA, col=julian)) + geom_point(size=3) +
           ggtitle(un.id[i]) +
    coord_cartesian(xlim=c(-1, 15), ylim=c(-4.5, 0)) + theme_bw()
  print(p)
}

ggplot(sdLat, aes(x=lnSDdist, y=lnOBDA, color=animal_id)) + geom_point() +
  coord_cartesian(xlim=c(-1, 15)) + theme_bw()


#### e-obs geese ####

acc <- read.csv("files_for_models/daily_odba_behavior.csv")

# what happens if compare OBDA to distance from previous day's location?
acc <- acc[acc$year==2012 | acc$year==2013,]
acc <- acc[,c(1:6,18)]

un.id <- unique(acc$animal_id)
eobs <- data.frame()

for (i in 1:length(un.id)) {
  
  temp <- acc[acc$animal_id==un.id[i],]
  
  xy <- temp[,c(6,5)]
  spdf <- SpatialPointsDataFrame(coords=xy, data=temp, proj4string=CRS("+proj=longlat +datum=WGS84"))
  spdf <- spTransform(spdf, CRS("+proj=aeqd +lat_0=90 +lon_0=0"))
  
  spdf$date <- as.POSIXct(spdf$date)
  
  # Create ltraj object
  rawtraj <- as.ltraj(coordinates(spdf), date=spdf$date, id=spdf$animal_id, burst=spdf$key, typeII=TRUE)
  
  # Check if locations are regularly spaced
  is.regular(rawtraj)
  
  # Regularize trajectories, round values
  refda <- min(spdf$date)
  tag <- setNA(rawtraj, refda,1,units="day")
  ttraj <- sett0(tag, refda,1,units="day")
  ttraj <- mindistkeep(ttraj, 5)
  
  tag <- ld(ttraj)
  tag$date <- as.character(tag$date)
  
  tag <- tag[,c(3,1,2,4:6)]
  names(tag)[2:3] <- c("x_coord", "y_coord")
  
  tag$date <- gsub(" 00:00:00", "", tag$date)
  
  temp <- left_join(temp, tag, by="date")
  temp <- temp[temp$julian>=121 & temp$julian<=212,]
  
  eobs <- rbind(eobs, temp)
  
}

# Are there missing locations
eobs[is.na(eobs$dy),]

# Log-transform ODBA and difference in latitude
eobs$lnDISTy <- log(abs(eobs$dy))
eobs$lnOBDA <- log(eobs$median.odba)

un.id <- unique(eobs$animal_id)
for (i in 1:length(un.id)) {
  temp <- eobs[eobs$animal_id==un.id[i],]
  p <- ggplot(temp, aes(x=lnSDdist, y=lnOBDA, col=julian)) + geom_point(size=3) +
    ggtitle(un.id[i]) +
    coord_cartesian(xlim=c(-1, 15), ylim=c(-4.5, 0)) + theme_bw()
  print(p)
}
