## Determining reproductive attempts or deferrals
## 2022-08-18
## Author: S. Cunningham

library(tidyverse)
library(sp)

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
  plot(as.Date(temp$date), temp$dld, type="l", main=un.id[i], ylim=c(0, 200))
  abline(h=25.4, col="red")
  abline(h=89.4, col="red", lty=3)
}




