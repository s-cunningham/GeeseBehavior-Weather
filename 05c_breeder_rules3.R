library(tidyverse)
library(adehabitatLT)
library(cowplot)
library(pracma)
library(zoo)
library(grid)
library(gridExtra)


theme_set(theme_test())

dat <-read.csv("output/dlm_emm_data3.csv", stringsAsFactors=FALSE)
dat <- dat[,-1]
names(dat)[1] <- "animal_id"
dat$tag[dat$animal_id=="2160"] <- "EOBS"
dat <- dat[dat$animal_id!="LM31F",]
dat$date <- as.Date(dat$date)

un.id <- unique(dat$id)
for (i in 1:length(un.id)) {
  temp <- dat[dat$id==un.id[i],]
  print(paste0(range(temp$date)," ", temp$id[1]))
}

# Read in GPS data
gps <- read.csv("output/interp-gps-data_full.csv")
gps <- gps[,-1]

gps$date <- as.Date(gps$date)

dat <- left_join(gps, dat, by=c("animal_id","date"))

dat <- dat[,c(2,3,12,1,4:9,11,15,19,24)]

names(dat)[c(8:11,14)] <- c("sex", "year", "pop", "julian", "odba")

dat$timestamp <- as.POSIXct(dat$timestamp)

## Subset to just eobs
dat <- subset(dat, tag=="EOBS")

# Create spatial points data frame to then crate ltraj object for displacement calculation
xy <- dat[,c(7,6)]
spdf <- SpatialPointsDataFrame(coords=xy, data=dat, proj4string=CRS("+proj=longlat +datum=WGS84"))
spdf <- spTransform(spdf, CRS("+proj=aeqd +lat_0=55 +lon_0=-60"))

traj <- as.ltraj(coordinates(spdf), date=spdf$timestamp, id=spdf$animal_id, burst=spdf$key, typeII=TRUE)

# Convert back to data frame
dst <- ld(traj)
dst$julian <- as.numeric(format(dst$date, "%j"))

names(dst)[11] <- "animal_id"

dat <- left_join(dat, dst, by=c("animal_id", "julian"))
dat <- dat[,c(1:14,20)]
names(dat)[4] <- "date"

dat$birdno <- as.numeric(factor(dat$key, labels=seq(1,15,1)))

## subset to breeding period
dat <- dat[dat$julian>=136 & dat$julian<=212 & dat$longitude<= -47,]

dat$pop <- "GRLD"

un.id <- unique(dat$animal_id)

m <- data.frame(id=un.id, odba=rep(NA, length(un.id)), 
                odba.min=rep(NA, length(un.id)), 
                dist=rep(NA, length(un.id)),
                dist.min=rep(NA, length(un.id)))

for (i in 1:length(un.id)) {
  
  temp <- dat[dat$animal_id==un.id[i],]
  m[i,2] <- mean(temp$odba)
  m[i,4] <- mean(temp$dist)
  
  x <- median(temp$odba)
  x2 <- mean(temp$odba)
  d <- median(temp$dist, na.rm=TRUE)
  
  h <- which(temp$odba<x)
  g <- which(temp$dist<d)

  # plot(temp$odba, main=un.id[i])
  # abline(h=x, col="red")
  # abline(v=h[2])
  # abline(v=h[2]+30)
  
  # plot(temp$julian, temp$dist, main=un.id[i])
  # abline(h=d, col="red")
  # abline(v=min(temp$julian)+h[2])
  # abline(v=min(temp$julian)+h[2]+30)
}







