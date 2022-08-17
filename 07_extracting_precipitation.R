library(ncdf4)
library(raster)
library(tidyverse)

# List files
files <- list.files(path="data/gridded_precip/", pattern=".nc", all.files=TRUE, full.names=TRUE)

## Read in goose data
dat <- read.csv("output/dlm_emm_data4.csv")
dat <- dat[,-1]
names(dat)[1] <- "animal_id"

## Read in GPS data
gps <- read.csv("output/interp-gps-data_full.csv")
gps <- gps[,c(4,2,6,7)]

## Join GPS points to data
dat <- left_join(dat, gps, by=c("animal_id", "date"))
dat <- dat[!is.na(dat$latitude),]

# change longitude
dat$longitude <- ifelse(dat$longitude<0, dat$longitude+360, dat$longitude)

# set up data
prcp <- data.frame()

for (i in 1:length(files)) {
  
  # Read precip raster
  b <- brick(files[i])
  
  # Date
  d <- names(b)
  d <- gsub("X", "", d)
  d <- gsub("[[:punct:]]", "-", d)
  
  # subset goose data
  temp <- dat[dat$date==d,]
  
  if (nrow(temp)>0) {
    # convert to spdf
    xy <- temp[,c(22,21)]
    spdf <- SpatialPointsDataFrame(coords=xy, data=temp, proj4string=CRS("+proj=longlat +datum=WGS84"))
    
    # Extract from raster
    w <- raster::extract(b, spdf, method='bilinear', fun='mean', df=TRUE)
    
    spdf$prcp <- w[,2]
    temp <- as.data.frame(spdf)
    
    prcp <- bind_rows(prcp, temp)
  }
 print(i)
}

prcp <- prcp[,c(1,3,23)]

dat <- left_join(dat, prcp, by=c("animal_id", "date"))

dat <- dat[!is.na(dat$prcp),]

write_csv(dat, "output/dlm_emm_data5.csv")


