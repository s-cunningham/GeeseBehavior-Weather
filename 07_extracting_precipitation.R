library(ncdf4)
library(raster)
library(tidyverse)

# List files
files <- list.files(path="data/gridded_precip/", pattern=".nc", all.files=TRUE, full.names=TRUE)

## Read in goose data
dat <- read_csv("files_for_models/daily_odba_behavior.csv")

# change longitude so that it matches rasters (all positive instead of E/W)
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
    xy <- temp[,c(6,5)]
    spdf <- SpatialPointsDataFrame(coords=xy, data=temp, proj4string=CRS("+proj=longlat +datum=WGS84"))
    
    # Extract from raster
    w <- raster::extract(b, spdf, method='bilinear', fun='mean', df=TRUE)
    
    spdf$prcp <- w[,2]
    temp <- as.data.frame(spdf)
    
    prcp <- bind_rows(prcp, temp)
  }
 print(i)
}

prcp <- prcp[,c(1,3,19)]

# Plot histogram of precipitation
hist(prcp$prcp)
range(prcp$prcp)

prcp$prcp[prcp$prcp==-99999] <- NA

# Write data
write_csv(prcp, "files_for_models/daily_precip.csv")


