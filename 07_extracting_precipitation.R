library(ncdf4)
library(raster)
library(tidyverse)
library(forcast)

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

### For day with missing data, subtract from the monthly value

missing_id <- prcp[is.na(prcp$prcp),"animal_id"]

# Subset file list to only April 2013
files <- files[grep("201304", files)]

for (i in 1:length(missing_id)) {
  
  month <- data.frame(date=seq.Date(as.Date("2013-04-01"), as.Date("2013-04-30"), by="day"),
                      prcp=rep(NA,30))
  
  # subset goose data
  temp <- dat[dat$animal_id==missing_id[i],]
  temp <- temp[temp$date==as.Date("2013-04-06"),] %>% as.data.frame()
  
  # convert to spdf
  xy <- temp[,c(6,5)]
  spdf <- SpatialPointsDataFrame(coords=xy, data=temp, proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Loop over days
  for (j in 1:length(files)) {
    
    b <- brick(files[j])
    
    # Extract from raster
    w <- raster::extract(b, spdf, method='bilinear', fun='mean', df=TRUE)
    
    month[j,2] <- w[1,2]
    
  }
  
  month$prcp[month$prcp<0] <- NA
  
  # Read in monthly file
  m <- brick("data/gridded_precip/monthly/gpcp_v02r03_monthly_d201304_c20170616.nc")
  
  # Extract from raster
  w <- raster::extract(m, spdf, method='bilinear', fun='mean', df=TRUE)
  
  
  
}




plot(b)
points(temp$longitude, temp$latitude, pch=16)
unique(b)








# Write data
write_csv(prcp, "files_for_models/daily_precip.csv")