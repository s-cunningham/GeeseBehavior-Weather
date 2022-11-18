## Indicating where geese are in migration
## 2022-08-24
## Author: S. Cunningham

library(tidyverse)
library(sf)

## Read in staging areas shapefiles
# North America - Prairie Pothole Region
ppr <- st_read("data/shapefiles", "gmannppr")
ppr <- st_transform(ppr, "+proj=aeqd +lat_0=55 +lon_0=-60 +datum=WGS84")
ppr$NAME_0 <- "PPR"
ppr <- ppr[,"NAME_0"]

# Greenland - Iceland
isl <- st_read("data/shapefiles", "ISL_adm0")
isl <- st_transform(isl, "+proj=aeqd +lat_0=55 +lon_0=-60 +datum=WGS84")
isl <- isl[,"NAME_0"]

## Read in wintering areas
# North America - Texas
texas <- st_read("data/shapefiles", "NAMCwinter_ne10m")
texas <- st_transform(texas, "+proj=aeqd +lat_0=55 +lon_0=-60 +datum=WGS84")
texas$NAME_0 <- "Texas"
texas <- texas[,"NAME_0"]

# Greenland - Ireland/Scotland
irescot <- st_read("data/shapefiles", "IrelandUK_ne10m")
irescot <- st_transform(irescot, "+proj=aeqd +lat_0=55 +lon_0=-60 +datum=WGS84")
irescot <- irescot[,"ADMIN"]
names(irescot)[1] <- "NAME_0"

## Read goose location data (one point per day
dat <- read.csv("files_for_models/daily_odba_behavior.csv")
dat <- dat[,c(1,3:6,10)]

# List of unique bird IDs
un.id <- unique(dat$animal_id)

# Set up empty data frame
mig_dates <- data.frame()

# Loop over individuals 
for (i in 1:length(un.id)) {
  
  # Subset by individual
  temp <- dat[dat$animal_id==un.id[i],]
  
  # Determine which population
  pop <- temp$pop[1]
  
  # Convert from data frame to sf
  projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"
  temp_sf <- st_as_sf(x=temp, coords=c("longitude", "latitude"), crs=projcrs)
  temp_sf <- st_transform(temp_sf, "+proj=aeqd +lat_0=55 +lon_0=-60 +datum=WGS84")
  plot(st_geometry(temp_sf))
  
  ## Staging Area
  if (pop=="GRLD") {
    # Spatial join
    pts_stage <- st_join(temp_sf, isl, join=st_within)
  } else {
    # Spatial join
    pts_stage <- st_join(temp_sf, ppr, join=st_within)
  }
  
  # Convert to data frame
  pts_stage <- st_drop_geometry(pts_stage)
  pts_stage <- pts_stage[!is.na(pts_stage$NAME_0),]
  
  # Extract date last at staging area, add 14
  end <- max(pts_stage$julian) + 14
  
  ## Wintering areas
  if (pop=="GRLD") {
    # Spatial join
    pts_stage <- st_join(temp_sf, irescot, join=st_within)
  } else {
    # Spatial join
    pts_stage <- st_join(temp_sf, texas, join=st_within)
  }
  
  # Convert to data frame
  pts_stage <- st_drop_geometry(pts_stage)
  pts_stage <- pts_stage[!is.na(pts_stage$NAME_0),]
  
  # What is last day on wintering areas?
  lwint <- max(pts_stage$julian)
  
  # what is earliest day on wintering areas?
  ewint <- min(pts_stage$julian)
  
  # how long on winter areas?
  wint <- lwint - ewint
  
  if (wint > 14) {
    pts_stage <- pts_stage[pts_stage$julian>=(lwint-14),]
    print(un.id[i])
  }
  
  # start date
  start <- min(pts_stage$julian)
  
  ## Combine into data frame
  new <- data.frame(animal_id=temp$animal_id[1], start=start, end=end)
  
  # add row to existing data
  mig_dates <- rbind(mig_dates, new)
}

mig_dates$start[mig_dates$animal_id=="RP01F"] <- 59

# Calculate duration
mig_dates <- mutate(mig_dates, duration=end-start)

# Add one to be inclusive of start and end dates
mig_dates$duration <- mig_dates$duration + 1

# Write to file
write_csv(mig_dates, "files_for_models/migration_dates.csv")


