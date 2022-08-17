## Data prep for extracting weather data and running models
## 2022-08-17
## Author: S. Cunningham

library(tidyverse)
library(move)

#### ACC data ####

## CTT ##
## Read files
ctt.files <- list.files(path="output/classified/ctt/", pattern=".csv", all.files=TRUE, full.names=TRUE)
ctt <- lapply(ctt.files, FUN=read.csv, header=TRUE, stringsAsFactors=FALSE)

for (i in 1:length(ctt.files)) {
  
  # Subset to each bird
  temp <- ctt[[i]]
  temp <- temp[,-1]

  # Calculate summarized values of behavior and ODBA  
  mean.odba <- temp %>% group_by(date) %>% summarize(mean.odba=mean(odba))
  median.odba <- temp %>% group_by(date) %>% summarize(median.odba=median(odba))
  nbursts <- temp %>% group_by(date) %>% count()
  nbehavior <- temp %>% group_by(date, behavior) %>% count()
  nbehavior <- pivot_wider(nbehavior, names_from="behavior", values_from="n")
  nbursts <- left_join(nbehavior, nbursts, by="date")
  names(nbursts)[5] <- "nbursts"
  nbursts <- mutate(nbursts, ptf=graze/nbursts)
  
  nbursts <- left_join(nbursts, mean.odba, by="date")
  nbursts <- left_join(nbursts, median.odba, by="date")
  nbursts <- as.data.frame(nbursts)
  
  # Create a table for id, tag type, sex and population
  tag <- rep("CTT", nrow(nbursts))
  animal_id <- rep(temp$id[1], nrow(nbursts))
  id <- data.frame(animal_id=animal_id, tag=tag)

  # bind individual identification to daily summarized values
  nbursts <- bind_cols(id, nbursts)
  
  # Save back into the list
  ctt[[i]] <- nbursts
}

# Convert list to data frame
ctt <- do.call(rbind, ctt)
ctt$animal_id <- as.character(ctt$animal_id)

## Ornitela ##
# Read files
orn.files <- list.files(path="output/odba_map/Ornitela/", pattern=".csv", all.files=TRUE, full.names=TRUE)
orn <- lapply(orn.files, FUN=read.csv, header=TRUE, stringsAsFactors=FALSE)

for (i in 1:length(orn.files)) {
  
  # Subset to each bird
  temp <- orn[[i]]

  # Calculate summarized values of behavior and ODBA  
  mean.odba <- temp %>% group_by(date) %>% summarize(mean.odba=mean(odba.map))
  median.odba <- temp %>% group_by(date) %>% summarize(median.odba=median(odba.map))
  nbursts <- temp %>% group_by(date) %>% count()
  nbehavior <- temp %>% group_by(date, behavior) %>% count()
  nbehavior <- pivot_wider(nbehavior, names_from="behavior", values_from="n")
  nbursts <- left_join(nbehavior, nbursts, by="date")
  names(nbursts)[5] <- "nbursts"
  nbursts <- mutate(nbursts, ptf=graze/nbursts)
  
  nbursts <- left_join(nbursts, mean.odba, by="date")
  nbursts <- left_join(nbursts, median.odba, by="date")
  nbursts <- as.data.frame(nbursts)
  
  # Create a table for id, tag type, sex and population
  tag <- rep("ORN", nrow(nbursts))
  animal_id <- rep(temp$id[1], nrow(nbursts))
  id <- data.frame(animal_id=animal_id, tag=tag)

  # bind individual identification to daily summarized values
  nbursts <- bind_cols(id, nbursts)
  
  # Save back into the list
  orn[[i]] <- nbursts
}

# Convert list to data frame
orn <- do.call(rbind, orn)

# Rename Jay's birds
orn$animal_id <- as.character(orn$animal_id)
orn$animal_id[orn$animal_id=="17701"] <- "RP20F"
orn$animal_id[orn$animal_id=="17709"] <- "RP19F"
orn$animal_id[orn$animal_id=="17719"] <- "RP17F"
orn$animal_id[orn$animal_id=="17744"] <- "LM39F"
orn$animal_id[orn$animal_id=="17828"] <- "RP23F"
orn$animal_id[orn$animal_id=="17829"] <- "RP22F"

## e-obs ##
## Read files
eobs.files <- list.files(path="output/odba_map/eobs/", pattern=".csv", all.files=TRUE, full.names=TRUE)
eobs <- lapply(eobs.files, FUN=read.csv, header=TRUE, stringsAsFactors=FALSE)

for (i in 1:length(eobs.files)) {
  
  # Subset to each bird
  temp <- eobs[[i]]
  
  # Calculate summarized values of behavior and ODBA  
  mean.odba <- temp %>% group_by(date) %>% summarize(mean.odba=mean(odba.map))
  median.odba <- temp %>% group_by(date) %>% summarize(median.odba=median(odba.map))
  nbursts <- temp %>% group_by(date) %>% count()
  nbehavior <- temp %>% group_by(date, behavior) %>% count()
  nbehavior <- pivot_wider(nbehavior, names_from="behavior", values_from="n")
  nbursts <- left_join(nbehavior, nbursts, by="date")
  if (ncol(nbehavior)>4) {
    nbursts <- nbursts[, -which(names(nbursts) %in% c("biting tag"))]
  }
  names(nbursts)[c(2,5)] <- c("graze", "nbursts")
  nbursts <- mutate(nbursts, ptf=graze/nbursts)
  
  nbursts <- left_join(nbursts, mean.odba, by="date")
  nbursts <- left_join(nbursts, median.odba, by="date")
  nbursts <- as.data.frame(nbursts)
  
  # Create a table for id, tag type, sex and population
  tag <- rep("EOBS", nrow(nbursts))
  animal_id <- rep(temp$id[1], nrow(nbursts))
  id <- data.frame(animal_id=animal_id, tag=tag)

  # bind individual identification to daily summarized values
  nbursts <- bind_cols(id, nbursts)
  
  # Save back into the list
  eobs[[i]] <- nbursts
}

# Convert list to data frame
eobs <- do.call(rbind, eobs)
eobs$animal_id <- as.character(eobs$animal_id)

## Combine
acc <- rbind(ctt, orn, eobs)
acc$date <- as.Date(acc$date)

#### GPS data ####
gps.files <- list.files(path="data/CSV_1ppd/", pattern=".csv", all.files=TRUE, full.names=TRUE)
gps <- lapply(gps.files, FUN=read.csv, header=TRUE, stringsAsFactors=FALSE)

# Loop over each individual and interpolate missing data
for (i in 1:length(gps)) {
  
  # Subset to each individual
  temp <- gps[[i]]
  temp <- temp[,-1]
  temp$date <- as.Date(temp$date)
  temp <- temp[,c(1:2,4,6:8,13,14)]
  
  dr <- range(temp$date)
  dse <- seq(as.Date(dr[1]),as.Date(dr[2]), "1 day")
  dats <- data.frame(date=dse)
  temp <- left_join(dats, temp, by="date")
  temp[is.na(temp$key),c(2,3,7,8)] <- temp[1,c(2,3,7,8)]
  
  temp[is.na(temp$latitude),4] <- paste0(temp$date[is.na(temp$latitude)], " 16:00:05")

  # Determine how many locations are missing
  temp$missing <- ifelse(is.na(temp$latitude),"y","n")
  nmiss <- sum(is.na(temp$latitude))

  # Add populaiton 
  temp$pop <- "GRLD"
  temp$pop[str_detect(temp$animal_id, "[A-Z]")] <- "NAMC"
  
  # Interpolate location for missing day
  if (nmiss > 0) {
    temp2 <- temp[temp$missing=="n",]
    
    date_range <- seq(min(temp2$date), max(temp2$date), by=1)
    dates <- data.frame(date=date_range[!date_range %in% temp2$date], time=NA)
    
    dates$time <- ifelse(temp2$pop[1]=="GRLD", "16:00:00", "21:00:00")
    dates <- unite(dates, "timestamp", 1:2, sep=" ", remove=FALSE)
    miss <- as.POSIXct(dates$timestamp, tz="UTC")
    
    # Create move object
    tmove <- move(temp2$longitude, temp2$latitude, time=as.POSIXct(temp2$timestamp, tz="UTC"), 
                  data=temp2, proj=CRS("+proj=longlat +ellps=WGS84"), animal=temp2$animal_id[1])
    
    interp <- interpolateTime(tmove, time=miss, spaceMethod='greatcircle')

    temp[is.na(temp$latitude),"latitude"] <- interp@coords[,2]
    temp[is.na(temp$longitude),"longitude"] <- interp@coords[,1]
    temp[is.na(temp$date),"date"] <- dates$date
    temp[is.na(temp$timestamp),"timestamp"] <- as.POSIXct(dates$timestamp, tz="UTC")
  }

  gps[[i]] <- temp
}

gps <- do.call(rbind, gps)

# Save date as Julian day
gps$julian <- as.numeric(format(gps$date, "%j"))

# Plot for visual representation of missing points
ggplot(gps, aes(x=julian, y=animal_id, shape=missing, color=missing)) + geom_point() + theme_bw()

#### Join GPS and ACC data ####
dat <- left_join(gps, acc, by=c("animal_id", "date"))

# See where tag and population is missing
unique(dat$animal_id[is.na(dat$tag)])
dat$pop[dat$animal_id=="LM17M" | dat$animal_id=="LM31F" | dat$animal_id=="RP01F" |
          dat$animal_id=="RP08F" | dat$animal_id=="RP15F"] <- "NAMC"
dat$tag[dat$animal_id=="LM17M" | dat$animal_id=="LM31F" | dat$animal_id=="RP01F" |
          dat$animal_id=="RP08F" | dat$animal_id=="RP15F"] <- "CTT"
dat$pop[dat$animal_id=="17763"] <- "GRLD"
dat$tag[dat$animal_id=="17763"] <- "ORN"

# delete unneeded columns, reorganize
dat <- dat[,c(3,2,1,11,5:10,12:19)]

