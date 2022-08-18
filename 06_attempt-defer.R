## Determining reproductive attempts or deferrals
## 2022-08-18
## Author: S. Cunningham

library(tidyverse)




#### Point-specific GPS locations ####

# Read files
orn.files <- list.files(path="data/GPSdata/Ornitela/", pattern=".csv", all.files=TRUE, full.names=TRUE)
orn <- lapply(orn.files, FUN=read.csv, header=TRUE, stringsAsFactors=FALSE)

fixes <- data.frame()

for (i in 1:length(orn)) {
  
  temp <- orn[[i]]
  temp <- temp[,-1]
  
  temp$UTC_datetime <- as.POSIXct(temp$UTC_datetime, format="%Y-%m-%d %H:%M:%S")
  
  temp$prevTS <- c(temp$UTC_datetime[1], temp$UTC_datetime[1:nrow(temp)-1])
  temp$prevTS <- as.POSIXct(temp$prevTS, format="%Y-%m-%d %H:%M:%S")
  temp <- temp[,c(1,2,17,3:16)]
  
  # timestamp column NA for some? But there is a date and time
  temp$UTC_datetime[is.na(temp$UTC_datetime)] <- paste0(temp$UTC_date[is.na(temp$UTC_datetime)], " ", temp$UTC_time[is.na(temp$UTC_datetime)])
  
  # convert to posix
  temp$UTC_datetime <- as.POSIXct(temp$UTC_datetime, format="%Y-%m-%d %H:%M:%S")
  temp$prevTS <- as.POSIXct(temp$prevTS, format="%Y-%m-%d %H:%M:%S")
  temp <- mutate(temp, fixdiff=UTC_datetime-prevTS)
  
  temp[1829:1831,]
  
  mean(temp$fixdiff, na.rm=TRUE)
  sd(temp$fixdiff, na.rm=TRUE)
  
}


