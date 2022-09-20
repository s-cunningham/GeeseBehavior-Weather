
# Author: Stephanie Cunningham
# Filtering GPS data to 1 point per day


library(tidyverse)

# Read in file that has tags NA data (JVB CTT & Orn + PL CTT)
tags <- read.csv("data/combined_NA_gps.csv", stringsAsFactors=FALSE)
tags <- tags[,-c(1)]

tags$date <- as.Date(tags$date, format="%m/%d/%Y")
tags$timestamp <- as.POSIXct(tags$timestamp, tz="GMT")

tags$tt <- "21:00:00"
tags <- unite(tags, "targetdt", c(3,15), sep=" ", remove=FALSE)

tags$targetdt <- as.POSIXct(tags$targetdt, tz="UTC")

tags <- mutate(tags, timedist=abs(timestamp-targetdt))
tags$timedist <- as.numeric(tags$timedist)

test <- tags %>% group_by(animal_id, date) %>% filter(row_number()==which.min(timedist))
test <- as.data.frame(test)
np <- test %>% group_by(key,date) %>% count()
range(np$n)

un.id <- unique(test$key)


## Write subset to file
# for (i in 1:length(un.id)) {
#   temp <- subset(test, key==un.id[i])
#   k <- temp$key[1]
#   filename <- paste0("output/CSV_1ppd/",  k, ".csv")
#   write.csv(temp, filename)
# }

# Read in file that has all GR data (SC CTT & Orn + MW e-obs)
tags <- read.csv("data/combined_GR_gps.csv", stringsAsFactors=FALSE)
tags <- tags[,-c(1)]

tags$timestamp <- as.POSIXct(tags$timestamp, tz="GMT")

tags$tt <- "16:00:00"
tags <- unite(tags, "targetdt", c(3,15), sep=" ", remove=FALSE)

tags$targetdt <- as.POSIXct(tags$targetdt, tz="UTC")

tags <- mutate(tags, timedist=abs(timestamp-targetdt))
tags$timedist <- as.numeric(tags$timedist)

test <- tags %>% group_by(animal_id, date) %>% filter(row_number()==which.min(timedist))
test <- as.data.frame(test)
np <- test %>% group_by(key,date) %>% count()
range(np$n)

un.id <- unique(test$key)

## Write subset to file
# for (i in 1:length(un.id)) {
#   temp <- subset(test, key==un.id[i])
#   k <- temp$key[1]
#   filename <- paste0("output/CSV_1ppd/",  k, ".csv")
#   write.csv(temp, filename)
# }


