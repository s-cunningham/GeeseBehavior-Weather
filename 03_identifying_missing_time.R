#**********************************************************************************************************************************
#**********************************************************************************************************************************

# Project: 
# Date: 
# Author: Stephanie Cunningham
# Description: 

#**********************************************************************************************************************************
#**********************************************************************************************************************************

library(tidyverse)

file.list <- list.files(path="data/CSV_1ppd/", pattern=".csv", all.files=TRUE, full.names=TRUE)
file.list <- lapply(file.list, FUN=read.csv, header=TRUE, stringsAsFactors=FALSE)
files <- do.call("rbind", file.list)

files <- files[,-1]
files <- files[files$key!="502_2018" & files$key!="523_2018" & files$key!="527_2018" & files$key!="530_2018" & 
                 files$key!="Charley_2017" & files$key!="Charley_2018" & files$key!="Daisy2_2017" & files$key!="Daisy2_2018"
               & files$key!="DeltaDoc_2017" & files$key!="DWFLeslie_2017" & files$key!="Frank_2016" & files$key!="LM03F_2017" 
               & files$key!="LM16F_2017" & files$key!="LM29F_2018" & files$key!="LM30F_2018" & files$key!="LPI2_2016" 
               & files$key!="LPI2_2017" & files$key!="Quint_2016" & files$key!="RP01F_2016" & files$key!="RP02F_2016"
               & files$key!="RP04F_2016" & files$key!="RP04F_2017" & files$key!="RP06F_2017" & files$key!="RP10F_2017"
               & files$key!="RP12F_2017" & files$key!="RP15F_2018" & files$key!="X4T_2017" & files$key!="RP04F_2017" 
               & files$key!="2176_2013" & files$key!="2167_2013" & files$key!="2164_2013" & files$key!="X9Z_2017",]

files$julian <- as.numeric(format(as.Date(files$date), "%j"))
files$birdno <- as.numeric(factor(files$key, labels=seq(1,37,1)))

ids <- data.frame(num=files$birdno, id=files$animal_id)
ids <- distinct(ids)

ggplot(files, aes(x=julian, y=birdno, group=animal_id)) + geom_point(shape=15) + theme_bw()

files$date <- as.Date(files$date, format="%Y-%m-%d")

un.id <- unique(files$animal_id)
dat <- data.frame()
for (i in 1:length(un.id)) {
  temp <- files[files$animal_id==un.id[i],]
  N.order <- order(temp$date, decreasing=FALSE)
  temp <- temp[N.order,]
  temp <- mutate(temp,lagtime=julian-lag(julian))
  print(paste(unique(temp$animal_id), " ",  unique(temp$lagtime)))
  dr <- range(temp$date)
  ds <- data.frame(date=seq(dr[1], dr[2], by="day"))
  t2 <- full_join(ds, temp, by="date")
  t2$key[is.na(t2$key)] <- t2$key[1]
  t2$animal_id[is.na(t2$animal_id)] <- t2$animal_id[1]
  t2$birdno[is.na(t2$birdno)] <- t2$birdno[1]
  t2$sex[is.na(t2$sex)] <- t2$sex[1]
  t2$year[is.na(t2$year)] <- t2$year[1]
  t2$dutycycle[is.na(t2$dutycycle)] <- t2$dutycycle[1]
  t2 <- t2[,-18]
  t2$julian <- as.numeric(format(t2$date, "%j"))
  dat <- rbind(dat, t2)
}

dat$missing <- ifelse(is.na(dat$timestamp), "y", "n")

ggplot(dat, aes(x=julian, y=animal_id, group=animal_id, shape=factor(missing))) + geom_point() + theme_bw() +
  scale_shape_manual(values=c(3,16)) + geom_vline(xintercept=196)

# ODBA/PTF
ptf <- read.csv("output/daily-odba-behavior_subset.csv", stringsAsFactors=FALSE)
ptf <- ptf[,-1]
ptf$date <- as.Date(ptf$date)
names(ptf)[1] <- "animal_id"

ggplot(ptf, aes(x=rel.day, y=id_ind)) + geom_point()

dat.dw <- left_join(ptf, dat, by=c("animal_id", "date"))

# clean up columns
dat.dw <- dat.dw[,c(1:23,33,36)]
names(dat.dw)[c(4,5,12)] <- c("year","julian", "sex")

dat.dw <- dat.dw[!is.na(dat.dw$key),]

ggplot(dat.dw, aes(x=rel.day, y=animal_id, color=missing)) + geom_point()

write.csv(dat.dw, "output/combined_gps_acc_subset.csv")
