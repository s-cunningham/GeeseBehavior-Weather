

library(tidyverse)
library(adehabitatLT)
library(cowplot)
library(pracma)
library(ggmap)

# rm(list=ls())

register_google(key="AIzaSyAmV2O_nHBQWzhd8uc8-s26nWimRktosRw")

dat1 <-read.csv("output/all_dailyodba.csv", stringsAsFactors=FALSE)
dat1 <- dat1[,-1]

dat1$date <- as.Date(dat1$date)

un.id <- unique(dat1$id)
for (i in 1:length(un.id)) {
  temp <- dat1[dat1$id==un.id[i],]
  print(paste0(range(temp$date)," ", temp$id[1]))
}

# Read in daily GPS data
setwd("data/CSV_1ppd")
file.list <- list.files(path="./", pattern=".csv", all.files=TRUE, full.names=FALSE)
file.list <- lapply(file.list, FUN=read.csv, header=TRUE, stringsAsFactors=FALSE)
gps <- do.call("rbind", file.list)
setwd("G:/ResearchProjects/Behavior_and_weather")

gps <- gps[,-1]
gps <- gps[gps$key!="502_2018" & gps$key!="523_2018" & gps$key!="527_2018" & gps$key!="530_2018" & 
                 gps$key!="Charley_2017" & gps$key!="Charley_2018" & gps$key!="Daisy2_2017" & gps$key!="Daisy2_2018"
               & gps$key!="DeltaDoc_2017" & gps$key!="DWFLeslie_2017" & gps$key!="Frank_2016" & gps$key!="LM03F_2017" 
               & gps$key!="LM16F_2017" & gps$key!="LM29F_2018" & gps$key!="LM30F_2018" & gps$key!="LPI2_2016" 
               & gps$key!="LPI2_2017" & gps$key!="Quint_2016" & gps$key!="RP01F_2016" & gps$key!="RP02F_2016"
               & gps$key!="RP04F_2016" & gps$key!="RP04F_2017" & gps$key!="RP06F_2017" & gps$key!="RP10F_2017"
               & gps$key!="RP12F_2017" & gps$key!="RP15F_2018" & gps$key!="X4T_2017" & gps$key!="RP04F_2017" 
               & gps$key!="2176_2013" & gps$key!="2167_2013" & gps$key!="2164_2013" & gps$key!="X9Z_2017",]

gps$julian <- as.numeric(format(as.Date(gps$date), "%j"))
gps$date <- as.Date(gps$date)

un.id <- unique(gps$animal_id)
par(mfrow=c(1,1))
for (i in 1:length(un.id)) {
  temp <- gps[gps$animal_id==un.id[i],]
  print(paste0(range(temp$date), " ", temp$animal_id[1]))
  plot(temp$julian, temp$latitude)
}

names(dat1)[1] <- "animal_id"
dat <- left_join(gps, dat1, by=c("animal_id","date"))

dat <- dat[,c(1,2,4,6:8,11:14,18:24)]
names(dat)[10:11] <- c("year", "julian")

un.id <- unique(dat$animal_id)
par(mfrow=c(1,1))
# for (i in 1:length(un.id)) {
#   temp <- dat[dat$animal_id==un.id[i],]
#   print(paste0(range(temp$date), " ", temp$animal_id[1]))
#   plot(temp$julian, temp$latitude)
# }

dat$timestamp <- as.POSIXct(dat$timestamp)

# Create spatial points data frame to then crate ltraj object for displacement calculation
xy <- dat[,c(6,5)]
spdf <- SpatialPointsDataFrame(coords=xy, data=dat, proj4string=CRS("+proj=longlat +datum=WGS84"))
spdf <- spTransform(spdf, CRS("+proj=aeqd +lat_0=55 +lon_0=-60"))


traj <- as.ltraj(coordinates(spdf), date=spdf$timestamp, id=spdf$animal_id, burst=spdf$key, typeII=TRUE)

refda <- min(dat$timestamp)
traj_NA <- setNA(traj, refda, 1,units="day")
traj <- sett0(traj_NA, refda,1,units="day")
traj <- mindistkeep(traj, 10)

# Convert back to data frame
dst <- ld(traj)
dst$julian <- as.numeric(format(dst$date, "%j"))

names(dst)[c(11,14)] <- c("animal_id", "julian")

dat <- left_join(dat, dst, by=c("animal_id", "julian"))
dat <- dat[,c(1:17,23,25)]
names(dat)[3] <- "date"

dat <- dat[dat$julian>=121 & dat$julian<=212,]

dat$pop <- "GRLD"
dat$pop[str_which(dat$animal_id, "[A-Z]")] <- "NAMC"

ggplot(dat, aes(y=dist, x=julian, group=key, color=pop)) + geom_line() +facet_grid(pop~., scales="free_y")
ggplot(dat, aes(y=odba, x=julian, group=key, color=pop)) + geom_line() +facet_grid(pop~., scales="free_y")

# Calculate rolling average of odba
un.id <- unique(dat$key)

for (i in 1:length(un.id)) {
  temp <- dat[dat$key==un.id[i],]
  temp <- temp[temp$julian>=154,]
  
  # s <- movavg(temp$dist, n=5, type="s")
  # w <- movavg(temp$dist, n=5, type="w")
  # r <- movavg(temp$dist, n=5, type="r")
  
  
  plot(temp$longitude, temp$latitude)
  
  
  # plot(temp$julian, temp$dist, type="l", col="gray70", main=un.id[i], ylim=c(0,10000))
  # abline(h=1000)
  # abline(v=c(121,152,182,213))
  # abline(v=c(163, 191), col="gray60", lty=2)
  # lines(temp$julian, s, col="red")
  # lines(temp$julian, w, col="darkgreen")
  # lines(temp$julian, r, col="purple")
  
  latm <- mean(temp$latitude)
  lonm <- mean(temp$longitude)
  cent <- c(lonm, latm)
  studymap <- get_map(location=cent, scale=2, zoom=12, maptype="satellite", source="google")
  
  ggmap(studymap) + 
    xlab("Longitude") + ylab("Latitude") + 
    geom_point(data=temp, aes(x=longitude, y=latitude), size=1, color=70)
  
  
  
  
  
}

# att <- read.csv("data/attempt_dates.csv", stringsAsFactors=FALSE)
# att[,c(4,5,7,8)] <- lapply(att[,c(4,5,7,8)], FUN=as.Date) 
# 
# att$start.acc <- as.numeric(format(att$AttemptStart.ACC, "%j"))
# att$end.acc <- as.numeric(format(att$AttemptEnd.ACC, "%j"))
# att$start.gps <- as.numeric(format(att$AttemptStart.GPS, "%j"))
# att$end.gps <- as.numeric(format(att$AttemptEnd.GPS, "%j"))
# 

for (i in 1:length(un.id)) {
  temp <- dat[dat$animal_id==un.id[i],]
  t2 <- att[att$animal_id==un.id[i],]

  filename <- paste0("output/attempt_plots/", temp$animal_id[1], "-", temp$pop[1], "-",
                     temp$tag[1], ".jpg")
  jpeg(filename, width=1000, height=796)
  par(mfrow=c(2,1), mar=c(4.2,4.2,1,0.5))
  plot(temp$julian, temp$odba, type="l", lwd=2, main=temp$animal_id[1], ylim=c(0,1.5),
       ylab="ODBA (energy expenditure)", xlab="Julian Day")
  abline(v=c(121, 152, 182), col="gray70", lty=2)
  # abline(h=0.2, col="gray70", lty=3)
  abline(h=mean(temp$odba, na.rm=TRUE), col="blue", lty=3)
  abline(h=quantile(temp$odba, probs=c(0.05, 0.95), na.rm=TRUE), col="purple", lty=4)
  text(y=c(1.4,1.4,1.4), x=c(124,154,184), labels=c("May", "June", "July"), pos=3)
  abline(v=c(t2$start.acc, t2$end.acc), col="red", lwd=2)
  plot(temp$julian, temp$dist, type="l", ylim=c(0,5000),
       ylab="Distance (meters)", xlab="Julian Day", lwd=2)
  abline(v=c(121, 152, 182), col="gray70", lty=2)
  abline(v=c(t2$start.gps, t2$end.gps), col="red", lwd=2)
  abline(h=median(temp$dist, na.rm=FALSE), col="blue", lty=3)
  abline(h=quantile(temp$dist, probs=c(0.05, 0.95), na.rm=TRUE), col="purple", lty=3)
  dev.off()
}

# dat1 <- dat[dat$pop=="GRLD" & !is.na(dat$animal_id),]

# un.id <- unique(dat1$animal_id)
# for (i in 1:length(un.id)) {
#   temp <- dat1[dat1$animal_id==un.id[i],]
#   id <- temp$animal_id[3]
#   year <- temp$year[3]
#   filename <- paste0("C:/Users/StephCunningham/Desktop/Mizzou/eobs_gps/",  id, "_", year, "_tracks.csv")
#   write.csv(temp, filename)
# }


# Create figure 2 in manuscript
defer <- dat[dat$attempt==0,]

bird <- dat[(dat$animal_id=="RP19F" | dat$animal_id=="17763") & dat$date>="2018-05-15" & dat$date<="2018-07-15",]
bird <- bird[complete.cases(bird),]

bird$attempt <- ifelse(bird$attempt==0, "Defer", "Attempt")
bird$dist[bird$dist > 30*1000] <- NA

odba <- ggplot(bird, aes(x=date, y=odba)) + geom_line(size=1) + 
  facet_grid(.~attempt) + 
  theme_classic() + ylab("Overall Dynamic Body Acceleration") +
  theme(panel.border=element_rect(color="black", fill=NA, size=0.5),
        axis.text.y=element_text(size=14), axis.title.y=element_text(size=16, face="bold"),
        strip.text.x=element_text(size=14, face="bold"),
        axis.text.x=element_blank(), axis.title.x=element_blank(),
        plot.title=element_text(size=16, face="bold", hjust=0.5))

disp <- ggplot(bird, aes(x=date, y=dist/1000)) + geom_line(size=1) + 
  facet_grid(.~attempt) + coord_cartesian(ylim=c(0,30)) +
  theme_classic() + ylab("Daily Displacement (km)") + xlab("Date") +
  scale_x_date(date_labels="%d-%b") +
  theme(panel.border=element_rect(color="black", fill=NA, size=0.5),
        axis.text.y=element_text(size=14), axis.title=element_text(size=16, face="bold"),
        axis.text.x=element_text(size=14, hjust=0.5),
        strip.text.x=element_text(size=14, face="bold"),
        plot.title=element_text(size=16, face="bold", hjust=0.5)) 

gA <- ggplotGrob(odba)
gB <- ggplotGrob(disp)
maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
gA$widths[2:5] <- as.list(maxWidth)
gB$widths[2:5] <- as.list(maxWidth)

grid.arrange(gA, gB, ncol=1)

ggarrange(odba, disp, nrow=2)

ggdraw()
draw_plot(plot, x=0, y=0, width=1, height=1)
draw_plot_label(label, x = 0, y = 1, size = 16)
