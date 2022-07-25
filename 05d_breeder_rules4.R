
#### USE THIS FOR MANSCRIPT !!!!!!!

library(tidyverse)
library(adehabitatLT)
library(cowplot)
library(pracma)
library(zoo)
library(grid)
library(gridExtra)
library(strucchange)

theme_set(theme_test())

drive <- getwd()
drive <- substr(drive,1,1)

setwd("data/mapped_classifications")
dat <- list.files(path="./", pattern=".csv", all.files=TRUE, full.names=FALSE)
dat <- lapply(dat, FUN=read.csv, header=TRUE, stringsAsFactors=FALSE)
dat <- do.call("rbind", dat)
setwd(paste0(drive, ":/ResearchProjects/Behavior_and_Weather"))

dat$date <- as.Date(dat$date)
dat$tag <- "ORN"
dat$tag[dat$id=="LM17M" | dat$id=="LM31F" | dat$id=="RP01F" | dat$id=="RP08F" | dat$id=="RP15F" | dat$id=="X9Z"] <- "CTT"
dat$tag[dat$id=="1749" | dat$id=="2160" | dat$id=="2161" | dat$id=="2164" | dat$id=="2167" | dat$id=="2174" | dat$id=="2176" | dat$id=="2820" |
          dat$id=="2825" | dat$id=="2826" | dat$id=="2827" | dat$id=="2830" | dat$id=="2832" | dat$id=="2838" | dat$id=="2839"] <- "EOBS"

dat$pop <- "GRLD"
dat$pop[dat$id=="LM17M" | dat$id=="LM31F" | dat$id=="RP01F" | dat$id=="RP08F" | dat$id=="RP15F" | dat$id=="17701" |
          dat$id=="17709" | dat$id=="17719" | dat$id=="17744" | dat$id=="17828" | dat$id=="17829"] <- "NAMC"

dat$sex <- "female"
dat$sex[dat$id=="1749" | dat$id=="2161" | dat$id=="2161" | dat$id=="2164" | dat$id=="2167" | dat$id=="2174" | dat$id=="2176" | 
          dat$id=="2820" | dat$id=="2825" | dat$id=="2826" | dat$id=="2827" | dat$id=="2830" | dat$id=="2832" | dat$id=="2838" | 
          dat$id=="2839" | dat$id=="LM17M"] <- "male"

dat$year <- format(dat$date, "%Y")
dat$julian <- format(dat$date, "%j")

dat <- subset(dat, (date>="2012-02-01" & date<="2012-08-01") | (date>="2013-02-01" & date<="2013-08-01") | 
                (date>="2017-02-01" & date<="2017-08-01") | (date>="2018-02-01" & date<="2018-08-01"))

dat <- dat[,-1]
dat <- dat[dat$id!="X9Z",]  # Not enough data for analysis

# Add in column for breeding attempt
dat$attempt <- 0  

# Change ID for NAMC ornitela
dat$id[dat$id=="17701"] <- "RP20F"
dat$id[dat$id=="17709"] <- "RP19F"
dat$id[dat$id=="17719"] <- "RP17F"
dat$id[dat$id=="17744"] <- "LM39F"
dat$id[dat$id=="17828"] <- "RP23F"
dat$id[dat$id=="17829"] <- "RP22F"

# change julian day to numeric
dat$julian <- as.numeric(dat$julian)

# modba <- aggregate(odba ~ id, data=dat, FUN=median, na.rm=TRUE)

br <- dat[dat$julian>=135 & dat$julian<=188,]
br <- subset(br, id!="RP15F" & id!="LM31F" & id!="LM17M")

un.id <- unique(br$id)


#### RULE 1 ####
inc <- subset(br, (id=="2161" & (julian>=149 & julian<=174)) | (id=="2838" & (julian>=152 & julian<=179)))

med <- aggregate(odba ~ julian + id, data=br, FUN=median, na.rm=TRUE)
med2 <- subset(med, (id=="2161") | (id=="2838"))

med2161 <- subset(med2, id=="2161")

test <- breakpoints(julian~odba, h=10, breaks=3, data=med2161)

plot(test)

plot(med2161$julian, med2161$odba)
abline(v=c(135+10,135+24,135+35))

med2 <- subset(med, (id=="2161" & (julian>=149 & julian<=174)) | (id=="2838" & (julian>=152 & julian<=179)))

quantile(med2$odba, probs=0.75)
med2$below <- ifelse(med2$odba<0.0648, 1, 0)

ggplot(med2, aes(x=julian, y=odba, group=id)) + geom_line() + geom_hline(aes(yintercept=0.372))


for (i in 1:length(un.id)) {
  b2161 <- subset(br, id==un.id[i])
  
  print(mean(b2161$odba))
  
  med2161 <- aggregate(odba ~ julian + id, data=b2161, FUN=median, na.rm=TRUE)
  mean2161 <- aggregate(odba ~ julian, data=b2161, FUN=mean, na.rm=TRUE)

  plot(b2161$julian, b2161$odba, main=un.id[i], ylim=c(0,2))
  lines(med2161$julian, med2161$odba, col="green", type="l", lwd=2)
  # lines(mean2161$julian, mean2161$odba, col="green", type="l", lwd=2)
  # abline(h=0.122, col="blue", lwd=2)
  abline(h=0.0648, col="blue", lwd=2)
}


# Rule 2: Fewer than 18 days of ODBA < 0.0648 (75th quantile of incubation from confirmed successful breeders)
dates <- data.frame(start=seq(136,163,1), end=seq(161,188,1))
vals <- data.frame()

for (j in 1:length(un.id)) {
  
  ind <- med[med$id==un.id[j],]
  v <- matrix(NA, nrow=nrow(dates), ncol=2) 
  v[,1] <- un.id[j]
  
  for (i in 1:nrow(dates)) { 
    
    temp <- ind[ind$julian>=dates[i,1] & ind$julian<=dates[i,2], ]
    far <- sum(temp$odba<0.0648)
    
    v[i,2] <- far
    
  }
  
  v <- as.data.frame(v)
  vals <- rbind(vals, v)
  
}

rule2 <- data.frame()
for (i in 1:length(un.id)) {
  temp <- vals[vals$V1==un.id[i],]
  temp <- cbind(temp, dates)
  
  temp2 <- as.numeric(temp$V2)>=18  # Number of days 18
  temp <- cbind(temp, temp2)
  
  if (sum(temp2)>=1) {
    print(un.id[i])
    rule2 <- rbind(rule2, temp[temp$temp2==TRUE,])
  }
}

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

dat$birdno <- as.numeric(factor(dat$key, labels=seq(1,36,1)))

## subset to breeding period
dat <- dat[dat$julian>=136 & dat$julian<=212 & dat$longitude<= -47,]

dat$pop <- "GRLD"
dat$pop[str_which(dat$animal_id, "[A-Z]")] <- "NAMC"

dat$tag[str_detect(dat$animal_id, "[A-Z]") & is.na(dat$tag)] <- "CTT"
dat$tag[str_which(dat$animal_id, "[1-9]{5}")] <- "ORN"

as.data.frame(dat %>% group_by(animal_id, tag) %>% count())

ggplot(dat, aes(x=julian, y=factor(birdno))) + geom_point()

# Calculate mean & sd for displacement during molt
molt.disp <- mean(dat$dist[dat$julian>=207 & dat$julian<=210], na.rm=TRUE)
molt.disp
molt.sd <- molt.disp + 4*sd(dat$dist[dat$julian>=207 & dat$julian<=210], na.rm=TRUE)
molt.sd

# classify movement according to calculated values
dat$molt <- ifelse(dat$dist<=molt.disp, "molt", "movement")

# Rule 2: Fewer than 12 days of movement greater than mean of molt movement (~2km) in 25 day period
dates <- data.frame(start=seq(136,163,1), end=seq(161,188,1))
vals <- data.frame()

un.id <- unique(dat$animal_id)

for (j in 1:length(un.id)) {
  
  ind <- dat[dat$animal_id==un.id[j],]
  v <- matrix(NA, nrow=nrow(dates), ncol=2) 
  v[,1] <- un.id[j]
  
  for (i in 1:nrow(dates)) { 
    
    temp <- ind[ind$julian>=dates[i,1] & ind$julian<=dates[i,2], ]
    far <- sum(temp$molt!="molt")
    
    v[i,2] <- far
    
  }
  
  v <- as.data.frame(v)
  vals <- rbind(vals, v)
  
}

rule3 <- data.frame()
for (i in 1:length(un.id)) {
  temp <- vals[vals$V1==un.id[i],]
  temp <- cbind(temp, dates)
  
  temp2 <- as.numeric(temp$V2)<=7  # Number of days (12) or 5
  temp <- cbind(temp, temp2)
  
  if (sum(temp2)>=1) {
    print(un.id[i])
    rule3 <- rbind(rule3, temp[temp$temp2==TRUE,])
  }
}

names(med)[2:3] <- c("animal_id", "medianODBA")
dat <- left_join(dat, med, by=c("animal_id", "julian"))

dat <- dat[dat$animal_id!="LM31F" & dat$animal_id!="LM17M" & dat$animal_id!="RP15F",]
dat$attempt <- 0
dat$attempt[dat$animal_id=="17763" | dat$animal_id=="17777" |
              dat$animal_id=="17778" | dat$animal_id=="17780" |
              dat$animal_id=="2161" | dat$animal_id=="2838" |
              dat$animal_id=="RP01F" | dat$animal_id=="RP08F" | 
              dat$animal_id=="RP17F" | dat$animal_id=="RP22F"] <- 1

dat <- dat[dat$julian>=136 & dat$julian<=188,]

dat$attempt <- ifelse(dat$attempt==1, "Succeed", "Defer/Fail")
dat$birdno <- as.numeric(factor(dat$key, labels=seq(1,34,1)))
dat$pop <- ifelse(dat$pop=="GRLD", "Greenland", "Midcontinent")
dat$molt <- ifelse(dat$molt=="molt", "Below threshold", "Above threshold")

dat$daymonth <- format(dat$date, "%d-%b")



defer <- dat[dat$attempt=="Defer/Fail",]
attempt <- dat[dat$attempt!="Defer/Fail",]

ggplot(attempt, aes(y=medianODBA, x=julian, group=id_ind, color=sex)) + geom_line()

attempt <- attempt[attempt$id_ind!=13,]

mean(attempt$medianODBA[attempt$sex=="female" & (attempt$julian>=166 & attempt$julian<=176)], na.rm=TRUE)
max(attempt$medianODBA[attempt$sex=="female"& (attempt$julian>=166 & attempt$julian<=176)], na.rm=TRUE)

mean(attempt$medianODBA[attempt$sex=="male" & (attempt$julian>=155 & attempt$julian<=175)], na.rm=TRUE)
max(attempt$medianODBA[attempt$sex=="male"& (attempt$julian>=155 & attempt$julian<=175)], na.rm=TRUE)


attempt <- attempt[!is.na(attempt$birdno),]



### Plotting ####

# Rule 1 ODBA

scaleFUN <- function(x) sprintf("%.1f", x)
r2def <- ggplot(defer, aes(y=medianODBA, x=julian, group=id_ind)) + 
  geom_line(color="gray45") + 
  coord_cartesian(ylim=c(0,1)) +
  ggtitle("Defer/Fail") +
  labs(tag="(a)", x="Date", y="ODBA") +
  guides(color=guide_legend(title="Population")) +
  scale_y_continuous(labels=scaleFUN) +
  scale_x_continuous(breaks=c(136,149,162,175,188), labels=c("16-May","29-May","11-June",
                                                             "24-June","07-July")) +
  # geom_hline(yintercept=0.065, color="red") + facet_grid(.~attempt) +
  theme(legend.position="none",
        panel.border=element_rect(color="black", fill=NA, size=0.5),
        axis.title.y=element_text(size=16),
        axis.text=element_text(size=12), axis.title.x=element_blank(),
        legend.text=element_text(size=14), legend.title=element_text(size=14),
        strip.text=element_blank(),
        plot.title=element_text(size=16, hjust=0.5))

r2att <- ggplot(attempt, aes(y=medianODBA, x=julian, group=id_ind)) + 
  geom_line(color="gray45") + 
  coord_cartesian(ylim=c(0,1)) +
  ggtitle("Succeed") +
  labs(tag="(b)", x="Date") +
  scale_x_continuous(breaks=c(136,149,162,175,188), labels=c("16-May","29-May","11-June",
                                                             "24-June","07-July")) +
  scale_y_continuous(labels=scaleFUN) +
  # geom_hline(yintercept=0.065, color="red") + facet_grid(.~attempt) +
  theme(panel.border=element_rect(color="black", fill=NA, size=0.5),
        axis.text=element_text(size=12), 
        axis.title=element_blank(),
        legend.text=element_text(size=14), 
        strip.text=element_blank(), 
        plot.title=element_text(size=16, hjust=0.5))

# Rule 2 Daily displacement

r3def <- ggplot(defer, aes(x=julian, y=as.factor(birdno))) + 
  geom_tile(aes(fill=molt), colour = "gray80") +
  scale_fill_manual(values=c("gray90", "#b2182b")) +
  scale_x_continuous(breaks=c(136,149,162,175,188), labels=c("16-May","29-May","11-June",
                                                             "24-June","07-July")) +
  labs(tag="(c)", x="Date", y="Goose ID") +
  theme(legend.position="none",
        axis.text=element_text(size=12), axis.title.y=element_text(size=16),
        axis.title=element_text(size=14))

r3att <- ggplot(attempt, aes(x=julian, y=as.factor(birdno))) + 
  geom_tile(aes(fill=molt), colour = "gray80") +
  scale_fill_manual(values=c("gray90", "#b2182b")) +
  scale_x_continuous(breaks=c(136,149,162,175,188), labels=c("16-May","29-May","11-June",
                                                                 "24-June","07-July")) +
  labs(tag="(d)", x="Date", y="Goose ID") +
  theme(legend.position="bottom",
        axis.text=element_text(size=12), axis.title.x=element_text(size=14),
        legend.text=element_text(size=14),axis.title.y=element_blank(),
        legend.title=element_blank())

rec <- rectGrob(gp=gpar(fill="white", col="white"))
lay <- cbind(c(3,4),
             c(3,4),
             c(3,4),
             c(1,2),
             c(1,2),
             c(1,2),
             c(1,5),
             c(1,5))

grid.arrange(r3def, r3att, r2def, r2att, rec, layout_matrix=t(lay))








