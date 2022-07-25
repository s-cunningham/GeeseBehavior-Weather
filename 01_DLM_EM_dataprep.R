## 19 July 2019


library(tidyverse)
library(lubridate)

rm(list=ls())

options(scipen=999, digits=3)

setwd("data/mapped_classifications")
dat <- list.files(path="./", pattern=".csv", all.files=TRUE, full.names=FALSE)
dat <- lapply(dat, FUN=read.csv, header=TRUE, stringsAsFactors=FALSE)
dat <- do.call("rbind", dat)
setwd("F:/ResearchProjects/Behavior_and_Weather")

dat$date <- as.Date(dat$date)
dat$tag <- "ORN"
dat$tag[dat$id=="LM17M" | dat$id=="LM31F" | dat$id=="RP01F" | dat$id=="RP08F" | dat$id=="RP15F" | dat$id=="X9Z"] <- "CTT"
dat$tag[dat$id=="1749" | dat$id=="2161" | dat$id=="2161" | dat$id=="2164" | dat$id=="2167" | dat$id=="2174" | dat$id=="2176" | dat$id=="2820" |
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
dat$attempt[dat$id=="2160" | dat$id=="2161" | dat$id=="2176" | dat$id=="2838" | dat$id=="17762" | dat$id=="17763" | dat$id=="17766" |
               dat$id=="17768" | dat$id=="17777" | dat$id=="17778" | dat$id=="17780" | dat$id=="17813" | dat$id=="17814" | dat$id=="17829" |
               dat$id=="RP01F" | dat$id=="LM17M" | dat$id=="RP15F" | dat$id=="RP08F" | dat$id=="LM31F" | dat$id=="17719"] <- 1

# Change ID fro NAMC ornitela
dat$id[dat$id=="17701"] <- "RP20F"
dat$id[dat$id=="17709"] <- "RP19F"
dat$id[dat$id=="17719"] <- "RP17F"
dat$id[dat$id=="17744"] <- "LM39F"
dat$id[dat$id=="17828"] <- "RP23F"
dat$id[dat$id=="17829"] <- "RP22F"


#### Add range labels ####
# North America
namc <- subset(dat, pop=="NAMC")
un.id <- unique(namc$id)
attributes(namc$timestamp)$tzone <- "America/Chicago"

namc <- separate(namc, col="timestamp", into=c("date_CST", "time_CST"), sep=" ", remove=FALSE)

# Read in GPS data
setwd("data/gps_zones/NAMC")
file.list <- list.files(path="./", pattern=".csv", all.files=TRUE, full.names=FALSE)
file.list <- lapply(file.list, FUN=read.csv, header=TRUE, stringsAsFactors=FALSE)
files <- do.call("rbind",file.list)
setwd("F:/ResearchProjects/Behavior_and_Weather")

files <- separate(files, col="timestamp", into=c("date_CST", "time_CST"), sep=" ", remove=FALSE)
names(files)[c(4,5,7,8)] <- c("dateUTC", "timeUTC", "date", "time")
files <- files[,c(2:7,9:21,27:29)]

files$date <- as.Date(files$date)

newnamc <- data.frame()
for (i in 1:length(un.id)) {
  acc <- subset(namc, id==un.id[i])
  gps <- subset(files, animal_id==un.id[i])
  
  wint <- gps[gps$Winter==1,]
  ppr <- gps[gps$PPR==1,]
  breed <- gps[gps$BreedRange==1,]
  
  acc$Winter <- 0
  acc$PPR <- 0
  acc$BrRange <- 0
  
  acc$Winter[acc$timestamp<=max(wint$timestamp)] <- 1
  acc$PPR[(acc$timestamp>=min(ppr$timestamp) & acc$timestamp<=max(ppr$timestamp))] <- 1
  acc$BrRange[acc$timestamp>=min(breed$timestamp)] <- 1
  
  newnamc <- rbind(newnamc, acc)
}
newnamc$timestamp <- as.POSIXct(newnamc$timestamp, format="%Y-%m-%d %H:%M:%S", tz="America/Chicago")
attributes(newnamc$timestamp)$tzone <- "UTC"

# Greenland
grld <- subset(dat, pop=="GRLD")
attributes(grld$timestamp)$tzone <- "America/Sao_Paulo"

grld <- separate(grld, col="timestamp", into=c("date_ASP", "time_ASP"), sep=" ", remove=FALSE)

# Read in GPS data
setwd("data/gps_zones/GRLD")
file.list <- list.files(path="./", pattern=".csv", all.files=TRUE, full.names=FALSE)
file.list <- lapply(file.list, FUN=read.csv, header=TRUE, stringsAsFactors=FALSE)
files <- do.call("rbind",file.list)
setwd("F:/ResearchProjects/Behavior_and_Weather")

files <- separate(files, col="timestamp", into=c("date_ASP", "time_ASP"), sep=" ", remove=FALSE)
names(files)[c(4,5,7,8)] <- c("dateUTC", "timeUTC", "date", "time")
files <- files[,c(2:7,9:24)]

files$date <- as.Date(files$date)
un.yr <- unique(files$year)

newgrld <- data.frame()
for (j in 1:4) {
  yr <- subset(grld, year==un.yr[j])
  yrfiles <- subset(files, year==un.yr[j])
  un.id <- unique(yr$id)
  
  for (i in 1:length(un.id)) {
    acc <- subset(yr, id==un.id[i])
    gps <- subset(yrfiles, animal_id==un.id[i])
    
    wint <- gps[gps$Winter==1,]
    ppr <- gps[gps$Iceland==1,]
    breed <- gps[gps$BreedRange==1,]
    
    acc$Winter <- 0
    acc$Iceland <- 0
    acc$BrRange <- 0
    
    acc$Winter[acc$timestamp<=max(wint$timestamp)] <- 1
    acc$Iceland[(acc$timestamp>=min(ppr$timestamp) & acc$timestamp<=max(ppr$timestamp))] <- 1
    acc$BrRange[acc$timestamp>=min(breed$timestamp)] <- 1
    
    newgrld <- rbind(newgrld, acc)
    
    print(un.yr[j])
    print(un.id[i])
    print(sum(is.na(acc$Winter)))
    print(sum(is.na(acc$Iceland)))
    print(sum(is.na(acc$BrRange)))
  }
}
newgrld$timestamp <- as.POSIXct(newgrld$timestamp, format="%Y-%m-%d %H:%M:%S", tz="America/Sao_Paulo")
attributes(newgrld$timestamp)$tzone <- "UTC"
names(newgrld)[16] <- "PPR"

# Combine again
newgrld <- newgrld[,c(1:3, 6:17)]
newnamc <- newnamc[,c(1:3, 6:17)]

dat <- rbind(newgrld, newnamc)
names(dat)[14] <- "SprStop"

dat$julian <- as.numeric(dat$julian)


# Calculate time periods for analysis
names(dat)[13] <- "WinArea"

un.yr <- unique(dat$year)
dat3 <- data.frame()
for (j in 1:length(un.yr)) {
  dat2 <- subset(dat, year==un.yr[j])
  
  un.id <- unique(dat2$id)
  
  for (i in 1:length(un.id)) {
    temp <- subset(dat2, id==un.id[i])
    
    temp$timeperiod <- 0
    
    # winter (1)
    maxWin <- max(temp$date[temp$WinArea==1])
    winstart <- maxWin - days(14)
    temp$timeperiod[temp$date>winstart & temp$date<=maxWin] <- 1
    
    # Late spring (3)
    maxSprSt <- max(temp$date[temp$SprStop==1])
    LSprstart <- maxSprSt - days(14)
    temp$timeperiod[temp$date>=LSprstart & temp$date<=maxSprSt] <- 3
    
    # Spring (2)
    maxSpr <- LSprstart - days(1)
    minSpr <- maxWin + days(1)
    temp$timeperiod[temp$date>=minSpr & temp$date<=maxSpr] <- 2
    
    # Arrival (4)
    minBr <- maxSprSt + 1
    arrend <- minBr + days(14)
    temp$timeperiod[temp$date>=minBr & temp$date<arrend] <- 4
    
    dat3 <- rbind(dat3, temp)
  }
}

dat3$sex[dat3$id=="2160"] <- "male"

dat3 <- dat3[,c(1:12,16)]

tpcheck <- dat3 %>% group_by(id, timeperiod) %>% count()

# Convert timestamp to POSIX and reorder
dat3$year <- as.numeric(dat3$year)

N.order <- order(dat3$julian, decreasing=FALSE)
dat3 <- dat3[N.order,]

# Remove 2013 data for 2167, 2164, 2176
dat3 <- dat3[!(dat3$id=="2167" & dat3$year==2013) & !(dat3$id=="2164" & dat3$year==2013) & 
               !(dat3$id=="2176" & dat3$year==2013) & !(dat3$id=="2161" & dat3$year==2013) & !(dat3$id=="2160" & dat3$year==2013),]

# Add a column for bird number
id <- unique(dat3$id)
id_ind <- numeric(dim(dat3)[1])
for (i in 1:dim(dat3)[1]) {
  id_ind[i] <- which(id == dat3$id[i])
}
dat3 <- data.frame(dat3, id_ind)

# write.csv(dat3, "output/all_databyburst.csv")

### Calculate average daily ODBA
odba <- aggregate(odba ~ id + id_ind + date + tag + pop + timeperiod + attempt, data=dat3, FUN=mean, na.rm=TRUE)

odba$tag <- factor(odba$tag, level=c("ORN", "CTT", "EOBS"))
odba$year <- as.numeric(format(odba$date, "%Y"))
odba$julian <- as.numeric(format(odba$date, "%j"))

# Calculate mean and variance for all tag types
tagm <- aggregate(odba ~ tag + behavior, data=dat, FUN=mean, na.rm=TRUE)
tagvar <- aggregate(odba ~ tag + behavior, data=dat, FUN=var, na.rm=TRUE)

ggplot(dat, aes(x=tag, y=odba, fill=behavior)) + geom_boxplot() + theme_bw()

N.order <- order(odba$julian, decreasing=FALSE)
odba <- odba[N.order,]

# Assign relative days from end of "arrival" period backwards
odba$rel.day <- NA

un.id <- unique(odba$id)
for (i in 1:length(un.id)) {
  temp <- odba[odba$id==un.id[i],]
  p4 <- temp[temp$timeperiod==4,]
  min_jul <- min(temp$julian)
  md <- max(p4$julian)
  dlen <- length(min_jul:md)
  days <- 1:dlen
  odba[odba$id==un.id[i],11] <- c(rev(days), rep(NA, (nrow(temp) - dlen)))
}

N.order <- order(odba$id_ind, decreasing=FALSE)
odba <- odba[N.order,]

# ggplot(odba, aes(x=rel.day, y=id_ind)) + geom_point()
# write.csv(odba, "output/all_dailyodba.csv")

# Aggregate behavior by date
a <- dat3 %>% group_by(date, behavior, id, id_ind, timeperiod, attempt, pop, sex, year) %>% count()
a <- a %>% group_by(date, id, id_ind, timeperiod, year, attempt, pop, sex) %>%
  mutate(total=sum(n)) #%>%
  # mutate(prop=n/total)

a <- as.data.frame(a)
# a <- a[,c(3,4,1,5:9,2,12)]

a2 <- spread(a, key="behavior", value="n")
# names(a2)[9:11] <- c("prop.fly", "prop.graze", "prop.stationary")
a2$julian <- as.numeric(format(a2$date, "%j"))

# ggplot(a2, aes(x=julian, y=id_ind)) + geom_point()
# write.csv(a2, "output/all_dailybehavior.csv")

# Join behavior and ODBA data together
dat <- left_join(odba, a2, by=c("id", "date", "julian"))
dat <- dat[,c(1:11,16,18:21)]
names(dat)[c(2,5:7,9)] <- c("id_ind", "pop", "timeperiod", "attempt", "year")
dat$log.odba <- log(dat$odba)
dat <- dat[,c(1:3,9:11,6,7,4,5,12,8,13:16)]

# ggplot(dat, aes(x=rel.day, y=id_ind)) + geom_point()
# write.csv(dat, "output/all_daily-odba-behavior.csv")

# Check how many days prior to the last day of "arrival" time period
dat <- subset(dat, timeperiod!=0) 

un.id <- unique(dat$id)
days <- numeric()
pop <- character()
for (i in 1:length(un.id)) {
  temp <- subset(dat, id==un.id[i])
  mrd <- max(temp$rel.day, na.rm=TRUE)
  days[i] <- mrd
  pop[i] <- temp$pop[i]
  print(paste0(temp$id[1], " - ", mrd))
}

df <- data.frame(days=days, pop=pop)
aggregate(days ~ pop, data=df, FUN=max)

ggplot(dat, aes(x=julian, y=odba, group=id, color=tag)) + geom_line(size=1) + theme_bw() +
  facet_wrap(~pop, nrow=2) #+ scale_color_manual(values=c("gray60", "black"))

dat$rev.rel.day <- NA
for (i in 1:length(un.id)) {
  temp <- subset(dat, id==un.id[i])
  rvseq <- rev(temp$rel.day)
  dat[dat$id==un.id[i],17] <- rvseq
}
dat <- dat[,c(1:6,17,7:16)]

# write.csv(dat, "output/daily-odba-behavior_subset.csv")

# Plot ODBA vs PTF
dat <- read.csv("output/daily-odba-behavior_subset.csv")
dat <- dat[,-1]

dat <- mutate(dat, ptf=graze/total)
dat2 <- dat[dat$ptf>=0.25,]

ggplot(dat2, aes(x=ptf, y=log(odba))) + geom_point() + theme_bw()

cor(dat2[,c(13,18)])
cor(dat[,c(13,18)], method="spearman")

eobs <- dat[dat$tag=="EOBS",]
ggplot(eobs, aes(x=ptf, y=odba)) + geom_point() + theme_bw()



