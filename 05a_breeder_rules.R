
library(tidyverse)
library(adehabitatLT)
library(cowplot)
library(pracma)
library(zoo)
library(grid)
library(gridExtra)


theme_set(theme_test())

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

ggplot(dat, aes(y=dist, x=julian, group=key, shape=molt, color=molt)) + geom_point() +
  # geom_vline(xintercept=c(136, 152, 191,196)) +
  geom_vline(xintercept=c(166, 182)) + 
  facet_wrap(animal_id~., scales="free_y") +
  theme(legend.position=c(1,0), legend.justification=c(1,0))

un.id <- unique(dat$animal_id)


# Rule 1: maximum distance traveled in a day between June 20 and July 1 is less than 8km (2x max distance snow geese traveled in Mainguy et al 2006)
rule1 <- data.frame()
for (i in 1:length(un.id)) {
  
  ind <- dat[dat$animal_id==un.id[i] & (dat$julian>=166 & dat$julian<=182),]
  
  if (max(ind$dist)<8000) {
    print(un.id[i])
  }
}

# Rule 2: Fewer than 12 days of movement greater than mean of molt movement (~2km) in 25 day period
dates <- data.frame(start=seq(136,163,1), end=seq(161,188,1))
vals <- data.frame()

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

rule2 <- data.frame()
for (i in 1:length(un.id)) {
  temp <- vals[vals$V1==un.id[i],]
  temp <- cbind(temp, dates)
  
  temp2 <- as.numeric(temp$V2)<=7  # Number of days (12) or 5
  temp <- cbind(temp, temp2)
  
  if (sum(temp2)>=1) {
    print(un.id[i])
    rule2 <- rbind(rule2, temp[temp$temp2==TRUE,])
  }
}

# Rule 3: ODBA stays low for 25 days (with less than 12 large movements) 
odba.mean <- mean(dat$odba[dat$julian>=191 & dat$julian<=196], na.rm=TRUE)
odba.sd <- sd(dat$odba[dat$julian>=191 & dat$julian<=196], na.rm=TRUE)
odba.usd <- odba.mean + odba.sd
odba.lsd <- odba.mean - odba.sd

ggplot(dat, aes(y=odba, x=julian, group=key)) + geom_line() +
  geom_hline(yintercept=c(odba.mean), color="red") + 
  geom_hline(yintercept=c(odba.lsd, odba.usd)) + 
  facet_wrap(animal_id~.)

dat$molt.odba <- odba.lsd

dat$odba.level <- ifelse(dat$odba<=odba.mean, "low", "not low")
dat$odba.level2 <- ifelse(dat$odba<=odba.lsd, "low", "not low")

dates <- data.frame(start=seq(136,163,1), end=seq(161,188,1))
vals <- data.frame()

for (j in 1:length(un.id)) {
  
  ind <- dat[dat$animal_id==un.id[j],]
  v <- matrix(NA, nrow=nrow(dates), ncol=2) 
  v[,1] <- un.id[j]
  
  for (i in 1:nrow(dates)) { 
    
    temp <- ind[ind$julian>=dates[i,1] & ind$julian<=dates[i,2], ]
    far <- sum(temp$odba.level2 != "low")
    
    v[i,2] <- far
    
  }
  
  v <- as.data.frame(v)
  vals <- rbind(vals, v)
  
}

vals <- vals[complete.cases(vals$V2),]

rule3 <- data.frame()
for (i in 1:length(un.id)) {
  temp <- vals[vals$V1==un.id[i],]
  
  if (nrow(temp)>1) {
    temp <- cbind(temp, dates[1:nrow(temp),])
    
    temp2 <- as.numeric(temp$V2)<=12  # Is this only 5 days?
    temp <- cbind(temp, temp2)
    temp <- temp[complete.cases(temp),]
    temp2 <- temp2[!is.na(temp2)]
    
    if (sum(temp2)>=1) {
      print(un.id[i])
      rule3 <- rbind(rule3, temp[temp$temp2==TRUE,])
    }
  }
}

# Rule 4: is there at least 1 25-day period that meets rule 2 and 3

r3.id <- unique(rule3$V1)

for (i in 1:length(r3.id)) {
  
  r3 <- rule3[rule3$V1==r3.id[i],]
  r2 <- rule2[rule2$V1==r3.id[i],]
  
  matches <- r2$start %in% r3$start
  
  if (sum(matches)>1) {
    print(r3.id[i])
  }
  
}

as.data.frame(dat %>% group_by(animal_id) %>% summarize(unique(attempt)))


dat$attempt <- 0
dat$attempt[dat$animal_id=="17763" | dat$animal_id=="17777" |
                 dat$animal_id=="17778" | dat$animal_id=="17780" |
                 dat$animal_id=="2161" | dat$animal_id=="2838" |
                 dat$animal_id=="RP01F" | dat$animal_id=="RP08F" | 
                 dat$animal_id=="RP17F" | dat$animal_id=="RP22F"] <- 1

dat <- dat[dat$animal_id!="LM31F" & dat$animal_id!="LM17M" & dat$animal_id!="RP15F",]

dat <- dat[dat$julian>=136 & dat$julian<=188,]

dat$attempt <- ifelse(dat$attempt==1, "Succeed", "Defer/Fail")
dat$birdno <- as.numeric(factor(dat$key, labels=seq(1,34,1)))
dat$pop <- ifelse(dat$pop=="GRLD", "Greenland", "Midcontinent")
dat$molt <- ifelse(dat$molt=="molt", "Low movement", "High movement")

# dat2 <- pivot_longer(dat, 17:18, names_to="variable", values_to="values") 

defer <- dat[dat$attempt=="Defer/Fail",]
attempt <- dat[dat$attempt!="Defer/Fail",]

# Rule 2
# ggplot(defer, aes(y=dist/1000, x=julian, group=animal_id)) + 
#   geom_line(color="gray70") + geom_point(aes(color=molt)) +
#   coord_cartesian(xlim=c(136,182), ylim=c(0,15)) +
#   xlab("day of year") + ylab("Daily Displacement (km)") +
#   # geom_hline(yintercept=molt.disp/1000) + 
#   facet_grid(birdno~.) +
#   theme(legend.position="none",
#     # legend.justification=c(1,0), legend.position=c(1,0),
#         axis.text=element_text(size=12), axis.title=element_text(size=16),
#         legend.text=element_text(size=14), legend.title=element_text(size=16),
#         strip.text=element_text(size=12))


r2def <- ggplot(defer, aes(x=julian, y=as.factor(birdno))) + 
  geom_tile(aes(fill=molt), colour = "gray80") +
  scale_fill_manual(values=c("gray90", "#b2182b")) +
  ggtitle("Defer/Fail") +
  labs(tag="A", x="Day of Year", y="Goose ID") +
  theme(legend.position="none",
        axis.text=element_text(size=12), axis.title.y=element_text(size=16),
        axis.title.x=element_blank(),
        plot.title=element_text(size=16, hjust=0.5))

r2att <- ggplot(attempt, aes(x=julian, y=as.factor(birdno))) + 
  geom_tile(aes(fill=molt), colour = "gray80") +
  scale_fill_manual(values=c("gray90", "#b2182b")) +
  ggtitle("Succeed") +
  # guides(fill=guide_legend(title="Category")) +
  labs(tag="B", x="Day of Year", y="Goose ID") +
  theme(legend.position="bottom",
        axis.text=element_text(size=12), axis.title=element_blank(),
        legend.text=element_text(size=14),
        legend.title=element_blank(), #legend.title=element_text(size=14),
        plot.title=element_text(size=16, hjust=0.5))

# Rule 3

scaleFUN <- function(x) sprintf("%.1f", x)
r3def <- ggplot(defer, aes(y=odba, x=julian, group=id_ind, color=pop)) + 
  geom_line(size=0.8) + 
  coord_cartesian(ylim=c(0,1)) +
  # scale_color_manual(values=c("#bf812d", "#01665e")) +
  scale_color_manual(values=c("gray65", "black")) +
  labs(tag="C", x="Day of Year", y="ODBA") +
  guides(color=guide_legend(title="Population")) +
  scale_y_continuous(labels=scaleFUN) +
  geom_hline(yintercept=odba.lsd, color="red") + facet_grid(.~attempt) +
  theme(legend.position="none",
        panel.border=element_rect(color="black", fill=NA, size=0.5),
        axis.text=element_text(size=12), axis.title=element_text(size=14),
        legend.text=element_text(size=14), legend.title=element_text(size=14),
        strip.text=element_blank())

r3att <- ggplot(attempt, aes(y=odba, x=julian, group=id_ind, color=pop)) + 
  geom_line(size=0.8) + 
  coord_cartesian(ylim=c(0,1)) +
  # scale_color_manual(values=c("#bf812d", "#01665e")) +
  scale_color_manual(values=c("gray65", "black")) +
  labs(tag="D", x="Day of Year") +
  # guides(color=guide_legend(title="Population")) +
  scale_y_continuous(labels=scaleFUN) +
  geom_hline(yintercept=odba.lsd, color="red") + facet_grid(.~attempt) +
  theme(legend.position=c(0.99,0.99), legend.justification=c(1,1),
        legend.title=element_blank(),
        panel.border=element_rect(color="black", fill=NA, size=0.5),
        axis.text=element_text(size=12), axis.title.x=element_text(size=14),
        axis.title.y=element_blank(),
        legend.text=element_text(size=14), #legend.title=element_text(size=14),
        strip.text=element_blank())

rec <- rectGrob(gp=gpar(fill="white", col="white"))
lay <- cbind(c(1,2),
             c(1,2),
             c(1,2),
             c(1,5),
             c(1,5),
             c(3,4),
             c(3,4),
             c(3,4))

grid.arrange(r2def, r2att, r3def, r3att, rec, layout_matrix=t(lay))


# gs <- lapply(1:9, function(ii) 
#   grobTree(rectGrob(gp=gpar(fill=ii, alpha=0.5)), textGrob(ii)))
# grid.arrange(grobs=gs, ncol=4, 
#              top="top label", bottom="bottom\nlabel", 
#              left="left label", right="right label")
# grid.rect(gp=gpar(fill=NA))




