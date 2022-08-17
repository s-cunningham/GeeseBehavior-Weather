
library(tidyverse)
library(qmap)

options(scipen=999, digits=3)

# Source the function file
source("R/00_ACCfunctions.R") 
source("R/00_EOBSfunctions.R")

#### Map Ornitela to CTT ###
# Read in data from a few individuals to set parameters for mapping
ctt1 <- read.csv("data/ed.sumstatsCTT/X9Z_sumstatsed.csv")
cssb1 <- data.frame(burst=ctt1$burst)
ctt2 <- read.csv("data/ed.sumstatsCTT/LM17M_sumstatsed.csv")
cssb2 <- data.frame(burst=ctt2$burst)

c1 <- read.csv("data/wideCTT/X9Z_wide.csv", stringsAsFactors=FALSE)
c1 <- c1[,2:3]
c1 <- semi_join(c1, cssb1, by="burst")
c2 <- read.csv("data/wideCTT/LM17M_wide.csv", stringsAsFactors=FALSE)
c2 <- c2[,2:3]
c2 <- semi_join(c2, cssb2, by="burst")
c <- rbind(c1, c2)

ctt <- rbind(ctt1, ctt2)
ctt <- ctt[,-1]
ctt <- cbind(c, ctt)

ctt$date <- format(as.POSIXct(ctt$timestamp, tz="GMT"), "%Y-%m-%d")
ctt <- ctt[,c(1,48,2,4:47)]

orn1 <- read.csv("data/ed.sumstats/17763_sumstatsed.csv")
ossb1 <- data.frame(burst=orn1$burst)
orn2 <- read.csv("data/ed.sumstats/17719_sumstatsed.csv")
ossb2 <- data.frame(burst=orn2$burst)

o1 <- read.csv("data/wide/17763_wide.csv", stringsAsFactors=FALSE)
o1 <- o1[,2:3]
o1 <- semi_join(o1, ossb1, by="burst")
o2 <- read.csv("data/wide/17719_wide.csv", stringsAsFactors=FALSE)
o2 <- o2[,2:3]
o2 <- semi_join(o2, ossb2, by="burst")
o <- rbind(o1, o2)

orn <- rbind(orn1, orn2)
orn <- orn[,-1]
orn <- cbind(o, orn)

orn$date <- format(as.POSIXct(orn$timestamp, tz="GMT"), "%Y-%m-%d")
orn <- orn[,c(1,48,2,4:47)]

# ODBA
odba.fitq <- fitQmapSSPLIN(obs=ctt$odba, mod=orn$odba, wet.day=FALSE, qsetp=0.001)
odba.map <- doQmap(orn$odba, odba.fitq)

# Plot mapped data
orn.ordered <- sort(orn$odba)
ctt.ordered <- sort(ctt$odba)
map.ordered <- sort(odba.map)
n <- sum(!is.na(orn$odba))
n2 <- sum(!is.na(ctt$odba))
n3 <- sum(!is.na(odba.map))
plot((1:n2)/n2, ctt.ordered, type='s', xlim=c(0,1), ylim=c(0,8), lwd=3, lty=1, col="black", xlab="", 
     ylab="Acceleration (g)", main="", cex.lab=1.5, cex.axis=1.3)
lines((1:n)/n, orn.ordered, col="blue", lwd=3, lty=2)
lines((1:n3)/n3, map.ordered, col="red", lwd=3, lty=3)
legend("topright", c(expression("CTT (P"[T]*")"), expression("Orn (P"[B]*")"), expression("Mapped Orn [h(P"[B]*")]")), col=c("black", "blue", "red"), lty=c(1, 2, 3), lwd=3, bty="n", cex=1.2)
text(0,8, labels="A", cex=1.5)
par(mar=c(4.2,4.2,1,1))


## Read in data to map ODBA
# Read in classified data with ODBA
# file.name <- list.files(path="output/classified/orn", pattern=".csv", all.files=TRUE, full.names=TRUE)
# file.list <- lapply(file.name, FUN=read.csv, header=TRUE, stringsAsFactors=FALSE)

# for (i in 1:length(file.list)) {
#   temp <- file.list[[i]]
#   temp <- temp[,-1]
#   
#   # Fix ID column
#   temp$id <- gsub("[[:punct:]]", "", temp$id) # remove punctuation
#   temp$id <- gsub("[a-z]", "", temp$id)
#   
#   # Map ODBA
#   odba.map <- doQmap(temp$odba, odba.fitq)
#   temp$odba.map <- odba.map
#   
#   # set up file name
#   id1 <- strsplit(file.name[[i]], "[/_]")
#   id <- id1[[1]][4]
#   
#   filename = paste0("output/odba_map/", id, "_classed-odba-map.csv")
#   write_csv(temp, filename)
# }

#### Map e-obs to CTT ###
eobs1 <- read.csv("data/eobs_sumstats/2820_sumstats.csv")
names(eobs1)[2] <- "burst"
essb1 <- data.frame(burst=eobs1$burst)
eobs2 <- read.csv("data/eobs_sumstats/2827_sumstats.csv")
names(eobs2)[2] <- "burst"
essb2 <- data.frame(burst=eobs2$burst)

e1 <- read.csv("data/eobs_converted/2820_wide.csv", stringsAsFactors=FALSE)
e1 <- e1[,2:3]
e1 <- semi_join(e1, essb1, by="burst")

e2 <- read.csv("data/eobs_converted/2827_wide.csv", stringsAsFactors=FALSE)
e2 <- e2[,2:3]
e2 <- semi_join(e2, essb2, by="burst")
eo <- rbind(e1, e2)

eobs <- rbind(eobs1, eobs2)
eobs <- eobs[,-1]
eobs <- cbind(eo, eobs)
eobs <- eobs[,-2]

# Qmap the e-obs subset
odba.fitq <- fitQmapQUANT(obs=ctt$odba, mod=eobs$odba, wet.day=FALSE, qsetp=0.001)
odba.map <- doQmap(eobs$odba, odba.fitq)

eobs.ordered <- sort(eobs$odba)
nb.ordered <- sort(ctt$odba)
map.ordered <- sort(odba.map)
n <- sum(!is.na(eobs$odba))
n2 <- sum(!is.na(ctt$odba))
n3 <- sum(!is.na(odba.map))
plot((1:n2)/n2, nb.ordered, type='s', xlim=c(0,1), ylim=c(0,8), lwd=3, lty=1, col="black", xlab="Empirical Probability",
     ylab="Acceleration (g)", main="", cex.lab=1.5, cex.axis=1.3)
lines((1:n)/n, eobs.ordered, col="blue", lwd=3, lty=2)
lines((1:n3)/n3, map.ordered, col="red", lwd=3, lty=3)
legend("topright", c(expression("CTT (P"[T]*")"), expression("e-Obs (P"[B]*")"), expression("Mapped e-Obs [h(P"[B]*")]")), col=c("black", "blue", "red"),
       lty=c(1, 2, 3), lwd=3, bty="n", cex=1.2)
text(0,8, labels="B", cex=1.5)

## Read in files
# burst ODBA
file.name <- list.files(path="data/eobs_original/burst_odba", pattern=".csv", all.files=TRUE, full.names=TRUE)
file.list <- lapply(file.name, FUN=read.csv, header=TRUE, stringsAsFactors=FALSE)

# burst classified
behav <- list.files(path="data/eobs_original/burst_classified", pattern=".csv", all.files=TRUE, full.names=TRUE)
behav <- lapply(behav, FUN=read.csv, header=TRUE, stringsAsFactors=FALSE)

for (i in 1:length(file.list)) {
  temp <- file.list[[i]]
  temp <- temp[,-1]
  
  # edit column names
  names(temp) <- tolower(names(temp))
  
  # Map ODBA
  odba.map <- doQmap(temp$odba, odba.fitq)
  temp$odba.map <- odba.map
  
  # Read behavior classification
  b <- behav[[i]]
  names(b) <- tolower(names(b)) 
  b <- b[,c(2:4,6,5)]
  
  # Join behavior and ODBA
  temp <- left_join(temp, b, by=c("id", "date", "time", "burst"))
  
  # Fix date
  newdate <- lapply(temp$date, FUN=dateConvert)
  newdate <- unlist(newdate)
  temp$date <- newdate
  
  filename = paste0("output/odba_map/", temp$id[1], "_classed-odba-map.csv")
  write_csv(temp, filename)
  
}


