
# Load packages
library(class)
library(tree)
library(randomForest)
library(MASS)
library(e1071)
library(matrixStats)
library(tidyverse)

# Source the function file
source("R/00_ACCfunctions.R")  

# Replication
set.seed(123)

# Read in Ornitela data
eobs.wide <- read.csv("data/eobs_wide.csv", stringsAsFactors=FALSE) 
eobs.wide <- eobs.wide[,-c(1)]
eobs.wide %>% group_by(behavior) %>% dplyr::count()
names(eobs.wide)[3:92] <- c(rep("X",30), rep("Y",30), rep("Z",30))

# Calculate summary statistics and scale
eobs.ss <- accSumStatsEOBS(orn.wide)
eobs.ss <- orn.ss[,-c(2:4, 19, 21, 24, 25)]






























file.name <- list.files(path="data/eobs_sumstats/", pattern=".csv", all.files=TRUE, full.names=TRUE)
file.list <- lapply(file.name, FUN=read.csv, header=TRUE, stringsAsFactors=FALSE)

wide <- list.files(path="data/eobs_converted/", pattern=".csv", all.files=TRUE, full.names=TRUE)
wide <- lapply(wide, FUN=read.csv, header=TRUE, stringsAsFactors=FALSE)

