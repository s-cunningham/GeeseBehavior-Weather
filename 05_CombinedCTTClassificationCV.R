#**********************************************************************************************************************************
#**********************************************************************************************************************************

# Project: Objective 1
# Date: 20 December 2018
# Author: Stephanie Cunningham
# Description: Classification of CTT dataset - cross validation to determine best method

#**********************************************************************************************************************************
#**********************************************************************************************************************************

# Load packages
library(class)
library(tree)
library(randomForest)
library(MASS)
library(tidyverse)
library(e1071)
library(matrixStats)

# Source the function file
source("R/00_ACCfunctions.R") 

set.seed(123)

# Read in training data
ctt.wide <- read.csv("data/new_ctt_continentscombined-wildgrazeonly.csv", stringsAsFactors=FALSE) # CTT data
ctt.wide <- ctt.wide[,-c(1)]
ctt.wide$Behavior[ctt.wide$Behavior=="walk"] <- "graze"
names(ctt.wide)[3:89] <- c(rep("X",29), rep("Y",29), rep("Z",29))

# Remove a random subset of walk/graze
samples <- sample(row.names(ctt.wide[ctt.wide$Behavior=="graze",]), size=139)
ctt.wide <- ctt.wide[!(rownames(ctt.wide) %in% samples),]
ctt.wide[,1:2] %>% group_by(Behavior) %>% dplyr::count()

# Calculate summary statistics and scale
ctt.ss <- accSumStatsCTT(ctt.wide)
ctt.ss <- ctt.ss[,-c(2:4, 19, 21, 24, 25)]

# Random Forest 
K=10
folds <- sample(1:K, nrow(ctt.ss), replace=TRUE)
rf.error <- matrix(0, nrow=1, ncol=K)
rf.prec <- matrix(0, nrow=3, ncol=K)
rf.rec <- matrix(0, nrow=3, ncol=K)
rf.accu <- matrix(0, nrow=3, ncol=K)

for (i in 1:K) {
  ctt.train <- ctt.ss[folds != i,2:38]
  behavior <- ctt.ss[folds != i, 1]
  ot <- cbind(behavior, ctt.train)
  ctt.test <- ctt.ss[folds == i,2:38]
  test.behaviors <- ctt.ss[folds==i,1]
  
  no.beh <- ncol(ctt.train)
  ctt.rf <- randomForest(factor(behavior)~., data=ot, mtry=sqrt(no.beh), ntree=2000)
  ctt.pred <- predict(ctt.rf, newdata=ctt.test)
  rf.error[1,i] <- mean(ctt.pred==test.behaviors) 
  
  t3 <- table(ctt.pred, test.behaviors)
  cv.m3 <- data.frame(modelPerformance(t3)[2])
  rf.prec[,i] <- as.matrix(cv.m3[2,2:4])
  rf.rec[,i] <- as.matrix(cv.m3[1,2:4])
  rf.accu[,i] <- as.matrix(cv.m3[3,2:4])
}

rms3 <- rowMeans(rf.error)
rms3

prec.rf <- rowMeans(rf.prec)
rec.rf <- rowMeans(rf.rec)
accu.rf <- rowMeans(rf.accu)

##########################################################################################################################
# K nearest neighbors

KCV=10
# folds <- sample(1:KCV, nrow(ctt.ss), replace=TRUE)
knn.error <- matrix(0, nrow=1, ncol=KCV)
knn.prec <- matrix(0, nrow=3, ncol=KCV)
knn.rec <- matrix(0, nrow=3, ncol=KCV)

for (i in 1:KCV) {
  ctt.train <- ctt.ss[folds != i,2:38]
  train.behaviors <- ctt.ss[folds != i, 1]
  ctt.test <- ctt.ss[folds == i,2:38]
  test.behaviors <- ctt.ss[folds==i,1]
  
  k=100
  
  ctt.k <- matrix(NA, nrow=100, ncol=2)
  ctt.k[,1] <- seq(1,100,1)
  
  for (j in 1:k) {
    ctt.knn2 <- knn(train=ctt.train, test=ctt.test, cl=train.behaviors, k=j)
    ctt.k[j,2] <- 1-mean(ctt.knn2==test.behaviors)
  }
  
  # Minimum error rate
  min(ctt.k[,2])
  K = min(which(ctt.k[,2]==min(ctt.k[,2])))
  
  # Run KNN with best value of k
  ctt.knn3 <- knn(train=ctt.train, test=ctt.test, cl=train.behaviors, k=K)
  
  knn.error[1,i] <- mean(ctt.knn3==test.behaviors) 
  
  t1 <- table(ctt.knn3,test.behaviors)
  cv.m1 <- data.frame(modelPerformance(t1)[2])
  knn.prec[,i] <- as.matrix(cv.m1[2,2:4])
  knn.rec[,i] <- as.matrix(cv.m1[1,2:4])
  
}

rms1 <- rowMeans(knn.error)
rms1

prec.knn <- rowMeans(knn.prec)
rec.knn <- rowMeans(knn.rec)

##########################################################################################################################
# Classification and regression trees

K=10
# folds <- sample(1:K, nrow(ctt.ss), replace=TRUE)
cart.error <- matrix(0, nrow=1, ncol=K)
cart.prec <- matrix(0, nrow=3, ncol=K)
cart.rec <- matrix(0, nrow=3, ncol=K)

for (i in 1:K) {
  ctt.train <- ctt.ss[folds != i,2:38]
  behavior <- ctt.ss[folds != i, 1]
  ctt.train <- cbind(behavior, ctt.train)
  ctt.test <- ctt.ss[folds == i,2:38]
  test.behaviors <- ctt.ss[folds==i,1]
  
  tree.ctt <- tree(factor(behavior)~., data=ctt.train)
  ctt.tree.p <- predict(tree.ctt, newdata=ctt.test, type="class")
  
  # Cross validation to determine optimal number of terminal nodes, and then "prune" the tree
  cv.ctt <- cv.tree(tree.ctt)
  prune.ctt <- prune.tree(tree.ctt,best=cv.ctt$size[which(cv.ctt$dev==min(cv.ctt$dev))])
  
  # Predict on the test set using the pruned tree
  ctt.prune.p <- predict(prune.ctt, newdata=ctt.test, type="class")
  
  cart.error[1,i] <- mean(ctt.prune.p==test.behaviors) 
  
  t2 <- table(ctt.prune.p, test.behaviors)
  cv.m2 <- data.frame(modelPerformance(t2)[2])
  cart.prec[,i] <- as.matrix(cv.m2[2,2:4])
  cart.rec[,i] <- as.matrix(cv.m2[1,2:4])
}

rms2 <- rowMeans(cart.error)
rms2

prec.cart <- rowMeans(cart.prec)
rec.cart <- rowMeans(cart.rec)

#************************************************************************************************************************
# Linear Discriminat Analysis (LDA)

# folds <- sample(1:K, nrow(ctt.ss), replace=TRUE)
lda.error <- matrix(0, nrow=1, ncol=K)
lda.prec <- matrix(0, nrow=3, ncol=K)
lda.rec <- matrix(0, nrow=3, ncol=K)

for (i in 1:K) {
  ctt.train <- ctt.ss[folds != i,2:38]
  behavior <- ctt.ss[folds != i, 1]
  ctt.train <- cbind(behavior, ctt.train)
  ctt.test <- ctt.ss[folds == i,2:38]
  test.behaviors <- ctt.ss[folds==i,1]
  
  ctt.lda <- lda(behavior~., data=ctt.train)
  ctt.lda.p <- predict(ctt.lda,  newdata=ctt.test)
  lda.error[1,i] <- mean(ctt.lda.p$class==test.behaviors)
  
  t4 <- table(ctt.lda.p$class, test.behaviors)
  cv.m4 <- data.frame(modelPerformance(t4)[2])
  lda.prec[,i] <- as.matrix(cv.m4[2,2:4])
  lda.rec[,i] <- as.matrix(cv.m4[1,2:4])
}

rms4 <- rowMeans(lda.error)
rms4

prec.lda <- rowMeans(lda.prec)
rec.lda <- rowMeans(lda.rec)

#************************************************************************************************************************
# support Vector Machines (SVM)

# folds <- sample(1:K, nrow(ctt.ss), replace=TRUE)
svm.error <- matrix(0, nrow=1, ncol=K)
svm.prec <- matrix(0, nrow=3, ncol=K)
svm.rec <- matrix(0, nrow=3, ncol=K)

for (i in 1:K) {
  ctt.train <- ctt.ss[folds != i,2:38]
  train.behaviors <- ctt.ss[folds != i, 1]
  ot <- cbind(train.behaviors, ctt.train)
  ctt.test <- ctt.ss[folds == i,2:38]
  test.behaviors <- ctt.ss[folds==i,1]
  
  ctt.svm <- svm(factor(train.behaviors)~., data=ot, kernel="radial")
  
  svm.p <- predict(ctt.svm, newdata=ctt.test)
  svm.error[1,i] <- mean(svm.p==test.behaviors)
  
  t5 <- table(svm.p, test.behaviors)
  cv.m5 <- data.frame(modelPerformance(t5)[2])
  svm.prec[,i] <- as.matrix(cv.m5[2,2:4])
  svm.rec[,i] <- as.matrix(cv.m5[1,2:4])
}

rms5 <- rowMeans(svm.error)
rms5

prec.svm <- rowMeans(svm.prec)
rec.svm <- rowMeans(svm.rec)


#************************************************************************************************************************
# Recreate Precision vs. Recall plot from AcceleRater

rf.pr <- rbind(prec.rf, rec.rf)
knn.pr <- rbind(prec.knn, rec.knn)
lda.pr <- rbind(prec.lda, rec.lda)
cart.pr <- rbind(prec.cart, rec.cart)
svm.pr <- rbind(prec.svm, rec.svm)

all <- rbind(knn.pr, cart.pr, rf.pr, lda.pr, svm.pr)
rownames(all) <- seq(1,nrow(all),1)
all <- as.data.frame(all)
Method <- c(rep("KNN",2), rep("CART",2), rep("RF",2), rep("LDA",2), rep("SVM",2))
all <- cbind(Method, all)
Measure <- rep(c("Precision","Recall"), 5)
all <- cbind(Measure, all)
names(all)[3:5] <- c("graze", "stationary", "fly")

apr <- all %>% gather(key="Behavior", value="Value", 3:5) %>%
  spread(Measure, Value)

ggplot(apr, aes(x=Recall, y=Precision, shape=Method, colour=Behavior)) + geom_point(size=5) + 
  coord_cartesian(xlim=c(70,100),ylim=c(70,100)) + theme_bw() + xlab("Recall (%)") + ylab("Precision (%)") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16, face="bold"),
        legend.text=element_text(size=14), legend.title=element_text(size=16, face="bold")) + 
  ggtitle("Model Performance - CTT")


#************************************************************************************************************************
# Plot overall accuracy for each method

error <- rbind(knn.error, cart.error, rf.error, lda.error, svm.error)
Mean <- rowMeans(error)*100
Median <- rowMedians(error)*100
SD <- rowSds(error)*100  
Method <- c("KNN","CART","RF","LDA","SVM")
error.stats <- data.frame(Method, Mean, Median, SD)
error.stats
error <- t(error)
error <- as.data.frame(error)
names(error) <- c("KNN","CART","RF","LDA","SVM")
error$CV <- as.factor(c("CV1","CV2","CV3","CV4","CV5","CV6","CV7","CV8","CV9","CV10"))
error2 <- gather(error, key="Method", value="CVmean", 1:5)
error2$CVmean <- error2$CVmean*100

ggplot(error2, aes(x=Method, y=CVmean, fill=Method)) + geom_boxplot() + theme_bw(base_size=16) + scale_fill_discrete(guide='none') + theme(axis.text=element_text(size=14))


######################################################

# Classifying CTT devices

## Reclassify data with new training set 

# Run the RF model
samples <- sample(1:nrow(ctt.ss),size=0.7*nrow(ctt.ss))
train <- ctt.ss[samples,]
test <- ctt.ss[-samples,-1]
ctt.rf <- randomForest(factor(behavior)~., data=train, mtry=sqrt(no.beh), ntree=2000)

# Predict on the test set
ctt.pred <- predict(ctt.rf, newdata=test)

# Confustion matrix
test.behaviors <- ctt.ss$behavior[-samples]
rfcm <- table(ctt.pred, test.behaviors)
rfcm

# Precision and accuracy for each behavior
modelPerformance(rfcm)

# Overall accuracy 
mean(ctt.pred==test.behaviors) 

# Read in summary statistics
file.name <- list.files(path="data/ed.sumstatsCTT/", pattern=".csv", all.files=TRUE, full.names=TRUE)
file.list <- lapply(file.name, FUN=read.csv, header=TRUE, stringsAsFactors=FALSE)

# Read in wide-format data
wide <- list.files(path="data/wideCTT/", pattern=".csv", all.files=TRUE, full.names=TRUE)
wide <- lapply(wide, FUN=read.csv, header=TRUE, stringsAsFactors=FALSE)

# Classify each bird
for (i in 2:length(file.list)) {
  wide.ss <- file.list[[i]]
  
  ossb1 <- data.frame(burst=wide.ss$burst)
  o1 <- wide[[i]]
  o1 <- o1[,2:3]
  o1 <- semi_join(o1, ossb1, by="burst")
  
  wide.ss <- wide.ss[,-c(1:5,20,22,25,26)]
  
  # Classify the data using Random Forest
  rf.labels <- predict(ctt.rf, newdata=wide.ss)
  
  odba <- wide.ss$odba
  
  id1 <- strsplit(file.name[[i]], "[/_]")
  id <- id1[[1]][3]
  
  print(id)
  print(table(rf.labels))
  
  o1$timestamp <- as.POSIXct(o1$timestamp, tz="GMT")
  date <- format(o1$timestamp, "%Y-%m-%d")
  
  dat <- data.frame(id=id, date=date, o1, odba=odba, behavior=rf.labels)
  
  filename = paste0("output/classified/", id, "_class-odba.csv")
  write.csv(dat, filename)
  
}







