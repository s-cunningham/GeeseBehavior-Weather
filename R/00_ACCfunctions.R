
# Function for calculating summary statistics on ACC data

library(moments)
library(caTools)

# Calculate summary statistics
accSumStatsORN <- function(x) {
  y <- data.frame()
  for (i in 1:nrow(x)) {
    temp <- x[i,]
    # Ornitela
    X <- as.vector(as.numeric(temp[,3:32]))  # HEAVE
    Y <- as.vector(as.numeric(temp[,33:62])) # SURGE
    Z <- as.vector(as.numeric(temp[,63:92])) # SWAY
    # Static.X <- mean(X)
    # Static.Y <- mean(Y)
    # Static.Z <- mean(Z)
    Static.X <- runmean(X, 9, alg="C", endrule="mean")
    Static.Y <- runmean(Y, 9, alg="C", endrule="mean")
    Static.Z <- runmean(Z, 9, alg="C", endrule="mean")
    Dynamic.X <- X - Static.X
    Dynamic.Y <- Y - Static.Y
    Dynamic.Z <- Z - Static.Z
    odba <- mean(abs(Dynamic.X) + abs(Dynamic.Y) + abs(Dynamic.Z))
    vedba <- mean(sqrt((Dynamic.X^2)+(Dynamic.Y^2)+(Dynamic.Z^2)))
    # diff.dxy <- temp$X-temp$Y
    # diff.dyz <- temp$X-temp$Y
    # diff.dxz <- temp$X-temp$Z
    mean.dxy <- mean(X-Y)
    mean.dyz <- mean(Y-Z)
    mean.dxz <- mean(X-Z)
    sd.dxy <- sd(Dynamic.X-Dynamic.Y)
    sd.dyz <- sd(Dynamic.Y-Dynamic.Z)
    sd.dxz <- sd(Dynamic.X-Dynamic.Z)
    sk.dx <- skewness(Dynamic.X)
    ku.dx <- kurtosis(Dynamic.X)
    sk.dy <- skewness(Dynamic.Y)
    ku.dy <- kurtosis(Dynamic.Y)
    sk.dz <- skewness(Dynamic.Z)
    ku.dz <- kurtosis(Dynamic.Z)
    cov.dxy <- cov(Dynamic.X, Dynamic.Y)
    cov.dxz <- cov(Dynamic.X, Dynamic.Z)
    cov.dyz <- cov(Dynamic.Y, Dynamic.Z)
    cor.dxy <- cor(Dynamic.X, Dynamic.Y)
    cor.dxz <- cor(Dynamic.X, Dynamic.Z)
    cor.dyz <- cor(Dynamic.Y, Dynamic.Z)
    quant.dx <- quantile(Dynamic.X, na.rm=FALSE)
    quant.dy <- quantile(Dynamic.Y, na.rm=FALSE)
    quant.dz <- quantile(Dynamic.Z, na.rm=FALSE)
    max.dx <- max(Dynamic.X)
    min.dx <- min(Dynamic.X)
    amp.dx <- abs(max.dx-min.dx)
    max.dy <- max(Dynamic.Y)
    min.dy <- min(Dynamic.Y)
    amp.dy <- abs(max.dy-min.dy)
    max.dz <- max(Dynamic.Z)
    min.dz <- min(Dynamic.Z)
    amp.dz <- abs(max.dz-min.dz)
    features <- data.frame(Static.X=sum(abs(Static.X)), Static.Y=sum(abs(Static.Y)), Static.Z=sum(abs(Static.Z)),
                           Dynamic.X=sum(abs(Dynamic.X)), Dynamic.Y=sum(abs(Dynamic.Y)), Dynamic.Z=sum(abs(Dynamic.Z)),
                           odba=odba, vedba=vedba,
                           mean.dxy=mean.dxy,mean.dyz=mean.dyz,mean.dxz=mean.dxz,
                           sd.dxy=sd.dxy,sd.dyz=sd.dyz,sd.dxz=sd.dxz,
                           cov.dxy=cov.dxy,cov.dxz=cov.dxz,cov.dyz=cov.dyz,
                           cor.dxy=cor.dxy,cor.dxz=cor.dxz,cor.dyz=cor.dyz,
                           sk.dx=sk.dx,ku.dx=ku.dx,
                           sk.dy=sk.dy,ku.dy=ku.dy,
                           sk.dz=sk.dz,ku.dz=ku.dz,
                           pct25.dx=quant.dx[2],pct50.dx=quant.dx[3],pct75.dx=quant.dx[4],
                           pct25.dy=quant.dy[2],pct50.dy=quant.dy[3],pct75.dy=quant.dy[4],
                           pct25.dz=quant.dz[2],pct50.dz=quant.dz[3],pct75.dz=quant.dz[4],
                           max.dx=max.dx,min.dx=min.dx, amp.dx=amp.dx,
                           max.dy=max.dy,min.dy=min.dy, amp.dy=amp.dy,
                           max.dz=max.dz,min.dz=min.dz, amp.dz=amp.dz)
    temp2 <- cbind.data.frame(behavior=temp[,2], features, row.names=NULL)
    y <- rbind(y,temp2, row.names=NULL)
  }
  row.names(y) <- seq(1,nrow(y),1)
  return(y)
}

# Calculate summary statistics
accSumStatsCTT <- function(x) {
  y <- data.frame()
  for (i in 1:nrow(x)) {
    temp <- x[i,]
    # CTT
    X <- as.vector(as.numeric(temp[,3:31]))  # HEAVE
    Y <- as.vector(as.numeric(temp[,32:60])) # SURGE
    Z <- as.vector(as.numeric(temp[,61:89])) # SWAY
    # Static.X <- mean(X)
    # Static.Y <- mean(Y)
    # Static.Z <- mean(Z)
    Static.X <- runmean(X, 9, alg="C", endrule="mean")
    Static.Y <- runmean(Y, 9, alg="C", endrule="mean")
    Static.Z <- runmean(Z, 9, alg="C", endrule="mean")
    Dynamic.X <- X - Static.X
    Dynamic.Y <- Y - Static.Y
    Dynamic.Z <- Z - Static.Z
    odba <- mean(abs(Dynamic.X) + abs(Dynamic.Y) + abs(Dynamic.Z))
    vedba <- mean(sqrt((Dynamic.X^2)+(Dynamic.Y^2)+(Dynamic.Z^2)))
    # diff.dxy <- temp$X-temp$Y
    # diff.dyz <- temp$X-temp$Y
    # diff.dxz <- temp$X-temp$Z
    mean.dxy <- mean(X-Y)
    mean.dyz <- mean(Y-Z)
    mean.dxz <- mean(X-Z)
    sd.dxy <- sd(Dynamic.X-Dynamic.Y)
    sd.dyz <- sd(Dynamic.Y-Dynamic.Z)
    sd.dxz <- sd(Dynamic.X-Dynamic.Z)
    sk.dx <- skewness(Dynamic.X)
    ku.dx <- kurtosis(Dynamic.X)
    sk.dy <- skewness(Dynamic.Y)
    ku.dy <- kurtosis(Dynamic.Y)
    sk.dz <- skewness(Dynamic.Z)
    ku.dz <- kurtosis(Dynamic.Z)
    cov.dxy <- cov(Dynamic.X, Dynamic.Y)
    cov.dxz <- cov(Dynamic.X, Dynamic.Z)
    cov.dyz <- cov(Dynamic.Y, Dynamic.Z)
    cor.dxy <- cor(Dynamic.X, Dynamic.Y)
    cor.dxz <- cor(Dynamic.X, Dynamic.Z)
    cor.dyz <- cor(Dynamic.Y, Dynamic.Z)
    quant.dx <- quantile(Dynamic.X)
    quant.dy <- quantile(Dynamic.Y)
    quant.dz <- quantile(Dynamic.Z)
    max.dx <- max(Dynamic.X)
    min.dx <- min(Dynamic.X)
    amp.dx <- abs(max.dx-min.dx)
    max.dy <- max(Dynamic.Y)
    min.dy <- min(Dynamic.Y)
    amp.dy <- abs(max.dy-min.dy)
    max.dz <- max(Dynamic.Z)
    min.dz <- min(Dynamic.Z)
    amp.dz <- abs(max.dz-min.dz)
    features <- data.frame(Static.X=sum(abs(Static.X)), Static.Y=sum(abs(Static.Y)), Static.Z=sum(abs(Static.Z)),
                           Dynamic.X=sum(abs(Dynamic.X)), Dynamic.Y=sum(abs(Dynamic.Y)), Dynamic.Z=sum(abs(Dynamic.Z)),
                           odba=odba, vedba=vedba,
                           mean.dxy=mean.dxy,mean.dyz=mean.dyz,mean.dxz=mean.dxz,
                           sd.dxy=sd.dxy,sd.dyz=sd.dyz,sd.dxz=sd.dxz,
                           cov.dxy=cov.dxy,cov.dxz=cov.dxz,cov.dyz=cov.dyz,
                           cor.dxy=cor.dxy,cor.dxz=cor.dxz,cor.dyz=cor.dyz,
                           sk.dx=sk.dx,ku.dx=ku.dx,
                           sk.dy=sk.dy,ku.dy=ku.dy,
                           sk.dz=sk.dz,ku.dz=ku.dz,
                           pct25.dx=quant.dx[2],pct50.dx=quant.dx[3],pct75.dx=quant.dx[4],
                           pct25.dy=quant.dy[2],pct50.dy=quant.dy[3],pct75.dy=quant.dy[4],
                           pct25.dz=quant.dz[2],pct50.dz=quant.dz[3],pct75.dz=quant.dz[4],
                           max.dx=max.dx,min.dx=min.dx,amp.dx=amp.dx,
                           max.dy=max.dy,min.dy=min.dy,amp.dy=amp.dy,
                           max.dz=max.dz,min.dz=min.dz,amp.dz=amp.dz)
    temp2 <- cbind.data.frame(behavior=temp[,2], features, row.names=NULL)
    y <- rbind(y,temp2, row.names=NULL)
  }
  row.names(y) <- seq(1,nrow(y),1)
  return(y)
}

# Calculate summary statistics
accSumStatsEOBStrain <- function(x) {
  y <- data.frame()
  for (i in 1:nrow(x)) {
    temp <- x[i,]
    X <- as.vector(as.numeric(temp[,3:84]))  # HEAVE
    Y <- as.vector(as.numeric(temp[,85:166])) # SURGE
    Z <- as.vector(as.numeric(temp[,167:248]))  # SWAY
    Static.X <- runmean(X, 9, alg="C", endrule="mean")
    Static.Y <- runmean(Y, 9, alg="C", endrule="mean")
    Static.Z <- runmean(Z, 9, alg="C", endrule="mean")
    Dynamic.X <- X - Static.X
    Dynamic.Y <- Y - Static.Y
    Dynamic.Z <- Z - Static.Z
    odba <- mean(abs(Dynamic.X) + abs(Dynamic.Y) + abs(Dynamic.Z))
    vedba <- mean(sqrt((Dynamic.X^2)+(Dynamic.Y^2)+(Dynamic.Z^2)))
    mean.dxy <- mean(X-Y)
    mean.dyz <- mean(Y-Z)
    mean.dxz <- mean(X-Z)
    sd.dxy <- sd(Dynamic.X-Dynamic.Y)
    sd.dyz <- sd(Dynamic.Y-Dynamic.Z)
    sd.dxz <- sd(Dynamic.X-Dynamic.Z)
    sk.dx <- skewness(Dynamic.X)
    ku.dx <- kurtosis(Dynamic.X)
    sk.dy <- skewness(Dynamic.Y)
    ku.dy <- kurtosis(Dynamic.Y)
    sk.dz <- skewness(Dynamic.Z)
    ku.dz <- kurtosis(Dynamic.Z)
    cov.dxy <- cov(Dynamic.X, Dynamic.Y)
    cov.dxz <- cov(Dynamic.X, Dynamic.Z)
    cov.dyz <- cov(Dynamic.Y, Dynamic.Z)
    cor.dxy <- cor(Dynamic.X, Dynamic.Y)
    cor.dxz <- cor(Dynamic.X, Dynamic.Z)
    cor.dyz <- cor(Dynamic.Y, Dynamic.Z)
    quant.dx <- quantile(Dynamic.X, na.rm=FALSE)
    quant.dy <- quantile(Dynamic.Y, na.rm=FALSE)
    quant.dz <- quantile(Dynamic.Z, na.rm=FALSE)
    max.dx <- max(Dynamic.X)
    min.dx <- min(Dynamic.X)
    amp.dx <- abs(max.dx-min.dx)
    max.dy <- max(Dynamic.Y)
    min.dy <- min(Dynamic.Y)
    amp.dy <- abs(max.dy-min.dy)
    max.dz <- max(Dynamic.Z)
    min.dz <- min(Dynamic.Z)
    amp.dz <- abs(max.dz-min.dz)
    features <- data.frame(Static.X=sum(abs(Static.X)), Static.Y=sum(abs(Static.Y)), Static.Z=sum(abs(Static.Z)),
                           Dynamic.X=sum(abs(Dynamic.X)), Dynamic.Y=sum(abs(Dynamic.Y)), Dynamic.Z=sum(abs(Dynamic.Z)),
                           odba=odba, vedba=vedba,
                           mean.dxy=mean.dxy,mean.dyz=mean.dyz,mean.dxz=mean.dxz,
                           sd.dxy=sd.dxy,sd.dyz=sd.dyz,sd.dxz=sd.dxz,
                           cov.dxy=cov.dxy,cov.dxz=cov.dxz,cov.dyz=cov.dyz,
                           cor.dxy=cor.dxy,cor.dxz=cor.dxz,cor.dyz=cor.dyz,
                           sk.dx=sk.dx,ku.dx=ku.dx,
                           sk.dy=sk.dy,ku.dy=ku.dy,
                           sk.dz=sk.dz,ku.dz=ku.dz,
                           pct25.dx=quant.dx[2],pct50.dx=quant.dx[3],pct75.dx=quant.dx[4],
                           pct25.dy=quant.dy[2],pct50.dy=quant.dy[3],pct75.dy=quant.dy[4],
                           pct25.dz=quant.dz[2],pct50.dz=quant.dz[3],pct75.dz=quant.dz[4],
                           max.dx=max.dx,min.dx=min.dx, amp.dx=amp.dx,
                           max.dy=max.dy,min.dy=min.dy, amp.dy=amp.dy,
                           max.dz=max.dz,min.dz=min.dz, amp.dz=amp.dz)
    temp2 <- cbind.data.frame(behavior=temp[,2], features, row.names=NULL)
    y <- rbind(y,temp2, row.names=NULL)
  }
  row.names(y) <- seq(1,nrow(y),1)
  return(y)
}

# Calculate summary statistics
accSumStatsEOBS <- function(x) {
  y <- data.frame()
  for (i in 1:nrow(x)) {
    temp <- x[i,]
    # CTT
    X <- as.vector(as.numeric(temp[,3:28]))  # HEAVE
    Y <- as.vector(as.numeric(temp[,29:54])) # SURGE
    Z <- as.vector(as.numeric(temp[,55:80])) # SWAY
    # Static.X <- mean(X)
    # Static.Y <- mean(Y)
    # Static.Z <- mean(Z)
    Static.X <- runmean(X, 9, alg="C", endrule="mean")
    Static.Y <- runmean(Y, 9, alg="C", endrule="mean")
    Static.Z <- runmean(Z, 9, alg="C", endrule="mean")
    Dynamic.X <- X - Static.X
    Dynamic.Y <- Y - Static.Y
    Dynamic.Z <- Z - Static.Z
    odba <- mean(abs(Dynamic.X) + abs(Dynamic.Y) + abs(Dynamic.Z))
    vedba <- mean(sqrt((Dynamic.X^2)+(Dynamic.Y^2)+(Dynamic.Z^2)))
    # diff.dxy <- temp$X-temp$Y
    # diff.dyz <- temp$X-temp$Y
    # diff.dxz <- temp$X-temp$Z
    mean.dxy <- mean(X-Y)
    mean.dyz <- mean(Y-Z)
    mean.dxz <- mean(X-Z)
    sd.dxy <- sd(Dynamic.X-Dynamic.Y)
    sd.dyz <- sd(Dynamic.Y-Dynamic.Z)
    sd.dxz <- sd(Dynamic.X-Dynamic.Z)
    sk.dx <- skewness(Dynamic.X)
    ku.dx <- kurtosis(Dynamic.X)
    sk.dy <- skewness(Dynamic.Y)
    ku.dy <- kurtosis(Dynamic.Y)
    sk.dz <- skewness(Dynamic.Z)
    ku.dz <- kurtosis(Dynamic.Z)
    cov.dxy <- cov(Dynamic.X, Dynamic.Y)
    cov.dxz <- cov(Dynamic.X, Dynamic.Z)
    cov.dyz <- cov(Dynamic.Y, Dynamic.Z)
    cor.dxy <- cor(Dynamic.X, Dynamic.Y)
    cor.dxz <- cor(Dynamic.X, Dynamic.Z)
    cor.dyz <- cor(Dynamic.Y, Dynamic.Z)
    quant.dx <- quantile(Dynamic.X)
    quant.dy <- quantile(Dynamic.Y)
    quant.dz <- quantile(Dynamic.Z)
    max.dx <- max(Dynamic.X)
    min.dx <- min(Dynamic.X)
    amp.dx <- abs(max.dx-min.dx)
    max.dy <- max(Dynamic.Y)
    min.dy <- min(Dynamic.Y)
    amp.dy <- abs(max.dy-min.dy)
    max.dz <- max(Dynamic.Z)
    min.dz <- min(Dynamic.Z)
    amp.dz <- abs(max.dz-min.dz)
    features <- data.frame(Static.X=sum(abs(Static.X)), Static.Y=sum(abs(Static.Y)), Static.Z=sum(abs(Static.Z)),
                           Dynamic.X=sum(abs(Dynamic.X)), Dynamic.Y=sum(abs(Dynamic.Y)), Dynamic.Z=sum(abs(Dynamic.Z)),
                           odba=odba, vedba=vedba,
                           mean.dxy=mean.dxy,mean.dyz=mean.dyz,mean.dxz=mean.dxz,
                           sd.dxy=sd.dxy,sd.dyz=sd.dyz,sd.dxz=sd.dxz,
                           cov.dxy=cov.dxy,cov.dxz=cov.dxz,cov.dyz=cov.dyz,
                           cor.dxy=cor.dxy,cor.dxz=cor.dxz,cor.dyz=cor.dyz,
                           sk.dx=sk.dx,ku.dx=ku.dx,
                           sk.dy=sk.dy,ku.dy=ku.dy,
                           sk.dz=sk.dz,ku.dz=ku.dz,
                           pct25.dx=quant.dx[2],pct50.dx=quant.dx[3],pct75.dx=quant.dx[4],
                           pct25.dy=quant.dy[2],pct50.dy=quant.dy[3],pct75.dy=quant.dy[4],
                           pct25.dz=quant.dz[2],pct50.dz=quant.dz[3],pct75.dz=quant.dz[4],
                           max.dx=max.dx,min.dx=min.dx,amp.dx=amp.dx,
                           max.dy=max.dy,min.dy=min.dy,amp.dy=amp.dy,
                           max.dz=max.dz,min.dz=min.dz,amp.dz=amp.dz)
    temp2 <- cbind.data.frame(burst=temp[,2], features, row.names=NULL)
    y <- rbind(y,temp2, row.names=NULL)
  }
  row.names(y) <- seq(1,nrow(y),1)
  return(y)
}


#Function to calculate model performace from a given confusion matrix table
# Will need to edit for number of behaviors
modelPerformance <- function(x) {
  # Convert the confusion matrix table to a data frame
  y <- as.data.frame.matrix(x) 
  # Set up matrix to show percent of observations classified in each category 
  cf.pct <- matrix(NA, nrow=ncol(y), ncol=ncol(y))
  row.totals <- rowSums(y)
  for (i in 1:ncol(y)) {
    for (j in 1:ncol(y)) {
      cf.pct[i,j] <- (y[i,j]/row.totals[i])*100 
    }
  }
  in.out <- names(y)
  cf.pct <- data.frame(in.out, cf.pct)
  names(cf.pct)[2:ncol(cf.pct)] <- names(y)
  # Set up matrix with TP, TN, FP, FN for each behavior
  perf.prep <- matrix(NA, nrow=4, ncol=ncol(y))
  for (i in 1:ncol(y)) {
    perf.prep[1,i] <- y[i,i] # True positives
    perf.prep[2,i] <- sum(y[-c(i),-c(i)]) # True negatives
    perf.prep[3,i] <- sum(y[-c(i),i]) # False positives
    perf.prep[4,i] <- sum(y[i,-c(i)]) # False negatives
  }
  # Calculate accuracy for each behavior (proportion of correctly assinged data points)
  accuracy <- matrix(NA, nrow=1, ncol=ncol(y))
  for (i in 1:ncol(y)) {
    accuracy[1,i] <- ((perf.prep[1,i]+perf.prep[2,i])/(perf.prep[1,i]+perf.prep[2,i]+perf.prep[3,i]+perf.prep[4,i]))*100
  }
  # Calculate precision for each behavior (proportion of correct positive classifications)
  precision <- matrix(NA, nrow=1, ncol=ncol(y))
  for (i in 1:ncol(y)) {
    precision[1,i] <- (perf.prep[1,i]/(perf.prep[1,i]+perf.prep[3,i]))*100
  }
  # Calculate recall for each behavior ("proportion of data pertaining to behavioral modes that were classified correctly as positive" - Bidder et al. (2014))
  recall <- matrix(NA, nrow=1, ncol=ncol(y))
  for (i in 1:ncol(y)) {
    recall[1,i] <- (perf.prep[1,i]/(perf.prep[1,i]+perf.prep[4,i]))*100
  }
  # Combine into a single data frame
  mp <- as.data.frame(rbind(recall,precision,accuracy))
  names(mp) <- names(y)
  Measure <- c("Recall","Precision","Accuracy")
  mp <- cbind(Measure, mp)
  results <- list(cf.pct, mp)
  return(results)
}

# Function to add burst and index column to acc data. dt/UTC_datetime column needs to be converted to 'timestamp'
addBurst <- function(x) {
  #calculate the difference between two consecutive time points
  x <- mutate(x,lagtime=timestamp-lag(timestamp))
  
  #Following commands create a variable burst which repeats value for each unique burst
  lagtime=abs(x$lagtime)
  lagtime[is.na(lagtime)] <- 6
  burststart = c(6,lagtime[-1])>5
  burst = rep(0,dim(x)[1])
  burst2 = burst+burststart
  burst3 = cumsum(burst2)
  
  #creates an index within each burst 1:length(burst)
  runlen = rle(burst3)$lengths
  index = unlist(apply(matrix(runlen,length(runlen),1),1,seq,from=1,by=1))
  x$burst=burst3
  x$index=index
  return(x)
}






