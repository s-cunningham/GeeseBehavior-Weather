library(tidyverse)

options(scipen=999)

#### ODBA ####
# Set up matrices and data frames
mintemp <- matrix(NA, ncol=35, nrow=121)
mintemp <- as.data.frame(mintemp)

prcp <- matrix(NA, ncol=35, nrow=121)
prcp <- as.data.frame(prcp)

# Load file list
files <- list.files("E:/ResearchProjects/GeeseBehavior-Weather/results/dlmODBA",  pattern=".Rdata", all.files=TRUE, full.names=TRUE)

# Loop over each file, calculate number of posterior samples >0
for (i in 1:length(files)) {
  
  # Load model output
  load(files[i])
  
  # Extract bird ID
  x <- str_split(files[i], "[:punct:]")
  x <- unlist(x[[1]][11])
  
  # save sims list
  beta1 <- out$sims.list$beta1
  beta2 <- out$sims.list$beta2
  
  for (j in 1:dim(beta1)[2]) {
    prcp[j,i] <- ifelse(unique(is.na(beta1[,j])),NA,sum(beta1[,j]>0)/out$mcmc.info$n.samples)
    names(prcp)[i] <- x
    
    mintemp[j,i] <- ifelse(unique(is.na(beta2[,j])),NA,sum(beta2[,j]>0)/out$mcmc.info$n.samples)
    names(mintemp)[i] <- x
  }
  
  print(x)
  print(dim(beta1)[2])
  print(dim(beta2)[2])
}

# Save to file for plotting
write.csv(prcp, "results/ODBAprcp_ptail.csv")
write.csv(mintemp, "results/PTFmintemp_ptail.csv")

#### PTF ####
# Set up matrices and data frames
mintemp <- matrix(NA, ncol=35, nrow=121)
mintemp <- as.data.frame(mintemp)

prcp <- matrix(NA, ncol=35, nrow=121)
prcp <- as.data.frame(prcp)

# Load file list
files <- list.files("E:/ResearchProjects/GeeseBehavior-Weather/results/dlmPTF", 
                    pattern=".Rdata", all.files=TRUE, full.names=TRUE)

# Loop over each file, calculate number of posterior samples >0
for (i in 1:length(files)) {
  
  # Load model output
  load(files[i])
  
  # Extract bird ID
  x <- str_split(files[i], "[:punct:]")
  x <- unlist(x[[1]][11])
  
  # save sims list
  beta1 <- out$sims.list$beta1
  beta2 <- out$sims.list$beta2
  
  for (j in 1:dim(beta1)[2]) {
    prcp[j,i] <- ifelse(unique(is.na(beta1[,j])),NA,sum(beta1[,j]>0)/out$mcmc.info$n.samples)
    names(prcp)[i] <- x
    
    mintemp[j,i] <- ifelse(unique(is.na(beta2[,j])),NA,sum(beta2[,j]>0)/out$mcmc.info$n.samples)
    names(mintemp)[i] <- x
  }
  
  print(x)
  print(dim(beta1)[2])
  print(dim(beta2)[2])
}

# Save to file for plotting
write.csv(prcp, "results/ODBAprcp_ptail.csv")
write.csv(mintemp, "results/PTFmintemp_ptail.csv")



