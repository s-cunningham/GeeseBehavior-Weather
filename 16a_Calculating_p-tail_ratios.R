library(tidyverse)

options(scipen=999)


# Set up matrices and data frames
mintemp <- matrix(NA, ncol=34, nrow=115)
mintemp <- as.data.frame(mintemp)

prate <- matrix(NA, ncol=34, nrow=115)
prate <- as.data.frame(prate)

# setwd("output/JAGS/ratioDLMs")
setwd("output/JAGS/ratioDLMs/20210824/")
files <- list.files("./",  pattern=".Rdata", all.files=TRUE, full.names=FALSE)

# smry <- list.files("G:/ResearchProjects/Behavior_and_Weather/output/modelcoefficients/dlm/",  pattern=".csv", all.files=TRUE, full.names=FALSE)

for (i in 1:length(files)) {
  
  # Load model output
  load(files[i])
  
  # Extract bird ID
  x <- str_split(files[i], "[:punct:]")
  x1 <- lapply(x, function(.x) if(is.vector(.x)) t(.x) else (.x))
  x2 <- lapply(x1, `[`,,4)
  x <- unlist(x2)
  
  # save sims list
  beta1 <- out1$sims.list$beta1
  beta2 <- out1$sims.list$beta2
  
  for (j in 1:dim(beta1)[2]) {
    prate[j,i] <- ifelse(unique(is.na(beta1[,j])),NA,sum(beta1[,j]>0)/105000)
    names(prate)[i] <- x
    
    mintemp[j,i] <- ifelse(unique(is.na(beta2[,j])),NA,sum(beta2[,j]>0)/105000)
    names(mintemp)[i] <- x
  }
  
  print(x)
  print(dim(beta1)[2])
  print(dim(beta2)[2])
}

setwd("G:/ResearchProjects/Behavior_and_Weather")

write.csv(prate, "output/20210901prate_ptail.csv")
write.csv(mintemp, "output/20210901mintemp_ptail.csv")

setwd("output/modelcoefficients/dlm")





# dat <- list.files("./",  pattern=".csv", all.files=TRUE, full.names=FALSE)
# 
# x <- str_split(dat, "[:punct:]")
# x1 <- lapply(x, function(.x) if(is.vector(.x)) t(.x) else (.x))
# x2 <- lapply(x1, `[`,,4])
# x <- unlist(x2)
# 
# dat <- lapply(dat, FUN=read.csv, header=TRUE, stringsAsFactors=FALSE)
# setwd("G:/ResearchProjects/Behavior_and_Weather")
# 
# 
# for (i in 1:36) {
#   dat[[i]]$animal_id <- x[i]
# }
# 
# dat <- do.call("rbind", dat)
# 
# 
# hist(dat$Rhat)
# 
# 
# unique(dat[dat$Rhat>1.1,13])

