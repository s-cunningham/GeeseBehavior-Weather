
load("G:/ResearchProjects/Behavior_and_Weather/data/PTF_mintempt-sims.Rdata")
# load("Downloads/odba_dlm.Rdata")

mintemp <- matrix(NA, ncol=37, nrow=115)
for (i in 1:37) {
  for (j in 1:115) {
    mintemp[j,i] <- ifelse(unique(is.na(beta2[,i,j])),NA,sum(beta2[,i,j]>0)/120000)
  }
}

write.csv(mintemp, "output/PTF_mintemp_proportions.csv")

load("F:/ResearchProjects/Behavior_and_Weather/data/PTF_prcp-sims.Rdata")
dim(beta1)

prate <- matrix(NA, ncol=37, nrow=115)
for (i in 1:37) {
  for (j in 1:115) {
    prate[j,i] <- ifelse(unique(is.na(beta1[,i,j])),NA,sum(beta1[,i,j]>0)/120000)
  }
}

hist(prate)
write.csv(prate, "output/PTF_prate_proportions.csv")



load("data/odba_mintemp-beta2.Rdata")
# load("Downloads/odba_dlm.Rdata")
dim(beta2)

mintemp <- matrix(NA, ncol=37, nrow=115)
for (i in 1:37) {
  for (j in 1:115) {
    mintemp[j,i] <- ifelse(unique(is.na(beta2[,i,j])),NA,sum(beta2[,i,j]>0)/120000)
  }
}

hist(mintemp)
write.csv(mintemp, "output/ODBA_mintemp_proportions.csv")


load("F:/ResearchProjects/Behavior_and_Weather/data/odba_prcp-beta1.Rdata")
dim(beta1)

prate <- matrix(NA, ncol=37, nrow=115)
for (i in 1:37) {
  for (j in 1:115) {
    prate[j,i] <- ifelse(unique(is.na(beta1[,i,j])),NA,sum(beta1[,i,j]>0)/120000)
  }
}

hist(prate)
write.csv(prate, "output/ODBA_prate_proportions.csv")




