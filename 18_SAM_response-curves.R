#**********************************************************************************************************************************
#**********************************************************************************************************************************

# Project: Quantifying the influence of behavioral contributions to reproductive attempts in geese of contrasting migration strategy
# Date: 3 June 2020
# Author: Stephanie Cunningham
# Description: SAM response curves

#**********************************************************************************************************************************
#**********************************************************************************************************************************

library(tidyverse)
library(gridExtra)
library(boot)
library(jagsUI)

rm(list=ls())

# Load JAGS objects
load("output/ecomem_NAMCodba3.Rdata")  # out1
# print(out1, digits=3)
load("output/ecomem_NAMC3ptf.Rdata")  # out2
# print(out2, digits=3)
load("output/ecomem_GRLDodba3.Rdata")  # out3
# print(out3, digits=3)
load("output/ecomem_GRLD3ptf.Rdata")  # out4
# print(out4, digits=3)


#### ODBA
## NAMC
# Setting up posterior samples
beta0 <- c(out1$samples[[1]][,1], out1$samples[[2]][,1], out1$samples[[3]][,1])
beta1 <- c(out1$samples[[1]][,2], out1$samples[[2]][,2], out1$samples[[3]][,2])  # effect of PTF
beta2 <- c(out1$samples[[1]][,3], out1$samples[[2]][,3], out1$samples[[3]][,3])  # effect of year (2018 compaterd to 2017)

# Predicting probability of a breeding attempt
nmcmc <- out1$mcmc.info$n.samples
pred_length <- 100
odba_pred <- seq(-1,1,length.out=pred_length)

out1.yr1 <- matrix(, nmcmc, pred_length)
out1.yr2 <- matrix(, nmcmc, pred_length)

for (j in 1:pred_length) {
  out1.yr1[,j] <- exp(beta0 + beta1*odba_pred[j] + beta2*0)
  out1.yr2[,j] <- exp(beta0 + beta1*odba_pred[j] + beta2*1)
}

out1.yr1 <- out1.yr1 / (out1.yr1 + 1)
out1.yr1.qt <- apply(out1.yr1, 2, quantile, probs=c(.5, .025, .975))

out1.yr2 <- out1.yr2 / (out1.yr2 + 1)
out1.yr2.qt <- apply(out1.yr2, 2, quantile, probs=c(.5, .025, .975))


df1 <- data.frame(y=out1.yr1.qt[1,], x=odba_pred, up1=out1.yr1.qt[2,], lo1=out1.yr1.qt[3,])
df2 <- data.frame(y=out1.yr2.qt[1,], x=odba_pred, up1=out1.yr2.qt[2,], lo1=out1.yr2.qt[3,])

p1 <- ggplot(df1, aes(x=x, y=y)) + geom_ribbon(aes(x=odba_pred, ymin=lo1, ymax=up1), alpha=0.4, fill="red") + 
  geom_ribbon(data=df2, aes(x=odba_pred, ymin=lo1, ymax=up1), alpha=0.4, fill="blue") + 
  geom_line(color="red", size=1) + geom_line(data=df2, aes(x=x, y=y), color="blue", size=1) +
  theme_classic()  + ggtitle("Midcontinent") +  
  ylab("Predicted Probability\nof Breeding Attempt") + xlab("Antecedent ODBA") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16, face="bold"), 
        panel.border=element_rect(color="black", fill=NA, size=0.5))

## GRLD
# Setting up posterior samples
beta0 <- c(out3$samples[[1]][,1], out3$samples[[2]][,1], out3$samples[[3]][,1])
beta1 <- c(out3$samples[[1]][,2], out3$samples[[2]][,2], out3$samples[[3]][,2])
beta2 <- c(out3$samples[[1]][,3], out3$samples[[2]][,3], out3$samples[[3]][,3])
beta3 <- c(out3$samples[[1]][,4], out3$samples[[2]][,4], out3$samples[[3]][,4])

# Predicting probability of a breeding attempt
nmcmc <- out1$mcmc.info$n.samples
pred_length <- 100
odba_pred <- seq(-1,1,length.out=pred_length)

out3.yr1 <- matrix(, nmcmc, pred_length)
out3.yr2 <- matrix(, nmcmc, pred_length)
out3.yr3 <- matrix(, nmcmc, pred_length)

for (j in 1:pred_length) {
  out3.yr1[,j] <- exp(beta0 + beta1*odba_pred[j] + beta2*0 + beta3*0) # 2018
  out3.yr2[,j] <- exp(beta0 + beta1*odba_pred[j] + beta2*1 + beta3*0) # 2012
  out3.yr3[,j] <- exp(beta0 + beta1*odba_pred[j] + beta2*0 + beta3*1) # 2013
}

out3.yr1 <- out3.yr1 / (out3.yr1 + 1)
out3.yr1.qt <- apply(out3.yr1, 2, quantile, probs=c(.5, .025, .975))

out3.yr2 <- out1.yr2 / (out3.yr2 + 1)
out3.yr2.qt <- apply(out3.yr2, 2, quantile, probs=c(.5, .025, .975))

out3.yr3 <- out3.yr3 / (out3.yr3 + 1)
out3.yr3.qt <- apply(out3.yr3, 2, quantile, probs=c(.5, .025, .975))

df1 <- data.frame(y=out3.yr1.qt[1,], x=odba_pred, up1=out3.yr1.qt[2,], lo1=out3.yr1.qt[3,])
df2 <- data.frame(y=out3.yr2.qt[1,], x=odba_pred, up1=out3.yr2.qt[2,], lo1=out3.yr2.qt[3,])
df3 <- data.frame(y=out3.yr3.qt[1,], x=odba_pred, up1=out3.yr3.qt[2,], lo1=out3.yr3.qt[3,])

p2 <- ggplot(df1, aes(x=x, y=y)) + geom_ribbon(aes(x=odba_pred, ymin=lo1, ymax=up1), alpha=0.4, fill="red") + 
  geom_ribbon(data=df2, aes(x=odba_pred, ymin=lo1, ymax=up1), alpha=0.4, fill="blue") + 
  geom_ribbon(data=df3, aes(x=odba_pred, ymin=lo1, ymax=up1), alpha=0.4, fill="green") + 
  geom_line(color="red", size=1) + # 2018
  geom_line(data=df2, aes(x=x, y=y), color="blue", size=1) + # 2012
  geom_line(data=df3, aes(x=x, y=y), color="green", size=1) + # 2012
  theme_classic() + ggtitle("Greenland") + 
  ylab("Predicted Probability\nof Breeding Attempt") + xlab("Antecedent ODBA") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16, face="bold"), 
        panel.border=element_rect(color="black", fill=NA, size=0.5))


#### PTF
## NAMC
# Setting up posterior samples
beta0 <- c(out1$samples[[1]][,1], out1$samples[[2]][,1], out1$samples[[3]][,1])
beta1 <- c(out1$samples[[1]][,2], out1$samples[[2]][,2], out1$samples[[3]][,2])  # effect of PTF
beta2 <- c(out1$samples[[1]][,3], out1$samples[[2]][,3], out1$samples[[3]][,3])  # effect of year (2018 compaterd to 2017)

# Predicting probability of a breeding attempt
nmcmc <- out2$mcmc.info$n.samples
pred_length <- 100
ptf_pred <- seq(-0.55,0.55,length.out=pred_length)

out2.yr1 <- matrix(, nmcmc, pred_length)
out2.yr2 <- matrix(, nmcmc, pred_length)
for (j in 1:pred_length) {
  out2.yr1[,j] <- exp(beta0 + beta1*ptf_pred[j] + beta2*0)
  out2.yr2[,j] <- exp(beta0 + beta1*ptf_pred[j] + beta2*1)
}

out2.yr1 <- out2.yr1 / (out2.yr1 + 1)
out2.yr1.qt <- apply(out2.yr1, 2, quantile, probs=c(.5, .025, .975))

out2.yr2 <- out2.yr2 / (out2.yr2 + 1)
out2.yr2.qt <- apply(out2.yr2, 2, quantile, probs=c(.5, .025, .975))


df1 <- data.frame(y=out2.yr1.qt[1,], x=ptf_pred, up1=out2.yr1.qt[2,], lo1=out2.yr1.qt[3,])
df2 <- data.frame(y=out2.yr2.qt[1,], x=ptf_pred, up1=out2.yr2.qt[2,], lo1=out2.yr2.qt[3,])

p3 <- ggplot(df1, aes(x=x, y=y)) + geom_ribbon(aes(x=ptf_pred, ymin=lo1, ymax=up1), alpha=0.4, fill="red") + 
  geom_ribbon(data=df2, aes(x=ptf_pred, ymin=lo1, ymax=up1), alpha=0.4, fill="blue") + 
  geom_line(color="red", size=1) + geom_line(data=df2, aes(x=x, y=y), color="blue", size=1) +
  theme_classic() + ggtitle("Midcontinent") +  
  ylab("Predicted Probability\nof Breeding Attempt") + xlab("Antecedent PTF") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16, face="bold"), 
        panel.border=element_rect(color="black", fill=NA, size=0.5))

## GRLD
# Setting up posterior samples
beta0 <- c(out4$samples[[1]][,1], out4$samples[[2]][,1], out4$samples[[3]][,1])
beta1 <- c(out4$samples[[1]][,2], out4$samples[[2]][,2], out4$samples[[3]][,2])
beta2 <- c(out4$samples[[1]][,3], out4$samples[[2]][,3], out4$samples[[3]][,3])
beta3 <- c(out4$samples[[1]][,4], out4$samples[[2]][,4], out4$samples[[3]][,4])

# Predicting probability of a breeding attempt
nmcmc <- out1$mcmc.info$n.samples
pred_length <- 100
ptf_pred <- seq(-0.55,0.55,length.out=pred_length)

out4.yr1 <- matrix(, nmcmc, pred_length)
out4.yr2 <- matrix(, nmcmc, pred_length)
out4.yr3 <- matrix(, nmcmc, pred_length)

for (j in 1:pred_length) {
  out4.yr1[,j] <- exp(beta0 + beta1*ptf_pred[j] + beta2*0 + beta3*0) # 2018
  out4.yr2[,j] <- exp(beta0 + beta1*ptf_pred[j] + beta2*1 + beta3*0) # 2012
  out4.yr3[,j] <- exp(beta0 + beta1*ptf_pred[j] + beta2*0 + beta3*1) # 2013
}

out4.yr1 <- out4.yr1 / (out4.yr1 + 1)
out4.yr1.qt <- apply(out4.yr1, 2, quantile, probs=c(.5, .025, .975))

out4.yr2 <- out1.yr2 / (out4.yr2 + 1)
out4.yr2.qt <- apply(out4.yr2, 2, quantile, probs=c(.5, .025, .975))

out4.yr3 <- out4.yr3 / (out4.yr3 + 1)
out4.yr3.qt <- apply(out4.yr3, 2, quantile, probs=c(.5, .025, .975))

df1 <- data.frame(y=out4.yr1.qt[1,], x=ptf_pred, up1=out4.yr1.qt[2,], lo1=out4.yr1.qt[3,])
df2 <- data.frame(y=out4.yr2.qt[1,], x=ptf_pred, up1=out4.yr2.qt[2,], lo1=out4.yr2.qt[3,])
df3 <- data.frame(y=out4.yr3.qt[1,], x=ptf_pred, up1=out4.yr3.qt[2,], lo1=out4.yr3.qt[3,])

p4 <- ggplot(df1, aes(x=x, y=y)) + geom_ribbon(aes(x=ptf_pred, ymin=lo1, ymax=up1), alpha=0.4, fill="red") + 
  geom_ribbon(data=df2, aes(x=ptf_pred, ymin=lo1, ymax=up1), alpha=0.4, fill="blue") + 
  geom_ribbon(data=df3, aes(x=ptf_pred, ymin=lo1, ymax=up1), alpha=0.4, fill="green") + 
  geom_line(color="red", size=1) + 
  geom_line(data=df2, aes(x=x, y=y), color="blue", size=1) +
  geom_line(data=df3, aes(x=x, y=y), color="green", size=1) +
  theme_classic() + ggtitle("Greenland") + 
  ylab("Predicted Probability\nof Breeding Attempt") + xlab("Antecedent PTF") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16, face="bold"), 
        panel.border=element_rect(color="black", fill=NA, size=0.5))


lay <- rbind(c(1,2),
             c(3,4))

grid.arrange(p1, p2, p3, p4, layout_matrix=lay)
