library(tidyverse)
library(grid)
library(gridExtra)
library(pracma)
library(jagsUI)
library(boot)
library(DMwR)

options(scipen=999, digits=3)

#### ODBA ####
# Read in calculated proportions
b1.odba <- read.csv("output/ODBA_prate_proportions.csv")
b1.odba <- b1.odba[,-1]

b2.odba <- read.csv("output/ODBA_mintemp_proportions.csv")
b2.odba <- b2.odba[,-1]

# read data for bird ID numbers
bird <- read.csv("output/dlm_emm_data.csv", stringsAsFactors=FALSE)
bird <- bird[,-c(1,19:22,25,26)]
bird$date <- as.Date(bird$date)

# add IDs 
ids <- unique(bird$animal_id)
colnames(b1.odba) <- ids
colnames(b2.odba) <- ids

b1.odba <- gather(b1.odba, key="animal_id", value="PRATE.prop.g0", 1:37)
b2.odba <- gather(b2.odba, key="animal_id", value="MINTE.prop.g0", 1:37)
odba <- data.frame(animal_id=b1.odba$animal_id, PRATE.prop=b1.odba$PRATE.prop.g0, MINTE.prop=b2.odba$MINTE.prop.g0)
odba <- odba[complete.cases(odba),]
odba$rev.rel.day <- NA

un.id <- unique(odba$animal_id)
for (i in 1:length(un.id)) {
  odba[odba$animal_id==un.id[i],4] <- seq(1:nrow(odba[odba$animal_id==un.id[i],]))
}

# remove birds with not enough data
bird <- bird[bird$animal_id!="RP08F" & bird$animal_id!="RP15F" & bird$animal_id!="2164"
             & bird$animal_id!="2176" & bird$animal_id!="2160" & bird$animal_id!="2167",]

# Assign new numbers to reflect removal of shortened winters, and to make pop/reproductive status sequential
bird$pop2 <- 1
bird$pop2[bird$pop=="NAMC"] <- 2
N.order <- order(bird$year, decreasing = FALSE)
bird <- bird[N.order,]
N.order <- order(bird$attempt, decreasing=FALSE)
bird <- bird[N.order,]
N.order <- order(bird$pop2, decreasing = FALSE)
bird <- bird[N.order,]
id <- unique(bird$animal_id)
plot_id <- numeric(dim(bird)[1])
for (i in 1:dim(bird)[1]) {
  plot_id[i] <- which(id == bird$animal_id[i])
}
bird <- data.frame(bird, plot_id)
bird <- bird[,-20]

unique(cbind(bird$id_ind, bird$animal_id, bird$plot_id, bird$year))

# Join to posterior summary
dat <- left_join(bird, odba, by=c("animal_id", "rev.rel.day"))
dat <- dat[!is.na(dat$animal_id),]

# Add negative to ones with mean below 0
dat$attempt <- factor(dat$attempt, levels=c("0", "1"))

# Subset by covariate
odba.na <- dat[dat$pop=="NAMC",]
odba.gr <- dat[dat$pop=="GRLD",]

names(dat)[21:22] <- c("PRATE", "TEMP")

dat <- gather(dat, 21:22, key="variable", value="value")

dat$variable <- ifelse(dat$variable=="TEMP", "Minimum Temperature", "Precipitation Rate")
dat$pop <- ifelse(dat$pop=="GRLD", "Greenland", "North America") 
dat$attempt <- ifelse(dat$attempt==1, "Attempt", "Defer")

dat <- dat[,c(20,7,9,11,21:22)]

# dat$signif <- ifelse(dat$value >= 0.95 | dat$value <= 0.05, 1, 0)
dat$signif <- ifelse((dat$variable=="Precipitation Rate" & dat$value<=0.1) | 
                       (dat$variable=="Minimum Temperature" & dat$value>=0.9), 1, 0)

signif <- as.data.frame(dat %>% group_by(variable, pop, attempt, plot_id) %>% summarise(sum(signif)))
all <- as.data.frame(dat %>% group_by(variable, pop, attempt, plot_id) %>% count())
signif <- left_join(signif, all, by=c("variable", "pop", "attempt", "plot_id"))
names(signif)[5] <- "signif"
signif <- mutate(signif, prop.sig=signif/n)
# signif$prop.roun <- round(signif$prop.sig, digits=2)
signif$response <- "ODBA"
odba <- signif[,c(8,1:7)]

#### PTF ####
# Read in calculated proportions
b1.ptf <- read.csv("output/PTF_prate_proportions.csv")
b1.ptf <- b1.ptf[,-1]

b2.ptf <- read.csv("output/PTF_mintemp_proportions.csv")
b2.ptf <- b2.ptf[,-1]

# read data for bird ID numbers
bird <- read.csv("output/dlm_emm_data.csv", stringsAsFactors=FALSE)
bird <- bird[,-c(1,19:22,25,26)]
bird$date <- as.Date(bird$date)

# add IDs 
ids <- unique(bird$animal_id)
colnames(b1.ptf) <- ids
colnames(b2.ptf) <- ids

b1.ptf <- gather(b1.ptf, key="animal_id", value="PRATE.prop.g0", 1:37)
b2.ptf <- gather(b2.ptf, key="animal_id", value="MINTE.prop.g0", 1:37)
ptf <- data.frame(animal_id=b1.ptf$animal_id, PRATE.prop=b1.ptf$PRATE.prop.g0, MINTE.prop=b2.ptf$MINTE.prop.g0)
ptf <- ptf[complete.cases(ptf),]
ptf$rev.rel.day <- NA

un.id <- unique(ptf$animal_id)
for (i in 1:length(un.id)) {
  ptf[ptf$animal_id==un.id[i],4] <- seq(1:nrow(ptf[ptf$animal_id==un.id[i],]))
}

# remove birds with not enough data
bird <- bird[bird$animal_id!="RP08F" & bird$animal_id!="RP15F" & bird$animal_id!="2164"
             & bird$animal_id!="2176" & bird$animal_id!="2160" & bird$animal_id!="2167",]

# Assign new numbers to reflect removal of shortened winters, and to make pop/reproductive status sequential
bird$pop2 <- 1
bird$pop2[bird$pop=="NAMC"] <- 2
N.order <- order(bird$year, decreasing = FALSE)
bird <- bird[N.order,]
N.order <- order(bird$attempt, decreasing=FALSE)
bird <- bird[N.order,]
N.order <- order(bird$pop2, decreasing = FALSE)
bird <- bird[N.order,]
id <- unique(bird$animal_id)
plot_id <- numeric(dim(bird)[1])
for (i in 1:dim(bird)[1]) {
  plot_id[i] <- which(id == bird$animal_id[i])
}
bird <- data.frame(bird, plot_id)
bird <- bird[,-20]

# unique(cbind(bird$id_ind, bird$animal_id, bird$plot_id))

# Join to posterior summary
dat <- left_join(bird, ptf, by=c("animal_id", "rev.rel.day"))
dat <- dat[!is.na(dat$animal_id),]

# Add negative to ones with mean below 0
dat$attempt <- factor(dat$attempt, levels=c("0", "1"))

# Subset by covariate
ptf.na <- dat[dat$pop=="NAMC",]
ptf.gr <- dat[dat$pop=="GRLD",]

names(dat)[21:22] <- c("PRATE", "TEMP")

dat <- gather(dat, 21:22, key="variable", value="value")

dat$variable <- ifelse(dat$variable=="TEMP", "Minimum Temperature", "Precipitation Rate")
dat$pop <- ifelse(dat$pop=="GRLD", "Greenland", "North America") 
dat$attempt <- ifelse(dat$attempt==1, "Attempt", "Defer")

dat <- dat[,c(20,7,9,11,21:22)]

dat$signif <- ifelse((dat$variable=="Precipitation Rate" & dat$value<=0.1) | 
                       (dat$variable=="Minimum Temperature" & dat$value>=0.9), 1, 0)

signif <- as.data.frame(dat %>% group_by(variable, pop, attempt, plot_id) %>% summarise(sum(signif)))
all <- as.data.frame(dat %>% group_by(variable, pop, attempt, plot_id) %>% count())
signif <- left_join(signif, all, by=c("variable", "pop", "attempt", "plot_id"))
names(signif)[5] <- "signif"
signif <- mutate(signif, prop.sig=signif/n)
# signif$prop.roun <- round(signif$prop.sig, digits=2)
signif$response <- "PTF"
ptf <- signif[,c(8,1:7)]

signif <- rbind(odba, ptf)
signif

st <- as.data.frame(signif %>% group_by(response, variable, pop, attempt) %>% summarise(sum(signif)))
n <- as.data.frame(signif %>% group_by(response, variable, pop, attempt) %>% summarise(sum(n)))

st <- cbind(st, n$`sum(n)`)
names(st)[5:6] <- c("signif", "days")
st <- mutate(st, prop.sig=signif/days)

s.dat <- signif
# s.dat$weather <- ifelse(s.dat$variable=="Minimum Temperature", 1, 0)
s.dat$attempt <- ifelse(s.dat$attempt=="Attempt", 1, 0)
s.dat$pop <- ifelse(s.dat$pop=="Greenland",1,0)

odba <- s.dat[s.dat$response=="ODBA",-c(6,7)]
odba <- spread(odba, key="variable", value="prop.sig")
odbaMT <- scale(odba$`Minimum Temperature`)
odbaPR <- scale(odba$`Precipitation Rate`)
odba$`Minimum Temperature` <- as.vector(odbaMT)
odba$`Precipitation Rate` <- as.vector(odbaPR)

ptf <- s.dat[s.dat$response=="PTF",-c(6,7)]
ptf <- spread(ptf, key="variable", value="prop.sig")
ptfMT <- scale(ptf$`Minimum Temperature`)
ptfPR <- scale(ptf$`Precipitation Rate`)
ptf$`Minimum Temperature` <- as.vector(ptfMT)
ptf$`Precipitation Rate` <- as.vector(ptfPR)

nind <- length(unique(s.dat$plot_id))

sink("R/ptf_weather_signif.txt")
cat("
    model {
    
    # Priors and Constraints
    beta0 ~ dnorm(0, 0.01)
    beta1 ~ dnorm(0, 0.01)
    beta2 ~ dnorm(0, 0.01)
    beta3 ~ dnorm(0, 0.01)
    beta4 ~ dnorm(0, 0.01)
    beta5 ~ dnorm(0, 0.01)
    sigma_id ~ dunif(0, 100)
    tau_id <- 1 / (sigma_id ^ 2)

    # for (k in 1:nind) {
    #   eps_id[k] ~ dnorm(0, tau_id)  
    # }

    # Likelihood (loop over all observations)
    for (i in 1:nind) {  
      ad[i] ~ dbern(p[i])
      logit(p[i]) <- beta0 + beta1*sPRATE[i] + beta2*sMTEMP[i] + beta3*pop[i] + beta4*sPRATE[i]*pop[i] + beta5*sMTEMP[i]*pop[i]
    }

}", fill=TRUE)
sink()

# Bundle data
jags.data <- list(nind=nind, sPRATE=ptf$`Precipitation Rate`, sMTEMP=ptf$`Minimum Temperature`, pop=ptf$pop, ad=ptf$attempt)

# Initial values
inits <- function() {list(beta0=rnorm(1),beta1=rnorm(1),beta2=rnorm(1),beta3=rnorm(1),beta4=rnorm(1),beta5=rnorm(1),sigma_id=1)}

# Parameters monitored
params <- c('beta0','beta1',"beta2","beta3",'beta4','beta5','sigma_id')

# MCMC settings
ni <- 10000
nb <- 6000
nt <- 1
nc <- 3

# Run model
out1 <- jags(jags.data, inits, params, "R/ptf_weather_signif.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, parallel=TRUE)

print(out1, digits=3)

whiskerplot(out1, c('beta0','beta1',"beta2","beta3",'beta4','beta5'))


sink("R/odba_weather_signif.txt")
cat("
    model {
    
    # Priors and Constraints
    beta0 ~ dnorm(0, 0.01)
    beta1 ~ dnorm(0, 0.01)
    beta2 ~ dnorm(0, 0.01)
    beta3 ~ dnorm(0, 0.01)
    beta4 ~ dnorm(0, 0.01)
    beta5 ~ dnorm(0, 0.01)
    sigma_id ~ dunif(0, 100)
    tau_id <- 1 / (sigma_id ^ 2)

    # for (k in 1:nind) {
    #   eps_id[k] ~ dnorm(0, tau_id)  
    # }

    # Likelihood (loop over all observations)
    for (i in 1:nind) {  
      ad[i] ~ dbern(p[i])
      logit(p[i]) <- beta0 + beta1*sPRATE[i] + beta2*sMTEMP[i] + beta3*pop[i] + beta4*sPRATE[i]*pop[i] + beta5*sMTEMP[i]*pop[i]
    }


}", fill=TRUE)
sink()

# Bundle data
jags.data <- list(nind=nind, sPRATE=odba$`Precipitation Rate`, sMTEMP=odba$`Minimum Temperature`, pop=odba$pop, ad=odba$attempt)

# Initial values
inits <- function() {list(beta0=rnorm(1),beta1=rnorm(1),beta2=rnorm(1),beta3=rnorm(1),beta4=rnorm(1),beta5=rnorm(1),sigma_id=1)}

# Parameters monitored
params <- c('beta0','beta1',"beta2","beta3",'beta4','beta5','sigma_id')

# MCMC settings
ni <- 10000
nb <- 6000
nt <- 1
nc <- 3

# Run model
out2 <- jags(jags.data, inits, params, "R/odba_weather_signif.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, parallel=TRUE)

print(out1, digits=2)
print(out2, digits=2)

whiskerplot(out2, c('beta0','beta1',"beta2","beta3",'beta4','beta5'))


par(mfrow=c(2,1), mar=c(4.1,2.1,2,1))
whiskerplot(out1, c('beta0','beta1',"beta2","beta3",'beta4','beta5'))
whiskerplot(out2, c('beta0','beta1',"beta2","beta3",'beta4','beta5'))

# save(file="output/ptf_sigdays.Rdata", list="out1")
# save(file="output/odba_sigdays.Rdata", list="out2")


load("output/ptf_sigdays.Rdata")
load("output/odba_sigdays.Rdata")

#### Violin plots
data_summary <- function(x) {
  m <- mean(x)
  mq <- quantile(x, probs=c(0.025, 0.975))
  ymin <- as.numeric(mq[1])
  ymax <- as.numeric(mq[2])
  return(c(y=m, ymin=ymin, ymax=ymax))
}

df.out1 <- data.frame(beta0=out1$sims.list$beta0, beta1=out1$sims.list$beta1, beta2=out1$sims.list$beta2,
                      beta3=out1$sims.list$beta3, beta4=out1$sims.list$beta4, beta5=out1$sims.list$beta5)
df.out1 <- gather(df.out1, key=parameter, value=value, 1:6)
df.out1$parameter[df.out1$parameter=="beta0"] <- "Intercept"
df.out1$parameter[df.out1$parameter=="beta1"] <- "Effect of "
df.out1$parameter[df.out1$parameter=="beta2"] <- "Antecedent\nEffect"
df.out1$parameter[df.out1$parameter=="beta3"] <- "2017 Intercept"
df.out1$parameter[df.out1$parameter=="beta4"] <- "Antecedent\nEffect"
df.out1$parameter[df.out1$parameter=="beta5"] <- "Antecedent\nEffect"
df.out1$parameter <- factor(df.out1$parameter, levels=c("2018 Intercept", "2017 Intercept", "Antecedent\nEffect"))

ggplot(df.out1, aes(x=parameter, y=value)) + 
  geom_hline(yintercept=0, color="red") + 
  geom_violin(fill="gray80", size=1, color="gray50") + 
  coord_cartesian(ylim=c(-40,40)) + 
  theme_classic() + ylab("Posterior Distribution") + 
  stat_summary(fun.data=data_summary, size=0.75) +
  theme(axis.text.x=element_text(size=14), axis.text.y=element_blank(),
        axis.title=element_blank(),
        panel.border=element_rect(color="black", fill=NA, size=0.5))







#### Response curve for ODBA ####
nmcmc <- out2$mcmc.info$n.samples

# Set up samples
beta0 <- c(out2$samples[[1]][,1], out2$samples[[2]][,1], out2$samples[[3]][,1])
beta1 <- c(out2$samples[[1]][,2], out2$samples[[2]][,2], out2$samples[[3]][,2])
beta2 <- c(out2$samples[[1]][,3], out2$samples[[2]][,3], out2$samples[[3]][,3])
beta3 <- c(out2$samples[[1]][,4], out2$samples[[2]][,4], out2$samples[[3]][,4])
beta4 <- c(out2$samples[[1]][,5], out2$samples[[2]][,5], out2$samples[[3]][,5])
beta5 <- c(out2$samples[[1]][,6], out2$samples[[2]][,6], out2$samples[[3]][,6])

# beta4 = sPRATE * pop interaction
pred_length <- 100
prate_pred <- seq(-0.659,2.590,length.out=pred_length)

prate.grld <- matrix(, nmcmc, pred_length)
prate.namc <- matrix(, nmcmc, pred_length)
for (j in 1:pred_length) {
  prate.grld[,j] <- inv.logit(beta0 + beta1*0 + beta2*0 + beta3*1 + beta4*prate_pred[j] + beta5*0)
  prate.namc[,j] <- inv.logit(beta0 + beta1*0 + beta2*0 + beta3*0 + beta4*prate_pred[j] + beta5*0)
}
# prate.grld <- prate.grld/(prate.grld + 1)
prate.grld.qt <- apply(prate.grld, 2, quantile, probs=c(.5, .025, .975))

# prate.namc <- prate.namc/(prate.namc + 1)
prate.namc.qt <- apply(prate.namc, 2, quantile, probs=c(.5, .025, .975))

# unscale data
prate <- unscale(prate_pred, odbaPR)

# Set up data frame
df1 <- data.frame(pop="Greenland", y=prate.grld[1,], x.s=prate_pred, x=prate, up1=prate.grld.qt[2,], lo1=prate.grld.qt[3,])
df2 <- data.frame(pop="Midcontinent", y=prate.namc[1,], x.s=prate_pred, x=prate, up1=prate.namc.qt[2,], lo1=prate.namc.qt[3,])

df <- rbind(df1, df2)

ggplot(df) + geom_ribbon(aes(x=x, ymin=lo1, ymax=up1, fill=pop), alpha=0.4) + 
  geom_line(aes(x=x, y=y, color=pop), size=1.3) +
  scale_color_manual(values=c("#2166ac", "#b2182b")) +
  scale_fill_manual(values=c("#2166ac", "#b2182b")) +
  theme_classic() + ylab("Predicted Probability of Breeding Attempt") + xlab("Proportion of Days with Significant Effect") +
  theme(legend.justification=c(1,1.05),legend.title=element_text(size=16, face="bold"), 
        legend.position=c(1,1), legend.text=element_text(size=14), 
        legend.background=element_rect(fill=NA),
        axis.text=element_text(size=14), axis.title=element_text(size=16, face="bold"),
        panel.border=element_rect(color="black", fill=NA, size=0.5)) +
  guides(fill=guide_legend(title="Population"), 
       color=guide_legend(title="Population")) 




ggplot(df1, aes(x=x, y=y)) + 
  geom_ribbon(aes(x=x, ymin=lo1, ymax=up1), alpha=0.4, fill="red") + 
  geom_ribbon(data=df2, aes(x=x, ymin=lo1, ymax=up1), alpha=0.4, fill="blue") + 
  geom_line(color="red", size=1) + 
  geom_line(data=df2, aes(x=x, y=y), color="blue", size=1) +
  # ggtitle("Precipitation Rate") + 


## PTF
nmcmc <- out1$mcmc.info$n.samples

# Set up samples
beta0 <- c(out1$samples[[1]][,1], out1$samples[[2]][,1], out1$samples[[3]][,1])
beta1 <- c(out1$samples[[1]][,2], out1$samples[[2]][,2], out1$samples[[3]][,2])
beta2 <- c(out1$samples[[1]][,3], out1$samples[[2]][,3], out1$samples[[3]][,3])
beta3 <- c(out1$samples[[1]][,4], out1$samples[[2]][,4], out1$samples[[3]][,4])
beta4 <- c(out1$samples[[1]][,5], out1$samples[[2]][,5], out1$samples[[3]][,5])
beta5 <- c(out1$samples[[1]][,6], out1$samples[[2]][,6], out1$samples[[3]][,6])

# beta4 = sPRATE * pop interaction
pred_length <- 100
prate_pred <- seq(-1.16,2.53,length.out=pred_length)

prate.grld <- matrix(, nmcmc, pred_length)
prate.namc <- matrix(, nmcmc, pred_length)
for (j in 1:pred_length) {
  prate.grld[,j] <- inv.logit(beta0 + beta1*0 + beta2*0 + beta3*1 + beta4*prate_pred[j] + beta5*0)
  prate.namc[,j] <- inv.logit(beta0 + beta1*0 + beta2*0 + beta3*0 + beta4*prate_pred[j] + beta5*0)
}
# prate.grld <- prate.grld/(prate.grld + 1)
prate.grld.qt <- apply(prate.grld, 2, quantile, probs=c(.5, .025, .975))

# prate.namc <- prate.namc/(prate.namc + 1)
prate.namc.qt <- apply(prate.namc, 2, quantile, probs=c(.5, .025, .975))

# unscale data
prate <- unscale(prate_pred, ptfPR)

# Set up data frame
df1 <- data.frame(y=prate.grld[1,], x.s=prate_pred, x=prate, up1=prate.grld.qt[2,], lo1=prate.grld.qt[3,])
df2 <- data.frame(y=prate.namc[1,], x.x=prate_pred, x=prate, up1=prate.namc.qt[2,], lo1=prate.namc.qt[3,])

ggplot(df1, aes(x=x, y=y)) + geom_ribbon(aes(x=x, ymin=lo1, ymax=up1), alpha=0.4, fill="red") + 
  geom_ribbon(data=df2, aes(x=x, ymin=lo1, ymax=up1), alpha=0.4, fill="blue") + 
  geom_line(color="red", size=1) + geom_line(data=df2, aes(x=x, y=y), color="blue", size=1) +
  ggtitle("Precipitation Rate") + 
  theme_classic() + ylab("Predicted Probability of Breeding Attempt") + xlab("Proportion of Days with Significant Effect") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16, face="bold"),
        panel.border=element_rect(color="black", fill=NA, size=0.5))

# beta5 = sMTEMP * pop interaction
pred_length <- 100
temp_pred <- seq(-1.45,2.24, length.out=pred_length)

temp.grld <- matrix(, nmcmc, pred_length)
temp.namc <- matrix(, nmcmc, pred_length)
for (j in 1:pred_length) {
  temp.grld[,j] <- inv.logit(beta0 + beta1*0 + beta2*0 + beta3*1 + beta4*0 + beta5*temp_pred[j])
  temp.namc[,j] <- inv.logit(beta0 + beta1*0 + beta2*0 + beta3*0 + beta4*0 + beta5*temp_pred[j])
}
 
temp.grld.qt <- apply(temp.grld, 2, quantile, probs=c(.5, .025, .975))

temp.namc.qt <- apply(temp.namc, 2, quantile, probs=c(.5, .025, .975))

# unscale data
temp <- unscale(temp_pred, ptfMT)

# Set up data frame
df1 <- data.frame(y=temp.grld[1,], x.s=temp_pred, x=temp, up1=temp.grld.qt[2,], lo1=temp.grld.qt[3,])
df2 <- data.frame(y=temp.namc[1,], x.x=temp_pred, x=temp, up1=temp.namc.qt[2,], lo1=temp.namc.qt[3,])

ggplot(df1, aes(x=x, y=y)) + geom_ribbon(aes(x=x, ymin=lo1, ymax=up1), alpha=0.4, fill="red") + 
  geom_ribbon(data=df2, aes(x=x, ymin=lo1, ymax=up1), alpha=0.4, fill="blue") + 
  geom_line(color="red", size=1) + geom_line(data=df2, aes(x=x, y=y), color="blue", size=1) +
  ggtitle("Precipitation Rate") + 
  theme_classic() + ylab("Predicted Probability of Breeding Attempt") + xlab("Proportion of Days with Significant Effect") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16, face="bold"),
        panel.border=element_rect(color="black", fill=NA, size=0.5))

















#### Interaction plots from raw data ####
odba$pop <- ifelse(odba$pop==1, "Greenland", "Midcontinent")
odba$attempt <- ifelse(odba$attempt==1, "Attempt", "Defer")
sig <- as.data.frame(odba %>% group_by(pop, variable, attempt) %>% summarise(signif=sum(signif)))
days <- as.data.frame(odba %>% group_by(pop, variable, attempt) %>% summarise(mig.days=sum(n)))
sig$mig.days <- days$mig.days
sig <- mutate(sig, prop=signif/mig.days)

ptf$pop <- ifelse(ptf$pop==1, "Greenland", "Midcontinent")
ptf$attempt <- ifelse(ptf$attempt==1, "Attempt", "Defer")
sig.a <- as.data.frame(ptf %>% group_by(pop, variable, attempt) %>% summarise(signif=sum(signif)))
days.a <- as.data.frame(ptf %>% group_by(pop, variable, attempt) %>% summarise(mig.days=sum(n)))
sig.a$mig.days <- days.a$mig.days
sig.a <- mutate(sig.a, prop=signif/mig.days)

range(sig.a$prop)

# Panel A
ptf.a <- sig.a %>% group_by(pop, variable) %>% summarize(signif=sum(signif))
ptf.da <- sig.a %>% group_by(pop, variable) %>% summarize(mig.days=sum(mig.days))
ptf.a$mig.days <- ptf.da$mig.days
ptf.a <- mutate(ptf.a, prop=signif/mig.days)

# Panel B
ptf.b <- sig.a %>% group_by(pop, attempt) %>% summarize(signif=sum(signif))
ptf.db <- sig.a %>% group_by(pop, attempt) %>% summarize(mig.days=sum(mig.days))
ptf.b$mig.days <- ptf.db$mig.days
ptf.b <- mutate(ptf.b, prop=signif/mig.days)

# Panel C
ptf.c <- sig.a %>% group_by(variable, attempt) %>% summarize(signif=sum(signif))
ptf.dc <- sig.a %>% group_by(variable, attempt) %>% summarize(mig.days=sum(mig.days))
ptf.c$mig.days <- ptf.dc$mig.days
ptf.c <- mutate(ptf.c, prop=signif/mig.days)


plota <- ggplot(ptf.a, aes(x=variable, y=prop, color=pop, group=pop)) + 
  coord_cartesian(ylim=c(0.10, 0.34)) +
  geom_path() + 
  geom_point(size=3) +
  theme_classic() +
  xlab("Weather Variable") +
  scale_color_manual(values=c("#2166ac", "#b2182b")) + 
  ggtitle("(a)") +
  theme(panel.border=element_rect(color="black", fill=NA, size=0.5),
        legend.justification=c(0,0), legend.position=c(0,0), legend.text=element_text(size=14), 
        legend.title=element_text(size=14, face="bold"), 
        axis.text=element_text(size=12), axis.title.x=element_text(size=14),
        axis.title.y=element_blank(),
        title=element_text(size=14, face="bold"),
        legend.background=element_rect(fill=NA, colour=NA)) +
  guides(color=guide_legend(title="Population"))

plotb <- ggplot(ptf.b, aes(x=pop, y=prop, color=attempt, group=attempt)) + 
  coord_cartesian(ylim=c(0.10, 0.34)) +
  geom_path() + 
  geom_point(size=3) +
  theme_classic() +
  xlab("Population") +
  scale_color_manual(values=c("#2166ac", "#b2182b")) + 
  ggtitle("(b)") +
  theme(panel.border=element_rect(color="black", fill=NA, size=0.5),
        legend.justification=c(1,0), legend.position=c(1,0), legend.text=element_text(size=14), 
        legend.title=element_text(size=14, face="bold"), 
        axis.text.y=element_blank(), axis.title.y=element_blank(),
        axis.text.x=element_text(size=12),
        axis.title=element_text(size=14),
        title=element_text(size=14, face="bold"),
        legend.background=element_rect(fill=NA, colour=NA)) +
  guides(color=guide_legend(title="Breeding\nStatus"))

plotc <- ggplot(ptf.c, aes(x=variable, y=prop, color=attempt, group=attempt)) + 
  coord_cartesian(ylim=c(0.10, 0.34)) +
  geom_path() + 
  geom_point(size=3) +
  theme_classic() +
  xlab("Weather Variable") +
  scale_color_manual(values=c("#2166ac", "#b2182b")) + 
  ggtitle("(c)") +
  theme(panel.border=element_rect(color="black", fill=NA, size=0.5),
        legend.justification=c(0,0), legend.position=c(0,0), legend.text=element_text(size=14), 
        legend.title=element_text(size=14, face="bold"), 
        axis.text.y=element_blank(), axis.title.y=element_blank(),
        title=element_text(size=14, face="bold"),
        axis.text.x=element_text(size=12),
        axis.title.x=element_text(size=14),
        legend.background=element_rect(fill=NA, colour=NA)) +
  guides(color=guide_legend(title="Breeding\nStatus"))

plotd <- ggplot(sig, aes(x=variable, y=prop, color=attempt, group=attempt)) + 
  geom_path() + 
  geom_point(size=3) +
  facet_grid(~pop) + theme_classic() +
  scale_color_manual(values=c("#2166ac", "#b2182b")) + 
  ggtitle("(d)") +
  xlab("Weather Variable") + 
  theme(panel.border=element_rect(color="black", fill=NA, size=0.5),
        legend.justification=c(0,1), legend.position=c(0,1), legend.text=element_text(size=14), 
        legend.title=element_text(size=14, face="bold"), 
        strip.text=element_text(size=14, face="bold"),
        axis.text=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_blank(),
        title=element_text(size=14, face="bold"),
        legend.background=element_rect(fill=NA, colour=NA)) +
  guides(color=guide_legend(title="Breeding\nStatus"))


lay <- rbind(c(1,1,2,2,3,3),
             c(1,1,2,2,3,3),
             c(1,1,2,2,3,3),
             c(4,4,4,4,4,4),
             c(4,4,4,4,4,4),
             c(4,4,4,4,4,4))

grid.arrange(plota, plotb, plotc, plotd, layout_matrix=lay, 
             left=textGrob("Proportion of Days with Significant Effect", rot=90,vjust=1,
                           gp=gpar(fontface="bold", cex=1.5)))
              


