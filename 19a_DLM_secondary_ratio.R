library(tidyverse)
library(jagsUI)
library(boot)
library(DMwR)

dat <- read.csv("output/dlm_results_ratio.csv")
dat <- dat[,-1]

# pivot longer
dat <- pivot_longer(dat, cols=14:15, names_to="variable", values_to="ptail")
dat <- as.data.frame(dat)

dat$variable <- ifelse(dat$variable=="prate.ptail", "prate", "mtemp")

dat$signif <- ifelse((dat$variable=="prate" & dat$ptail<=0.1) | 
                       (dat$variable=="mtemp" & dat$ptail>=0.9), 1, 0)

signif <- as.data.frame(dat %>% group_by(variable, pop, attempt, animal_id) %>% summarise(sum(signif)))
all <- as.data.frame(dat %>% group_by(variable, pop, attempt, animal_id) %>% count())
signif <- left_join(all, signif, by=c("variable", "pop", "attempt", "animal_id"))
names(signif)[6] <- "signif"

signif <- mutate(signif, prop.sig=signif/n)
signif <- signif[,c(2:4,1,7)]

signif <- pivot_wider(signif, names_from="variable", values_from="prop.sig")

signif$response <- "ODBAgraze"
signif <- as.data.frame(signif)

mt <- scale(signif$mtemp)
pr <- scale(signif$prate)

signif[,c(4,5)] <- scale(signif[,c(4,5)])

nind=length(unique(dat$animal_id))

signif$pop <- ifelse(signif$pop=="GRLD", 0, 1)


sink("R/ODBAgraze_weather_signif.txt")
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

    # Likelihood (loop over all observations)
    for (i in 1:nind) {  
      ad[i] ~ dbern(p[i])
      logit(p[i]) <- beta0 + beta1*sPRATE[i] + beta2*sMTEMP[i] + beta3*pop[i] + beta4*sPRATE[i]*pop[i] + beta5*sMTEMP[i]*pop[i]
    }

}", fill=TRUE)
sink()

# Bundle data
jags.data <- list(nind=nind, sPRATE=signif$prate, sMTEMP=signif$mtemp, pop=signif$pop, ad=signif$attempt)

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
out1 <- jags(jags.data, inits, params, "R/ODBAgraze_weather_signif.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, parallel=TRUE)

print(out1, digits=3)

whiskerplot(out1, c('beta0','beta1',"beta2","beta3",'beta4','beta5'))

#### Response curve ####
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
prate_pred <- seq(-0.4856225,3.7201099,length.out=pred_length)

prate.grld <- matrix(, nmcmc, pred_length)
prate.namc <- matrix(, nmcmc, pred_length)
for (j in 1:pred_length) {
  prate.grld[,j] <- inv.logit(beta0 + beta1*0 + beta2*0 + beta3*0 + beta4*prate_pred[j] + beta5*0)
  prate.namc[,j] <- inv.logit(beta0 + beta1*0 + beta2*0 + beta3*1 + beta4*prate_pred[j] + beta5*0)
}
# prate.grld <- prate.grld/(prate.grld + 1)
prate.grld.qt <- apply(prate.grld, 2, quantile, probs=c(.5, .025, .975))

# prate.namc <- prate.namc/(prate.namc + 1)
prate.namc.qt <- apply(prate.namc, 2, quantile, probs=c(.5, .025, .975))

# unscale data
prate <- unscale(prate_pred, pr)

# Set up data frame
df1 <- data.frame(pop="Greenland", y=prate.grld[1,], x.s=prate_pred, x=prate, up1=prate.grld.qt[2,], lo1=prate.grld.qt[3,])
df2 <- data.frame(pop="Midcontinent", y=prate.namc[1,], x.s=prate_pred, x=prate, up1=prate.namc.qt[2,], lo1=prate.namc.qt[3,])

df <- rbind(df1, df2)

ggplot(df) + geom_ribbon(aes(x=x, ymin=lo1, ymax=up1, fill=pop), alpha=0.4) + 
  geom_line(aes(x=x, y=y, color=pop), size=1.3) +
  scale_color_manual(values=c("#2166ac", "#b2182b")) +
  scale_fill_manual(values=c("#2166ac", "#b2182b")) +
  coord_cartesian(xlim=c(0,0.12)) +
  theme_classic() + ylab("Predicted Probability of Breeding Attempt") + xlab("Proportion of Days with Significant Effect") +
  theme(legend.justification=c(1,0),legend.title=element_text(size=16, face="bold"), 
        legend.position=c(1,0), legend.text=element_text(size=14), 
        legend.background=element_rect(fill=NA),
        axis.text=element_text(size=14), axis.title=element_text(size=16, face="bold"),
        panel.border=element_rect(color="black", fill=NA, size=0.5)) +
  guides(fill=guide_legend(title="Population"), 
         color=guide_legend(title="Population")) 


# beta5 = sMTEMP * pop interaction
pred_length <- 100
mtemp_pred <- seq(-1.251048,2.666383,length.out=pred_length)

mtemp.grld <- matrix(, nmcmc, pred_length)
mtemp.namc <- matrix(, nmcmc, pred_length)
for (j in 1:pred_length) {
  mtemp.grld[,j] <- inv.logit(beta0 + beta1*0 + beta2*0 + beta3*0 + beta4*0 + beta5*mtemp_pred[j] )
  mtemp.namc[,j] <- inv.logit(beta0 + beta1*0 + beta2*0 + beta3*1 + beta4*0 + beta5*mtemp_pred[j] )
}

mtemp.grld.qt <- apply(mtemp.grld, 2, quantile, probs=c(.5, .025, .975))
mtemp.namc.qt <- apply(mtemp.namc, 2, quantile, probs=c(.5, .025, .975))

# unscale data
mtemp <- unscale(mtemp_pred, mt)

# Set up data frame
df1 <- data.frame(pop="Greenland", y=mtemp.grld[1,], x.s=mtemp_pred, x=mtemp, up1=mtemp.grld.qt[2,], lo1=mtemp.grld.qt[3,])
df2 <- data.frame(pop="Midcontinent", y=mtemp.namc[1,], x.s=mtemp_pred, x=mtemp, up1=mtemp.namc.qt[2,], lo1=mtemp.namc.qt[3,])

df <- rbind(df1, df2)

ggplot(df) + geom_ribbon(aes(x=x, ymin=lo1, ymax=up1, fill=pop), alpha=0.4) + 
  geom_line(aes(x=x, y=y, color=pop), size=1.3) +
  coord_cartesian(xlim=c(0,0.5)) +
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



