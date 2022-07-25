# Whiskerplots for Ecological Memory results
# 5 August 2019

#### Greenland
load("output/ecomem_GRLD.Rdata")

df <- data.frame(antPTF=out2$mean$antODBA, status=dat2$attempt)
df$status <- ifelse(df$status==1, "Attempt", "Defer")

aggregate(antPTF ~ status, data=df, FUN=mean)




ggplot(df, aes(y=antPTF, x=status, group=status)) + geom_boxplot(fill="gray70") + theme_classic() +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16, face="bold"), 
        panel.border=element_rect(color="black", fill=NA, size=0.5))



# Violin plot
grld.df <- data.frame(beta1=out2$sims.list$beta1, beta2=out2$sims.list$beta2)
grld.df <- gather(grld.df, key=beta, value=value, 1:2)
grld.df$beta <- ifelse(grld.df$beta=="beta1","Antecedent PTF", "Year")

data_summary <- function(x) {
  m <- mean(x)
  mq <- quantile(x, probs=c(0.025, 0.975))
  ymin <- as.numeric(mq[1])
  ymax <- as.numeric(mq[2])
  return(c(y=m, ymin=ymin, ymax=ymax))
}

ggplot(grld.df, aes(x=beta, y=value)) + 
  geom_hline(yintercept=0, color="red") + 
  geom_violin(fill="gray80", size=1, color="gray50") + 
  theme_classic() + xlab("Covariate") + ylab("Posterior Distribution") + 
  stat_summary(fun.data=data_summary, size=1) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16, face="bold"), 
        panel.border=element_rect(color="black", fill=NA, size=0.5))

# Weights


ow <- data.frame(weights=out2$mean$weightOrdered, day=seq(1,45,1), min=out2$q2.5$weightOrdered, max=out2$q97.5$weightOrdered)

ggplot(ow, aes(x=day, y=weights)) + 
  ylim(0, 0.1) + 
  geom_segment(aes(x=day, y=min, xend=day, yend=max), size=1, color="gray30") + 
  geom_point(size=3) + 
  ylab("Weight") + xlab("Day of Migration") +
  geom_hline(yintercept=(1/45), color="red", size=1) + 
  theme_classic() +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16, face="bold"), 
        panel.border=element_rect(color="black", fill=NA, size=0.5))




#### North America

load("output/ecomem_NAMC.Rdata")

# Violin plot
namc.df <- data.frame(beta0=out2$sims.list$beta0, beta1=out2$sims.list$beta1)
namc.df <- gather(namc.df, key=beta, value=value, 1:2)
namc.df$beta <- ifelse(namc.df$beta=="beta1","Antecedent PTF", "Year")

data_summary <- function(x) {
  m <- mean(x)
  mq <- quantile(x, probs=c(0.025, 0.975))
  ymin <- as.numeric(mq[1])
  ymax <- as.numeric(mq[2])
  return(c(y=m, ymin=ymin, ymax=ymax))
}

ggplot(namc.df, aes(x=beta, y=value)) + 
  geom_hline(yintercept=0, color="red") + 
  geom_violin(fill="gray80", size=1, color="gray50") + 
  theme_classic() + xlab("Covariate") + ylab("Posterior Distribution") + 
  stat_summary(fun.data=data_summary, size=1) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16, face="bold"), 
        panel.border=element_rect(color="black", fill=NA, size=0.5))

# Weights

cw = data.frame(cweights=out2$mean$cum.weight, day=seq(1,89,1))

ggplot(cw, aes(x=day, y=cweights)) + geom_point() + 

plot(out2$mean$weightOrdered, type="p", ylim=c(0,0.06), pch=20, ylab="Daily Weights")
segments(x0=1:un.dur, x1=1:un.dur, y0=out2$q2.5$weightOrdered, y=out2$q97.5$weightOrdered)

ow <- data.frame(weights=out2$mean$weightOrdered, day=seq(1,89,1), min=out2$q2.5$weightOrdered, max=out2$q97.5$weightOrdered)

ggplot(ow, aes(x=day, y=weights)) + 
  ylim(0, 0.06) + 
  geom_segment(aes(x=day, y=min, xend=day, yend=max), size=1, color="gray30") + 
  geom_point(size=3) + 
  ylab("Weight") + xlab("Day of Migration") +
  geom_hline(yintercept=(1/89), color="red", size=1) + 
  theme_classic() +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16, face="bold"), 
        panel.border=element_rect(color="black", fill=NA, size=0.5))


