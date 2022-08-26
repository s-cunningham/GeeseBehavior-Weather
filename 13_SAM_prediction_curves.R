##*
##*  Plotting response curves
##*  2022-08-25
##*   
##*

library(tidyverse)
# library(patchwork)
# library(boot)

## ODBA results
load("results/ODBA_sam.Rdata")

# Set up posterior samples
beta01 <- c()
beta02 <- c()
beta1 <- c()
beta2 <- c()
beta3 <- c()

# Predict
pred_length <- 100
wint_pred <- seq(,,length.out=pred_length)


