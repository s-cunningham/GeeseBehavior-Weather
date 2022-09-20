# Author: Stephanie Cunningham
# Checking data quality

library(tidyverse)

# Read in file that has tags NA data (JVB CTT & Orn)
tags <- read.csv("data/combined_NA_gps.csv", stringsAsFactors=FALSE)
tags <- tags[,-c(1)]

tags$date <- as.Date(tags$date, format="%m/%d/%Y")

days <- tags %>% group_by(key, date) %>% count()
days$julian <- format(days$date, "%j") |> as.numeric()

ggplot(days, aes(x=julian, y=factor(key), color=n)) + geom_point()

# Read in file that has all GR data (SC CTT & Orn + MW e-obs)
tags <- read.csv("data/combined_GR_gps.csv", stringsAsFactors=FALSE)
tags <- tags[,-c(1)]

tags$date <- as.Date(tags$date)

days <- tags %>% group_by(key, date) %>% count()
days$julian <- format(days$date, "%j") |> as.numeric()

ggplot(days, aes(x=julian, y=factor(key), color=n)) + geom_point()



## ACC


