## Data prep for extracting weather data and running models
## 2022-08-17
## Author: S. Cunningham

library(tidyverse)


## CTT ##
## Read files
ctt.files <- list.files(path="output/classified/ctt/", pattern=".csv", all.files=TRUE, full.names=TRUE)
ctt <- lapply(ctt.files, FUN=read.csv, header=TRUE, stringsAsFactors=FALSE)

for (i in 1:length(ctt.files)) {
  
  # Subset to each bird
  temp <- ctt[[i]]
  temp <- temp[,-1]

  # Calculate summarized values of behavior and ODBA  
  mean.odba <- temp %>% group_by(date) %>% summarize(mean.odba=mean(odba))
  median.odba <- temp %>% group_by(date) %>% summarize(median.odba=median(odba))
  nbursts <- temp %>% group_by(date) %>% count()
  nbehavior <- temp %>% group_by(date, behavior) %>% count()
  nbehavior <- pivot_wider(nbehavior, names_from="behavior", values_from="n")
  nbursts <- left_join(nbehavior, nbursts, by="date")
  names(nbursts)[5] <- "nbursts"
  nbursts <- mutate(nbursts, ptf=graze/nbursts)
  
  nbursts <- left_join(nbursts, mean.odba, by="date")
  nbursts <- left_join(nbursts, median.odba, by="date")
  nbursts <- as.data.frame(nbursts)
  
  # Create a table for id, tag type, sex and population
  tag <- rep("CTT", nrow(nbursts))
  animal_id <- rep(temp$id[1], nrow(nbursts))
  id <- data.frame(animal_id=animal_id, tag=tag)
  id$sex <- ifelse(id$animal_id=="LM17M", "M", "F")
  id$pop <- ifelse(id$animal_id=="X9Z", "GRLD", "NAMC")
  
  # bind individual identification to daily summarized values
  nbursts <- bind_cols(id, nbursts)
  
  # Save back into the list
  ctt[[i]] <- nbursts
}

# Convert list to data frame
ctt <- do.call(rbind, ctt)
ctt$animal_id <- as.character(ctt$animal_id)

## Ornitela ##
# Read files
orn.files <- list.files(path="output/odba_map/Ornitela/", pattern=".csv", all.files=TRUE, full.names=TRUE)
orn <- lapply(orn.files, FUN=read.csv, header=TRUE, stringsAsFactors=FALSE)

for (i in 1:length(orn.files)) {
  
  # Subset to each bird
  temp <- orn[[i]]

  # Calculate summarized values of behavior and ODBA  
  mean.odba <- temp %>% group_by(date) %>% summarize(mean.odba=mean(odba.map))
  median.odba <- temp %>% group_by(date) %>% summarize(median.odba=median(odba.map))
  nbursts <- temp %>% group_by(date) %>% count()
  nbehavior <- temp %>% group_by(date, behavior) %>% count()
  nbehavior <- pivot_wider(nbehavior, names_from="behavior", values_from="n")
  nbursts <- left_join(nbehavior, nbursts, by="date")
  names(nbursts)[5] <- "nbursts"
  nbursts <- mutate(nbursts, ptf=graze/nbursts)
  
  nbursts <- left_join(nbursts, mean.odba, by="date")
  nbursts <- left_join(nbursts, median.odba, by="date")
  nbursts <- as.data.frame(nbursts)
  
  # Create a table for id, tag type, sex and population
  tag <- rep("ORN", nrow(nbursts))
  animal_id <- rep(temp$id[1], nrow(nbursts))
  id <- data.frame(animal_id=animal_id, tag=tag)
  id$sex <- "F"
  id$pop <- ifelse(id$animal_id==17701 |
                     id$animal_id==17709 |
                     id$animal_id==17719 |
                     id$animal_id==17744 |
                     id$animal_id==17828 |
                     id$animal_id==17829, "NAMC", "GRLD")
  
  # bind individual identification to daily summarized values
  nbursts <- bind_cols(id, nbursts)
  
  # Save back into the list
  orn[[i]] <- nbursts
}

# Convert list to data frame
orn <- do.call(rbind, orn)

# Rename Jay's birds
# orn$animal_id <- as.character(orn$animal_id)
# orn$animal_id[orn$animal_id=="17701"] <- "RP20F"
# orn$animal_id[orn$animal_id=="17709"] <- "RP19F"
# orn$animal_id[orn$animal_id=="17719"] <- "RP17F"
# orn$animal_id[orn$animal_id=="17744"] <- "LM39F"
# orn$animal_id[orn$animal_id=="17828"] <- "RP23F"
# orn$animal_id[orn$animal_id=="17829"] <- "RP22F"

## e-obs ##
## Read files
eobs.files <- list.files(path="output/odba_map/eobs/", pattern=".csv", all.files=TRUE, full.names=TRUE)
eobs <- lapply(eobs.files, FUN=read.csv, header=TRUE, stringsAsFactors=FALSE)

for (i in 1:length(eobs.files)) {
  
  # Subset to each bird
  temp <- eobs[[i]]
  
  # Calculate summarized values of behavior and ODBA  
  mean.odba <- temp %>% group_by(date) %>% summarize(mean.odba=mean(odba.map))
  median.odba <- temp %>% group_by(date) %>% summarize(median.odba=median(odba.map))
  nbursts <- temp %>% group_by(date) %>% count()
  nbehavior <- temp %>% group_by(date, behavior) %>% count()
  nbehavior <- pivot_wider(nbehavior, names_from="behavior", values_from="n")
  nbursts <- left_join(nbehavior, nbursts, by="date")
  if (ncol(nbehavior)>4) {
    nbursts <- nbursts[, -which(names(nbursts) %in% c("biting tag"))]
  }
  names(nbursts)[c(2,5)] <- c("graze", "nbursts")
  nbursts <- mutate(nbursts, ptf=graze/nbursts)
  
  nbursts <- left_join(nbursts, mean.odba, by="date")
  nbursts <- left_join(nbursts, median.odba, by="date")
  nbursts <- as.data.frame(nbursts)
  
  # Create a table for id, tag type, sex and population
  tag <- rep("EOBS", nrow(nbursts))
  animal_id <- rep(temp$id[1], nrow(nbursts))
  id <- data.frame(animal_id=animal_id, tag=tag)
  id$sex <- "M"
  id$pop <- "GRLD"
  
  # bind individual identification to daily summarized values
  nbursts <- bind_cols(id, nbursts)
  
  # Save back into the list
  eobs[[i]] <- nbursts
}

# Convert list to data frame
eobs <- do.call(rbind, eobs)
eobs$animal_id <- as.character(eobs$animal_id)

## Combine

