## Indicating where geese are in migration
## 2022-08-24
## Author: S. Cunningham

library(tidyverse)
library(sf)


## Read in staging areas shapefiles
# North America - Prairie Pothole Region
ppr <- st_read("data/shapefiles", "gmannppr")
ppr <- st_transform(ppr, "+proj=aeqd +lat_0=55 +lon_0=-60 +datum=WGS84")

# Greenland - Iceland
isl <- st_read("data/shapefiles", "ISL_adm0")
isl <- st_transform(isl, "+proj=aeqd +lat_0=55 +lon_0=-60 +datum=WGS84")

