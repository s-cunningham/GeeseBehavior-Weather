
library(tidyverse)
library(rworldmap)
library(sp)
library(rgdal)
library(ggspatial)

# Import polygons 
world <- readOGR("data/shapefiles", "ne_50m_land") 
world <- spTransform(world, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
world <- spTransform(world, CRS("+proj=aeqd +lat_0=55 +lon_0=-60"))  # both
mp <- borders(world, colour="gray70", fill="gray80")

ppr <- readOGR("data/shapefiles", "gmannppr")
ppr <- spTransform(ppr, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
ppr <- spTransform(ppr, CRS("+proj=aeqd +lat_0=55 +lon_0=-60"))
ppr2 <- borders(ppr, colour="gray40", fill="gray50")

isl <- readOGR("data/shapefiles", "ISL_adm0")
isl <- spTransform(isl, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
isl <- spTransform(isl, CRS("+proj=aeqd +lat_0=55 +lon_0=-60"))
isl2 <- borders(isl, colour="gray40", fill="gray50")

grid <- readOGR("data/shapefiles", "ne_10m_graticules_10")
grid <- spTransform(grid, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
grid <- spTransform(grid, CRS("+proj=aeqd +lat_0=55 +lon_0=-60"))
# plot(grid)
grid_df <- fortify(grid)

grid_df$group <- as.character(grid_df$group)
grid_df <- grid_df[!(grid_df$group=="23.1" & grid_df$order<350),]

# Import data frame with lat/long and convert to spatial points data frame so can project
# dat1 <-read.csv("output/dlm_emm_data.csv", stringsAsFactors=FALSE)
# dat1 <- dat1[,-c(1,13:18,20,21,22,25,26)]

# remove birds with not enough data
# dat1 <- dat1[dat1$animal_id!="RP08F" & dat1$animal_id!="RP15F" & dat1$animal_id!="2164"
             # & dat1$animal_id!="2176" & dat1$animal_id!="2160" & dat1$animal_id!="2167",]
dat1 <- read.csv("output/interp-gps-data_full.csv")
dat1 <- dat1[,-1]
dat1 <- dat1[dat1$animal_id!="LM31F" & dat1$animal_id!="LM17M" & dat1$animal_id!="RP15F",]
# dat1 <- dat1[dat1$julian<=150,]

# xy <- dat1[,c(14,13)]
xy <- dat1[,c(6,5)]
spdf <- SpatialPointsDataFrame(coords=xy, data=dat1, proj4string=CRS("+proj=longlat +datum=WGS84"))
spdf <- spTransform(spdf, CRS("+proj=aeqd +lat_0=55 +lon_0=-60"))
dat <- data.frame(spdf)
# names(dat)[c(15,16)] <- c("x", "y")
names(dat)[c(12,13)] <- c("x", "y")
dat$pop <- ifelse(dat$pop=="GRLD", "Greenland", "Midcontinent")

apr30 <- dat[dat$julian==140,]
apr30 %>% group_by(pop) %>% summarize(mean(latitude))
# Plot map
# ggplot() + geom_path(data = grid_df, aes(x = long, y = lat, group = group), color="gray50") +
#   mp + ppr2 + isl2 + coord_cartesian(xlim=c(-5000000,4000000), ylim=c(-3700000,4500000)) +
#   geom_point(data=dat, aes(x=x, y=y, group=animal_id, color=pop), size=2) +
#   scale_color_manual(values=c("#2166ac", "#b2182b")) + 
#   theme_classic() + theme(axis.title.x=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(),
#                           axis.title.y=element_blank(), panel.border=element_rect(color="black", fill=NA, size=0.5),
#                           legend.justification=c(1,0), legend.position=c(1,0), legend.text=element_text(size=14), 
#                           legend.title=element_text(size=16, face="bold"), 
#                           legend.background=element_rect(fill="white", colour="black")) +
#   guides(fill=guide_legend(title="Population"), color=guide_legend(title="Population")) +
#   annotation_scale(location="bl", text_cex=1.4, style="bar") +
#   annotate("text", x=3050000, y=4800000, label="Projection: Azimuthal Equidistant")


# Plot latitude by julian day
inset <- ggplotGrob(ggplot(dat) + geom_line(aes(x=julian, y=latitude, color=pop, group=animal_id), size=1) + 
  theme_classic() +
  scale_color_manual(values=c("#2166ac", "#b2182b")) + 
  ylab("Latitude") + xlab("Date") +
    scale_x_continuous(breaks=c(30,60,90,120,150), labels=c("30-Jan","01-Mar","30-Mar",
                                                               "30-Apr","30-May")) +
  theme(panel.border=element_rect(color="black", fill=NA, size=0.5),
        axis.text=element_text(size=12), axis.title=element_text(size=14, face="bold"),
        # legend.justification=c(1,0), legend.position=c(1,0), legend.text=element_text(size=14), 
        # legend.title=element_text(size=16, face="bold"), 
        # legend.background=element_rect(fill="white", colour="black")
        legend.position="none")
)

rect <- data.frame(xmin=0-12000, xmax=4400000+10000, ymin=-4050000-10000,  ymax=-500000+10000)

ggplot() + geom_path(data = grid_df, aes(x = long, y = lat, group = group), color="gray50") +
  mp + ppr2 + isl2 + coord_cartesian(xlim=c(-5000000,4000000), ylim=c(-3700000,4500000)) +
  geom_point(data=dat, aes(x=x, y=y, group=animal_id, color=pop), size=2) +
  scale_color_manual(values=c("#2166ac", "#b2182b")) +
  theme_classic() + theme(axis.title.x=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(),
                          axis.title.y=element_blank(), panel.border=element_rect(color="black", fill=NA, size=0.5),
                          legend.justification=c(1,1), legend.position=c(1,1), legend.text=element_text(size=14),
                          legend.title=element_text(size=16, face="bold"),
                          legend.background=element_rect(fill="white", colour="black")) +
  guides(fill=guide_legend(title="Population"), color=guide_legend(title="Population")) +
  annotation_scale(location="bl", text_cex=1.4, style="bar") +
  annotate("label", x=-4000000, y=4800000, label="Projection: Azimuthal Equidistant", fill="white") +
  geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="black", fill="white") +
  annotation_custom(grob=inset, xmin=0, xmax=4300000, ymin=-4050000,  ymax=-500000)


##### A plot showing where interpolated points are

miss <- dat[dat$missing=="y",]

ggplot() + geom_path(data = grid_df, aes(x = long, y = lat, group = group), color="gray50") +
  mp + coord_cartesian(xlim=c(0,3750000), ylim=c(500000,2250000)) +
  geom_line(data=dat, aes(x=x, y=y, group=animal_id), color="#4393c3") +
  geom_point(data=dat, aes(x=x, y=y, group=animal_id, color=missing, shape=missing), size=3, alpha=0.5) +
  geom_point(data=miss, aes(x=x, y=y, group=animal_id), size=3, shape=24, fill="#b2182b", color="black") +
  scale_color_manual(values=c("#4393c3", "#b2182b")) +
  theme_classic() + theme(axis.title.x=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(),
                          axis.title.y=element_blank(), panel.border=element_rect(color="black", fill=NA, size=0.5),
                          legend.position="none") +
  annotation_scale(location="bl", text_cex=1.4, style="bar")








##### A plot I made for a presentation

# Read in data
# dat <-read.csv("output/dlm_emm_data.csv", stringsAsFactors=FALSE)
# dat <- dat[,-1]
# names(dat)[1] <- "animal_id"
# dat$tag[dat$animal_id=="2160"] <- "EOBS"
# dat <- dat[dat$animal_id!="LM31F",] # Censor this individual (LM31F), 43 days with > 0.5 bursts missing
# 
# dat$pop <- ifelse(dat$pop=="GRLD", "Greenland", "Midcontinent")
# 
# ggplot(dat, aes(x=julian, y=latitude, group=animal_id, color=pop)) + geom_line(size=1) +
#   theme_bw() + ylab("Latitude") + xlab("Day of Year") +
#   scale_color_manual(values=c("#b2182b", "#2166ac")) + 
#   guides(color=guide_legend(title="Population")) +
#   theme(legend.position=c(1,0), legend.justification=c(1,0),
#         legend.title=element_text(size=16, face="bold"),
#         legend.text=element_text(size=14),
#         axis.text=element_text(size=14), axis.title=element_text(size=16))
# 
# 
# 



