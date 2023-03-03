# EAtlantic_map.R
# aim: Map of animal trajectories at-sea in Eastern Atlantic ocean

# Step 1. Set your working directory
# Step 2. Install and load libraries required.
# Step 3. Define your study area
# Step 4. Load bathymetry
# Step 5. Load land mask
# Step 6. Load EEZ 
# Step 7. Load tag deployment sites 
# Step 8. Load GPS files 
# Step 9. Plot it 
# Step 10. Save it

# ------------------- #
# Step 1. Set your WD #
# ------------------- #

#WD <- "C:/Users/lnh88/Dropbox/GitData/Data-visualization-tools/" #minipc
WD <- "C:/Users/lnh88/Dropbox/GitData/Data-visualization-tools/" #laptop

# -------------------- #
# Step 2. Requirements #
# -------------------- #

# install packages
#install.packages("marmap")
#install.packages("rnaturalearthdata")
#install.packages("sf")
#install.packages("ggplot2")
#install.packages("Cairo")
#install.packages("purrr")
#install.packages("readr")

# load libraries
library(marmap)
library(rnaturalearthdata)
library(sf)
library(ggplot2)
library(Cairo)
library(purrr)
library(readr)

# ------------------ #
# Step 3. Study area #
# ------------------ #

# set your extent
latmin <- 14
latmax <- 45
lonmin <- -26
lonmax <- 6
  
# ----------------------- #
# Step 4. Load bathymetry #
# ----------------------- #

bath <- marmap::getNOAA.bathy(
  lon1 = lonmin-1, lon2 = lonmax+1,
  lat1 = latmin-1, lat2 = latmax+1, 
  resolution = 4)

# ---------------------- #
# Step 5. Load land mask #
# ---------------------- #

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# ---------------- #
# Step 6. Load EEZ #
# ---------------- #

# from marineregions
# eezs <- sf::read_sf(paste0(WD,"input/EEZ_land_union_v3_202003/EEZ_Land_v3_202030.shp")) 

# --------------------------------- #
# Step 7. Load tag deployment sites #
# --------------------------------- #

sites <- read.csv2(paste0(WD,"input/sites.csv"))

# ---------------------- #
# Step 8. Load GPS files #
# ---------------------- #

files <- list.files(path = paste0(WD, "input/trips/"), pattern = "*.csv", recursive = TRUE)

GPS <- files %>%
  # read in all the files
  map_df(~ readr::read_csv(file.path(paste0(WD,"input/trips/"), .))) %>%
  # select coordinates and colonyname
  dplyr::select(longitude, latitude, tripID) 

# --------------- #
# Step 9. Plot it #
# --------------- #

p <- 
  # plot bathymetry
  ggplot(bath) +
  # fill background in white
  geom_raster(aes(x, y),fill="white")+ 
  # add GPS paths
  geom_path(data = GPS, aes(x=longitude, y=latitude, group=tripID, colour=tripID), lwd  = 0.1, alpha=0.8, show.legend = F) +
  # plot land mask
  geom_sf(data = world, 
          color = "gray30", fill = "gray90",lwd  = 0.05) +
  # plot EEZ
  #geom_sf(data = eezs,
  #        color = alpha("gray30",0.4),
  #        fill = NA,
  #        lwd  = 0.05,
  #        lty = 1)+     
  # add bathymetry -200 line 
  geom_contour(data = bath, aes(x, y, z = z),
               breaks=c(-200),
               colour="gray10", lwd  = 0.1 ,
               lty = 1) + 
  # add tag deployment sites
  geom_point(data=sites, aes(longitude,latitude), size=1.3, shape=17, colour="black", show.legend = F)+  
  geom_point(data=sites, aes(longitude,latitude), size=0.3, shape=17, colour="white", show.legend = F)+  
  # extent
  coord_sf(xlim = c(lonmin, lonmax), ylim = c(latmin, latmax))+
  # add scale
  ggspatial::annotation_scale(location = "br", 
                              line_col ="black",
                              text_col="black",
                              height = unit(0.1, "cm"),
                              line_width = 0.5,
                              text_cex= 0.5,
                              style = "ticks") +
  # define theme
  theme_bw()+
  theme(strip.text = element_text(size=3),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5),        
        plot.margin = unit(c(0, 0, 0, 0), "lines"),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.position="none"
  )+
  # define x and y labels
  xlab("Longitude") + ylab("Latitude")+
  # some more labels specific for that area
  geom_point(x=-17.05,y=20.99, size=0.8, shape=16, colour="black")+
  geom_point(x=-17.05,y=20.99, size=0.05, shape=16, colour="white")+
  geom_point(x=-7.62,y=33.60, size=0.8, shape=16, colour="black")+
  geom_point(x=-7.62,y=33.60, size=0.05, shape=16, colour="white")+
  geom_point(x=2.18,y=41.38, size=0.8, shape=16, colour="black")+
  geom_point(x=2.18,y=41.38, size=0.05, shape=16, colour="white")+
  geom_point(x=3.88,y=43.59, size=0.8, shape=16, colour="black")+
  geom_point(x=3.88,y=43.59, size=0.05, shape=16, colour="white")+
  geom_point(x=-17.46,y=14.72, size=0.8, shape=16, colour="black")+
  geom_point(x=-17.46,y=14.72, size=0.05, shape=16, colour="white")+
  geom_point(x=-14.499167,y=26.126944, size=0.8, shape=16, colour="black")+
  geom_point(x=-14.499167,y=26.126944, size=0.05, shape=16, colour="white")+
  annotate(
    geom = "text",
    x = -12.5+0.5,
    y = 26.2,
    label = "Cabo Bojador",
    fontface = "italic",
    color = "black",
    size = 1.4
  ) +
  annotate(
    geom = "text",
    x = 1,
    y = 42,
    label = "Barcelona",
    fontface = "italic",
    color = "black",
    size = 1.4
  ) +
  annotate(
    geom = "text",
    x = 4,
    y = 44.27,
    label = "Montpellier",
    fontface = "italic",
    color = "black",
    size = 1.4
  )+
  annotate(
    geom = "text",
    x = -5.8+0.4,
    y = 33.2+0.1,
    label = "Casablanca",
    fontface = "italic",
    color = "black",
    size = 1.4
  )+
  annotate(
    geom = "text",
    x = -16,
    y = 14.8,
    label = "Dakar",
    fontface = "italic",
    color = "black",
    size = 1.4
  )+
  annotate(
    geom = "text",
    x = -15+0.5,
    y = 21,
    label = "Cabo Blanco",
    fontface = "italic",
    color = "black",
    size = 1.4
  )+
  annotate(
    geom = "text",
    x = -5,
    y = 40,
    label = "ES",
    fontface = "italic",
    color = "black",
    size = 1.4
  )+
  annotate(
    geom = "text",
    x = -8,
    y = 32,
    label = "MA",
    fontface = "italic",
    color = "black",
    size = 1.4
  )+
  annotate(
    geom = "text",
    x = -14,
    y = 19,
    label = "MR",
    fontface = "italic",
    color = "black",
    size = 1.4
  )+
  annotate(
    geom = "text",
    x = -14,
    y = 15,
    label = "SN",
    fontface = "italic",
    color = "black",
    size = 1.4
  )+
  annotate(
    geom = "text",
    x = -14,
    y = 23,
    label = "WS",
    fontface = "italic",
    color = "black",
    size = 1.4
  )+
  annotate(
    geom = "text",
    x = 0,
    y = 44,
    label = "FR",
    fontface = "italic",
    color = "black",
    size = 1.4
  ) 
  

#x11();p

# ---------------- #
# Step 10. Save it #
# ---------------- #

setwd(paste0(WD,"output/"))

Cairo::Cairo(file = "EAtlantic.png",
             type = "png",
             units = "mm",
             width = 80,
             height = 100,
             dpi = 1000,
             bg = "white")
p
dev.off()
