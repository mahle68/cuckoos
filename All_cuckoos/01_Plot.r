#script for visualizing Kasper's cuckoo data. input rds was created by Kami in 00_ImportData.r
#Jan 25, 2023. Elham Nourani, PhD. Konstanz, DE

library(tidyverse)
library(move)
library(mapview)
library(sf)
library(rworldmap)

setwd("/home/enourani/ownCloud/Work/Collaborations/cuckoos/cuckoos/All_cuckoos/")
ck <- readRDS("Data/Cookoos_as_move.rds") %>% 
  as.data.frame()

#plot

world <- st_read("/home/enourani/ownCloud/Work/GIS_files/continent_shapefile/World_Continents.shp") %>% 
  st_union()

png("all_cuckoos.png", res = 300, width = 11, height = 8, units = "in")
ggplot() +
  geom_sf(data = world, fill = "black", col = 1) +
  geom_point(data = ck, aes(x = coords.x1, y = coords.x2, color = individual.local.identifier), size = 1, show.legend = FALSE) + 
  theme_void()
dev.off()