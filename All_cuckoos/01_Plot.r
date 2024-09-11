#script for visualizing Kasper's cuckoo data. input rds was created by Kami in 00_ImportData.r
#Jan 25, 2023. Elham Nourani, PhD. Konstanz, DE

library(tidyverse)
library(move)
library(mapview)
library(sf)
library(rworldmap)

setwd("/home/mahle68/ownCloud/Work/Collaborations/cuckoos/cuckoos/All_cuckoos/")

ck <- readRDS("Data/Cookoos_as_move.rds") %>% 
  as.data.frame()

#try plotting with the new data (sent by Kasper in Jan 2023)
ck <- read.table("/home/mahle68/ownCloud/Work/Collaborations/cuckoos/Kasper_cuckoos/data/AllCuckoosTotal6a.txt", header = T,dec = ".")


#plot

world <- st_read("/home/mahle68/ownCloud/Work/GIS_files/continent_shapefile/World_Continents.shp") %>% 
  st_union()

png("all_cuckoos.png", res = 300, width = 11, height = 8, units = "in")
ggplot() +
  geom_sf(data = world, fill = "black", col = 1) +
  geom_point(data = ck, aes(x = long, y = lat, color = id), size = 1, show.legend = FALSE) + 
  theme_void()
dev.off()