#attempt to interpolate the tracks
#Feb.3. 2023 Konstanz, DE.
#Elham Nourani, PhD.

library(tidyverse)
library(mapview)
library(lubridate)
library(sf)
library(move)
library(rnaturalearth)
library(viridis)
library(aniMotum)
#using the foiGras package
#install.packages("aniMotum", 
#                 repos = "https://ianjonsen.r-universe.dev")
#remotes::install_github("ianjonsen/foieGras", dependencies = c("Imports","LinkingTo","Suggests"))
#library(foieGras)

wgs <- crs("+proj=longlat +datum=WGS84 +no_defs")
source("/home/enourani/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Projects/functions.R")

#open cuckoo data
cck <- read.csv("/home/mahle68/ownCloud/Work/Collaborations/cuckoos/Olga_cuckoos/for_annotation.csv") %>% 
  mutate(timestamp,timestamp = as.POSIXct(strptime(timestamp,format = "%Y-%m-%d %H:%M:%S"),tz = "UTC"))

cck_sf <- cck %>% 
  st_as_sf(coords = c("location.long", "location.lat"), crs = wgs)

#foiGras input should have these columns: id, date,lc,lon,lat
input <- cck %>% 
  rename(id = individual.local.identifier,
         date = timestamp,
         lon = location.long,
         lat = location.lat) %>% 
  mutate(lc = "G") #assign G for GPS
         

fit <- fit_ssm(input, vmax = 15, model = "rw", time.step = 1, 
               control = ssm_control(verbose = 0))

grab_sf <- grab(fit, what = "predicted", as_sf = T)

mapview(grab_sf, zcol = "id")

#save the file

grab_n <- grab(fit, what = "predicted", as_sf = F)

saveRDS(grab_n, file = "/home/mahle68/ownCloud/Work/Collaborations/cuckoos/Olga_cuckoos/interpolated_tracks_hrly.rds")

#prep for annotation
grab_df <- grab_n %>% 
  rename(individual.local.identifier = id) %>% 
  mutate(timestamp = paste(as.character(date),"000",sep = ".")) %>% 
  as.data.frame()

colnames(grab_df)[c(3,4)] <- c("location-long","location-lat")

write_csv(grab_df, "/home/mahle68/ownCloud/Work/Collaborations/cuckoos/Olga_cuckoos/tracks_to_annotate_hrly.csv")


#open annotated file
ann <- read.csv("/home/enourani/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Olga_cuckoos/annotated/hrly/tracks_to_annotate_hrly.csv-7878335371820883444.csv") %>% 
mutate(timestamp = as.POSIXct(strptime(timestamp,format = "%Y-%m-%d %H:%M:%S"),tz = "UTC")) %>% 
  mutate(year = year(timestamp)) %>% 
  arrange(individual.local.identifier, timestamp) 


#calculate heading. make sure there is one move object per track
mv <- move(x = ann$location.long, y = ann$location.lat, time = ann$timestamp, 
           proj = "+proj=longlat +datum=WGS84 +no_defs", animal = paste(ann$individual.local.identifier, ann$year, sep = "_"), data = ann)
mv$heading <- unlist(lapply(angle(mv), c, NA)) #heading from -180 ~ +180

#calculate wind variables
wind_df <- as.data.frame(mv) %>% 
  dplyr::select(-c("coords.x1","coords.x2")) %>% 
  mutate(heading_0_360 = ifelse(heading < 0, heading + 360, heading)) %>% #calculate heading from 0-360
  mutate(wind_support = wind_support(u = ECMWF.ERA5.PL.U.Wind, v = ECMWF.ERA5.PL.V.Wind, heading = heading_0_360),
         cross_wind= cross_wind(u = ECMWF.ERA5.PL.U.Wind, v = ECMWF.ERA5.PL.V.Wind, heading = heading_0_360),
         wind_speed = sqrt(ECMWF.ERA5.PL.U.Wind^2 + ECMWF.ERA5.PL.V.Wind^2),
         abs_cross_wind = abs(cross_wind(u = ECMWF.ERA5.PL.U.Wind, v = ECMWF.ERA5.PL.V.Wind, heading = heading_0_360))) %>% 
  as.data.frame()

saveRDS(wind_df, file = "/home/enourani/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Olga_cuckoos/hrly_wind_annotations.rds")

write.csv(wind_df, "/home/enourani/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Olga_cuckoos/hrly_wind_annotations.csv",
          row.names = F)




#### exploration
wind_df <- readRDS("/home/enourani/ownCloud/Work/Collaborations/cuckoos/Olga_cuckoos/hrly_wind_annotations.rds")


wind_sf <- wind_df %>% 
  st_as_sf(coords = c("location.long", "location.lat"), crs = wgs)

mapview(wind_sf, zcol = "wind_support")


#plot
base <- ne_coastline(scale = 'medium', returnclass = 'sf')

#color palettes: mako is blue, magma is red

(ws <- ggplot(data = base) +
  geom_sf(col = "gray", fill = "gray") +
  coord_sf(xlim = c(88.5, 158), ylim = c(-17, 60), expand = FALSE) +
  geom_path(data = wind_df, aes(x = location.long, y = location.lat, col = wind_support), linewidth = 2.5, lineend = "round") +
  scale_colour_viridis(option = "magma", na.value = "white", name = "m/s", alpha = 0.5) +
  theme_linedraw() +
  scale_x_continuous(breaks = c(0,30)) +
  scale_y_continuous(breaks = c(10,30,50)) +
  theme(axis.text = element_text(size = 10, colour = 1),
        legend.text = element_text(size = 10, colour = 1), 
        legend.title = element_text(size = 10, colour = 1),
        legend.position = "right",
        legend.background = element_rect(colour = NULL, fill = "white"))+
  labs(x = NULL, y = NULL, title = "Wind support") +
  facet_wrap(.~individual.local.identifier, nrow = 2))

png("/home/enourani/ownCloud/Work/Collaborations/cuckoos/Olga_cuckoos/wind_support_maps.png", units = "in", width = 13, height = 8, res = 300)
print(ws)
dev.off()

