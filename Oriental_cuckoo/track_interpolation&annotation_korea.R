#attempt to interpolate the tracks
#Feb.3. 2023 Konstanz, DE.
#Elham Nourani, PhD.

library(tidyverse)
library(readxl)
library(mapview)
library(lubridate)
library(sf)
library(move2)
library(rnaturalearth)
library(viridis)
library(aniMotum)
library(units)
#using the foiGras package
# install.packages("aniMotum",
#                 repos = "https://ianjonsen.r-universe.dev")
# remotes::install_github("ianjonsen/foieGras", dependencies = c("Imports","LinkingTo","Suggests"))
# library(foieGras)

wgs <- st_crs("+proj=longlat +datum=WGS84 +no_defs")
source("/home/enourani/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Projects/functions.R")

#open data ################################
setwd("/home/enourani/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Olga_cuckoos/Korea_collab/Raw_data/")
#there is one sheet per individual. read all in and put in one dataframe

sheets <- readxl::excel_sheets("NIBR_Oriental Cuckoo PTT data.xls")[-1]
#some sheets have 11 and some 22 columns. find col names of interest that they all have
cols <- intersect(colnames(readxl::read_excel("NIBR_Oriental Cuckoo PTT data.xls", sheet = sheets[[1]])),
              colnames(readxl::read_excel("NIBR_Oriental Cuckoo PTT data.xls", sheet = sheets[[7]])))

kck <- lapply(sheets, function(x) {
  readxl::read_excel("NIBR_Oriental Cuckoo PTT data.xls", sheet = x) %>% 
    dplyr::select(Longitude, Latitude, 'Loc. date', 'Loc. quality') %>% 
    mutate(ID = x) %>% 
    rename(timestamp = 'Loc. date',
           loc_quality = 'Loc. quality') %>% 
    filter(loc_quality %in% c("1","2","3","0"))
  }) %>% 
  reduce(rbind)

#save a csv file for Pavel

write.csv(as.data.frame(kck), file = "Korea_cuckoos_all.csv")

#convert to mt object ################################
kck_sf <- kck %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = wgs) %>% 
  arrange(ID, timestamp)

mv <- mt_as_move2(kck_sf, time_column = "timestamp", track_id_column = "ID") 

w_lines <- mt_track_lines(mv)

#interpolate ################################
#foiGras input should have these columns: id, date,lc,lon,lat
input <- kck %>% 
  mutate(lon = as.numeric(kck$Longitude),
         lat = as.numeric(kck$Latitude)) %>% 
  rename(id = ID,
         date = timestamp) %>% 
  mutate(lc = loc_quality)


fit <- fit_ssm(input, vmax = 15, model = "rw", time.step = 1, 
               control = ssm_control(verbose = 0))

grab_sf <- grab(fit, what = "predicted", as_sf = T)

mapview(grab_sf, zcol = "id")

grab_n <- grab(fit, what = "predicted", as_sf = F)
saveRDS(grab_n, file = "/home/mahle68/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Olga_cuckoos/K_interp_tracks_hrly.rds")

#prep for annotation ################################
grab_df <- grab_n %>% 
  rename(individual.local.identifier = id) %>% 
  mutate(timestamp = paste(as.character(date),"000",sep = ".")) %>% 
  as.data.frame()

colnames(grab_df)[c(3,4)] <- c("location-long","location-lat")

write_csv(grab_df, "/home/mahle68/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Olga_cuckoos/K_interp_tracks_hrly.csv")

#wind at 950 pressure level


#prep for annotation ################################

#open annotated file
ann <- read.csv("/home/mahle68/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Olga_cuckoos/Korea_collab/annotated/K_interp_tracks_hrly.csv-8793165922889505217/K_interp_tracks_hrly.csv-8793165922889505217.csv") %>% 
  full_join(read.csv("/home/mahle68/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Olga_cuckoos/Korea_collab/annotated/K_interp_tracks_hrly.csv-8406592499361180532/K_interp_tracks_hrly.csv-8406592499361180532.csv")) %>% 
  mutate(timestamp = as.POSIXct(strptime(timestamp,format = "%Y-%m-%d %H:%M:%S"),tz = "UTC")) %>% 
  mutate(year = year(timestamp)) %>% 
  rename(u_wind_950 = ECMWF.ERA5.PL.U.Wind,
         v_wind_950 = ECMWF.ERA5.PL.V.Wind,
         u_wind_10m = ECMWF.ERA5.SL.Wind..10.m.above.Ground.U.Component.,
         v_wind_10m = ECMWF.ERA5.SL.Wind..10.m.above.Ground.V.Component.,
         sst = ECMWF.ERA5.SL.Sea.Surface.Temperature,
         t2m = ECMWF.ERA5.SL.Temperature..2.m.above.Ground.,
         geo_height_950 = ECMWF.ERA5.PL.Geopotential) %>% 
  arrange(individual.local.identifier, timestamp) 

#convert to move2 object
mv_w <- ann %>% 
  st_as_sf(coords = c("location.long", "location.lat"), crs = wgs) %>% 
  mt_as_move2(time_column = "timestamp", track_id_column = "individual.local.identifier") 

w_lines <- mt_track_lines(mv_w)


#calculate heading. make sure there is one move object per track
mv_w$heading <- mt_azimuth(mv_w) %>% set_units("degrees") %>% as.numeric()

#calculate wind variables
wind_df <- mv_w %>% 
  mutate(location_long = sf::st_coordinates(.)[,1],
         location_lat = sf::st_coordinates(.)[,2]) %>% 
  as.data.frame() %>% 
  mutate(heading_0_360 = ifelse(heading < 0, heading + 360, heading)) %>% #calculate heading from 0-360
  mutate(wind_support_950 = wind_support(u = u_wind_950, v = v_wind_950, heading = heading_0_360),
         cross_wind_950 = cross_wind(u = u_wind_950, v = v_wind_950, heading = heading_0_360),
         wind_speed_950 = sqrt(u_wind_950^2 + u_wind_950^2),
         abs_cross_wind_950 = abs(cross_wind(u = u_wind_950, v = v_wind_950, heading = heading_0_360)),
         wind_support_10m = wind_support(u = u_wind_10m, v = v_wind_10m, heading = heading_0_360),
         cross_wind_10m = cross_wind(u = u_wind_10m, v = v_wind_10m, heading = heading_0_360),
         wind_speed_10m = sqrt(u_wind_10m^2 + u_wind_10m^2),
         abs_cross_wind_10m = abs(cross_wind(u = u_wind_10m, v = v_wind_10m, heading = heading_0_360))) %>% 
  as.data.frame()

saveRDS(wind_df, file = "/home/mahle68/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Olga_cuckoos/Korea_collab/k_hrly_wind_annotations.rds")

#### exploration

wind_df <- readRDS("/home/mahle68/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Olga_cuckoos/Korea_collab/k_hrly_wind_annotations.rds")

wind_sf <- wind_df %>% 
  st_as_sf(coords = c("location_long", "location_lat"), crs = wgs)

mapview(wind_sf, zcol = "wind_support_950")
mapview(wind_sf, zcol = "wind_speed_950")


#plot
base <- ne_coastline(scale = 'medium', returnclass = 'sf')

#color palettes: mako is blue, magma is red

(ws <- ggplot(data = base) +
  geom_sf(col = "gray", fill = "gray") +
  coord_sf(xlim = c(88.5, 158), ylim = c(-17, 60), expand = FALSE) +
  geom_path(data = wind_df, aes(x = location_long, y = location_lat, col = wind_support_950), linewidth = 2.5, lineend = "round") +
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

png("/home/mahle68/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Olga_cuckoos/k_wind_support_maps.png", units = "in", width = 13, height = 8, res = 300)
print(ws)
dev.off()

