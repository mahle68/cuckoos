#preparing Olga's Cuckoo data for analysis

library(tidyverse)
library(lubridate)
library(sf)
library(reticulate)
library(raster)
library(cowplot)
library(rcartocolor)
library(mapview)
library(rworldmap)
library(cowplot)
library(ncdf4)
library(RNCEP)
library(ggridges)
library(viridis)

source("/home/enourani/ownCloud/Work/Projects/functions.R")
wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs")
#open data
files <- list.files("/home/enourani/ownCloud/Work/Collaborations/Olga_cuckoos/", pattern = "ICARUS.*.csv", full.names = T) 

cck_sf <- lapply(files, read.csv) %>% 
  reduce(rbind) %>% 
  mutate(timestamp,timestamp = as.POSIXct(strptime(timestamp,format = "%Y-%m-%d %H:%M:%S"),tz = "UTC")) %>% 
  st_as_sf(coords = c("location.long", "location.lat"), crs = wgs)

mapview(cck_sf, zcol = "individual.local.identifier")

#prep for annotation
cck <- lapply(files, read.csv) %>% 
  reduce(rbind) %>% 
  mutate(timestamp = paste(as.character(timestamp),"000",sep = "."))
  
colnames(cck)[c(4,5)] <- c("location-long","location-lat")

write.csv(cck, "/home/enourani/ownCloud/Work/Collaborations/Olga_cuckoos/for_annotation.csv")
saveRDS(cck_sf,file = "/home/enourani/ownCloud/Work/Collaborations/Olga_cuckoos/cck_data.rds")

cck_cols <- cck %>% 
  dplyr::select("timestamp", "location.long", "location.lat", "individual.local.identifier", "tag.local.identifier") %>% 
  as.data.frame()

colnames(cck_cols)[c(2,3)] <- c("location-long","location-lat")

write.csv(cck_cols, "/home/enourani/ownCloud/Work/Collaborations/Olga_cuckoos/for_annotation_no_NAs.csv")

# movebank keeps failing

#plot wind fields and see (from wind_fileds.R in seabirds_storms)

#--------------------request data download -----

#only for month and dates corresponding with the trip
#only for sakhalin
sk <- read.csv("/home/enourani/ownCloud/Work/Collaborations/Olga_cuckoos//ICARUS Russia Cuckoo Sakhalin _3birds.csv") %>% 
  mutate(timestamp,timestamp = as.POSIXct(strptime(timestamp,format = "%Y-%m-%d %H:%M:%S"),tz = "UTC")) %>% 
  filter(location.lat <= 37) #only keep the start of sea-crossing onward

#extract unique months and days
#unique timestamps for each trip
times <- sk %>% 
  mutate(year = year(timestamp),
         month = month(timestamp),
         yday = yday(timestamp),
         day = day(timestamp)) %>% 
  group_by(individual.local.identifier, year, month, yday) %>% 
  summarize(day = head(day,1)) %>% 
  ungroup()

#make a list of unique months and days
d <- list()
for(i in unique(times$month)){
  min_day <- min(times[times$month == i, "day"])
  max_day <- max(times[times$month == i, "day"])
  days <- list(str_pad(min_day:max_day, 2,"left","0"))
  d <- c(d,days)
}

#determine the extent
ext <- sk %>% 
  st_as_sf(coords = c("location.long", "location.lat"), crs = wgs) %>% 
  st_bbox()
  
#import the python library ecmwfapi
path <- "/home/enourani/.local/lib/python2.7/site-packages/"
use_python(path)
cdsapi <- import_from_path("cdsapi", path = path)
cdsapi <- import_from_path("cdsapi", path = "/root/.local/share/r-miniconda/envs/r-reticulate/bin/python/")

server = cdsapi$Client()

output_path <- "/home/enourani/Documents/cuckoo_weather/"


#lapply(c(1:nrow(extents[-6,])), function(x){ #skip atlantic yellow-nosed albatross (already done in seabird_complete)
  
  #species <- as.character(extents[x, "sci_name"])
  #trip <- as.character(extents[x, "TripID"])
  #datetimes <- days_ls[names(days_ls) == trip]
  year <- "2021"
  #area <- as.numeric(extents[x,3:6])
  area <- as.numeric(ext[c(4,1,2,3)])
  
  for (i in names(d)){ #for each month
    month <- i
    days <- unlist(d[names(d) == i])
    
    request <- r_to_py(list(
      product_type = "reanalysis",
      variable = c("10m_u_component_of_wind", "10m_v_component_of_wind", "2m_temperature", "sea_surface_temperature",
                   "significant_height_of_combined_wind_waves_and_swell", "surface_pressure"), #sea surface temp helps later for cropping land
      year = year,
      month = month,
      day = days,
      time = str_c(seq(0,23,1),"00",sep = ":") %>% str_pad(5,"left","0"),
      area = area,  # North, West, South, East.
      grid = c(1, 1), #reduce the grid size
      format = "netcdf",
      dataset_short_name = "reanalysis-era5-single-levels"
    ))
    
    server$retrieve("reanalysis-era5-single-levels",
                    request,
                    target = paste0(output_path,"atm_data_", month, "_2021.nc")) 
    
  }
  

#--------------------open annotated data -----
  ann <- read.csv("/home/enourani/ownCloud/Work/Collaborations/Olga_cuckoos/annotated/for_annotation_no_NAs.csv-6245954160786671131.csv")  %>% 
    mutate(timestamp,timestamp = as.POSIXct(strptime(timestamp,format = "%Y-%m-%d %H:%M:%S"),tz = "UTC"),
           wind_speed = sqrt(ECMWF.ERA5.SL.Wind..10.m.above.Ground.U.Component.^2 + ECMWF.ERA5.SL.Wind..10.m.above.Ground.V.Component.^2),
           delta_t = ECMWF.ERA5.SL.Sea.Surface.Temperature - ECMWF.ERA5.SL.Temperature..2.m.above.Ground.) %>% 
    rename(sst = ECMWF.ERA5.SL.Sea.Surface.Temperature,
           t2m = ECMWF.ERA5.SL.Temperature..2.m.above.Ground.,
           blh = ECMWF.ERA5.SL.Boundary.Layer.Height,
           wind_u_10m = ECMWF.ERA5.SL.Wind..10.m.above.Ground.U.Component.,
           wind_v_10m = ECMWF.ERA5.SL.Wind..10.m.above.Ground.V.Component.) %>% 
    dplyr::select(-1)
  
write.csv(ann, file = "/home/enourani/ownCloud/Work/Collaborations/Olga_cuckoos/annotated_tracks.csv", row.names = F)
  
  
ann_sf <- st_as_sf(ann, coords = c("location.long", "location.lat"), crs = wgs)

#latest annotation with wind at pressure level 900. corresponds to height of 995 m asl

#open old annotations to extract temperature variables
old_ann <- read.csv("/home/enourani/ownCloud/Work/Collaborations/Olga_cuckoos/annotated_tracks.csv") %>% 
  mutate(timestamp,timestamp = as.POSIXct(strptime(timestamp,format = "%Y-%m-%d %H:%M:%S"),tz = "UTC")) %>% 
  dplyr::select(-c("wind_speed", "wind_u_10m", "wind_v_10m")) 


ann2 <- read.csv("/home/enourani/ownCloud/Work/Collaborations/Olga_cuckoos/annotated/900mbar/for_annotation_no_NAs.csv-5783841532802204946.csv") %>% 
  mutate(timestamp,timestamp = as.POSIXct(strptime(timestamp,format = "%Y-%m-%d %H:%M:%S"),tz = "UTC")) %>% 
  full_join(old_ann) %>% 
  rename(wind_u_900 = ECMWF.ERA5.PL.U.Wind,
         wind_v_900 = ECMWF.ERA5.PL.V.Wind) %>% 
  mutate(geopotential_height = ECMWF.ERA5.PL.Geopotential/9.80665,
         wind_speed = sqrt(wind_u_900^2 + wind_v_900^2),
         wind_direction = (270-atan2(wind_v_900,wind_u_900)*180/pi)%%360) %>%  #calculate geopotential height from geopotential.
  group_by(individual.local.identifier) %>% 
  #arrange(timestamp, .by_group = TRUE) %>% 
  #rowwise() %>% 
  mutate(heading = ifelse(row_number() == nrow(.), NA, NCEP.loxodrome.na(lat1 = location.lat, lat2 = lead(location.lat,1), lon1 = location.long, lon2 = lead(location.long,1)))) %>% 
  ungroup() %>% 
  mutate(wind_support= wind_support(u = wind_u_900, v = wind_v_900, heading = heading),
         cross_wind= cross_wind(u = wind_u_900, v = wind_v_900, heading = heading)) %>% 
  as.data.frame()

write.csv(ann2, file = "/home/enourani/ownCloud/Work/Collaborations/Olga_cuckoos/annotated_tracks_1km.csv", row.names = F)


#--------------------plot conditions encountered -----

data_sf <- read.csv("/home/enourani/ownCloud/Work/Collaborations/Olga_cuckoos/annotated_tracks_1km.csv") %>% 
  st_as_sf(coords = c("location.long", "location.lat"), crs = wgs)

#coastline shapefile
coastline <- st_read("/home/enourani/ownCloud/Work/GIS_files/continent_shapefile/World_Continents.shp") %>% 
  st_crop(st_bbox(data %>% st_as_sf(coords = c("location.long", "location.lat"), crs = wgs))) %>%
  mutate(sea = F)
  #st_union()


data_sw <- st_join(data_sf, coastline) %>% 
  mutate(sea = ifelse(is.na(sea), "sea", "land"))

mapview(data_sw, zcol = "sea")

#keep sea-crossing tracks. along with last point on land before departure and first point on land after arrival
sea_tracks <- data_sw %>% 
  group_by(individual.local.identifier) %>% 
  arrange(timestamp) %>% 
  filter(sea == "sea" | sea == "land" & lead(sea,1) == "sea" | sea == "land" & lag(sea,1) == "sea")

mapview(sea_tracks, zcol = "individual.local.identifier")

#summarize track info... and plot them

#convert to long form for plotting
long_df <- sea_tracks %>% 
  as("Spatial") %>% 
  as.data.frame() %>% 
  mutate(day_of_year = yday(timestamp)) %>% 
  group_by(individual.local.identifier) %>% 
  pivot_longer(cols = c("delta_t", "wind_speed", "wind_support", "cross_wind"),
               names_to = "variable_names",
               values_to = "variable_values")


#plot with 4 panels
ggplot(long_df, aes(x = day_of_year, y = variable_values, color = individual.local.identifier)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~ variable_names) +
  labs(title = "atmospheric support along the sea-crossing trajectories", y = "wind support", x = "day of year") +
  theme_minimal() +
  theme(legend.position = "bottom")

#plot with 4 panels with ridges
X11(width = 9, height = 8)

png("/home/enourani/ownCloud/Work/Collaborations/Olga_cuckoos/figures/along_tracks.png", width = 9, height = 8, units = "in", res = 300)
ggplot(long_df, aes(x = variable_values, y = individual.local.identifier, fill = stat(x))) +
  geom_density_ridges_gradient(jittered_points = TRUE, scale = 1.5, rel_min_height = .01,
                          point_shape = "|", point_size = 2, size = 0.25, alpha = 0.6,
                          position = position_points_jitter(height = 0)) + 
  scale_fill_viridis(option = "mako", guide = "none") +
  facet_wrap(~ variable_names, labeller = labeller(variable_names = c( "cross_wind" = "Corss wind (m/s)", 
                                                                    "delta_t" = "Delta T (°C)", 
                                                                    "wind_speed" = "Wind speed (m/s)",
                                                                    "wind_support" = "Wind support (m/s)"))) +
  labs(title = "Atmospheric conditions experienced along the sea-crossing tracks", y = "", x = "") +
  theme_bw() +
  theme(legend.position = NULL)

dev.off()
