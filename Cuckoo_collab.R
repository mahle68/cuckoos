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
  


  
  