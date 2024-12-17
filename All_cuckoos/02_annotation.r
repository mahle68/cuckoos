#script for visualizing Kasper's cuckoo data and preparing for annotation. data was sent by Kasper in Feb 2023
#Feb 3, 2023. Elham Nourani, PhD. Konstanz, DE
#update: April 2024

library(tidyverse)
library(mapview)
library(lubridate)
library(sf)
library(readxl)
library(move2)
library(ncdf4)
library(reticulate)


wgs <- st_crs("+proj=longlat +datum=WGS84 +no_defs")
source("/home/enourani/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Projects/functions.R")

## STEP 1: prep for annotation ------------------------------------------
#second round with cleaned up data
cck <- readRDS("/home/enourani/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Kasper_cuckoos/R_files/full_data_apr24.rds") %>% 
  mutate(row_id = row_number(),
         timestamp = format(as.POSIXct(timestamp, tz = "UTC"), "%Y-%m-%d %H:%M:%S")) %>% 
  as.data.frame()

#have a look
cck_sf <- cck %>% 
  st_as_sf(coords = c("location.long", "location.lat"), crs = wgs)

mapview(cck_sf, zcol = "tag.local.identifier")



## STEP 2: annotate ------------------------------------------
#wind data was manually downloaded from CDS. data request code is at the end of this script

years <- as.character(2010:2022)
months <- c(1:12) %>% str_pad(2,"left","0")
days <- c(1:31) %>% str_pad(2,"left","0")
hours <- c(0:23) %>% str_pad(2, "left", "0") %>% paste0(":00")

output_path <- "/media/enourani/Ellham's HDD/cuckoo_raw_wind/"

#set up the API personal access token following the tutorial: https://cds.climate.copernicus.eu/how-to-api
use_python("/home/enourani/anaconda3/bin/python")

cdsapi <- import_from_path("cdsapi")

server <- cdsapi$Client()

for (i in years){
  request <- r_to_py(list(
    product_type = "reanalysis",
    variable = c("10m_u_component_of_wind", "10m_v_component_of_wind"),
    year = i,
    month = months,
    day = days,
    time = hours,
    area = c(69, -20, -25, 160),  # North, West, South, East. 
    format = "netcdf",
    dataset_short_name = "reanalysis-era5-land"
  ))
  
  server$retrieve("reanalysis-era5-land",
                  request,
                  target = paste0(output_path, i, "_wind_u_v_10m.nc"))
  
}

#nc_file <- "/home/enourani/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Kasper_cuckoos/raw_wind_oct24/83e3b1b40b8bd9bc1702f07838f36bd0.nc"





####old stuff ---------------------------------------------------------------------------------------------------------------------------------------------
#save as csv to submit to movebank. deal with calculating heading after annotation.

data <- cck %>% 
  dplyr::select(c("row_id", "tag.local.identifier", "location.long", "location.lat", "timestamp")) %>% #individual.local.identifier has many NAs. use tag id instead
  mutate(timestamp = paste(as.character(timestamp),"000",sep = ".")) %>% 
  as.data.frame()

colnames(data)[c(3,4)] <- c("location-long", "location-lat")

write_csv(data, "/home/enourani/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Kasper_cuckoos/tracks_to_annotate_cmpl_Apr24.csv")

## STEP 2: open annotated tracks ------------------------------------------
setwd("/home/enourani/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Kasper_cuckoos")

ann <- read.csv("/home/enourani/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Kasper_cuckoos/annotations_apr24/tracks_to_annotate_cmpl_Apr24.csv-2107373778739144544.csv", 
                stringsAsFactors = F) %>% 
  mutate(timestamp = as.POSIXct(strptime(timestamp,format = "%Y-%m-%d %H:%M:%S"),tz = "UTC")) %>% 
  mutate(year = year(timestamp)) %>% 
  group_by(tag.local.identifier, timestamp) %>% #remove duplicated timestamps
  slice(1) %>% 
  group_by(tag.local.identifier, year) %>%  #group by track to remove those with fewer rows than 2
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(n > 2) %>% 
  dplyr::select(-n) %>% 
  arrange(tag.local.identifier, timestamp) %>% 
  as.data.frame()

#calculate heading. make sure there is one move object per track
mv <- move(x = ann$location.long, y = ann$location.lat, time = ann$timestamp, 
           proj = "+proj=longlat +datum=WGS84 +no_defs", animal = paste(ann$tag.local.identifier, year(ann$timestamp), sep = "_"), data = ann)
mv$heading <- unlist(lapply(angle(mv), c, NA)) #heading from -180 ~ +180
mv$distance <- unlist(lapply(distance(mv), c, NA)) #also calculate distance

#calculate wind variables
wind_df <- as.data.frame(mv) %>% 
  dplyr::select(-c("coords.x1","coords.x2", "timestamps", "sensor", "optional")) %>% 
  mutate(heading_0_360 = ifelse(heading < 0, heading + 360, heading)) %>% #calculate heading from 0-360
  mutate(wind_support = wind_support(u = ECMWF.ERA5.PL.U.Wind, v = ECMWF.ERA5.PL.V.Wind, heading = heading_0_360),
         cross_wind= cross_wind(u = ECMWF.ERA5.PL.U.Wind, v = ECMWF.ERA5.PL.V.Wind, heading = heading_0_360),
         wind_speed = sqrt(ECMWF.ERA5.PL.U.Wind^2 + ECMWF.ERA5.PL.V.Wind^2),
         abs_cross_wind = abs(cross_wind(u = ECMWF.ERA5.PL.U.Wind, v = ECMWF.ERA5.PL.V.Wind, heading = heading_0_360)))

saveRDS(wind_df, file = "/home/enourani/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Kasper_cuckoos/R_files/wind_annotations_jul23.rds")


## try reticulate later:
#https://github.com/mahle68/seabirds_storms/blob/dad476f5675d8ef6daf825d1183b2548d7e594c6/seabird_complete.R#L310


### manual annotations
#submitted three requests for different years
# import cdsapi
# 
# dataset = "reanalysis-era5-pressure-levels"
# request = {
#   "product_type": ["reanalysis"],
#   "variable": [
#     "u_component_of_wind",
#     "v_component_of_wind"
#   ],
#   "year": [
#     "2010", "2011", "2012",
#     "2013", "2014", "2015"
#   ],
#   "month": [
#     "01", "02", "03",
#     "04", "05", "06",
#     "07", "08", "09",
#     "10", "11", "12"
#   ],
#   "day": [
#     "01", "02", "03",
#     "04", "05", "06",
#     "07", "08", "09",
#     "10", "11", "12",
#     "13", "14", "15",
#     "16", "17", "18",
#     "19", "20", "21",
#     "22", "23", "24",
#     "25", "26", "27",
#     "28", "29", "30",
#     "31"
#   ],
#   "time": [
#     "00:00", "01:00", "02:00",
#     "03:00", "04:00", "05:00",
#     "06:00", "07:00", "08:00",
#     "09:00", "10:00", "11:00",
#     "12:00", "13:00", "14:00",
#     "15:00", "16:00", "17:00",
#     "18:00", "19:00", "20:00",
#     "21:00", "22:00", "23:00"
#   ],
#   "pressure_level": ["950"],
#   "data_format": "netcdf",
#   "download_format": "unarchived",
#   "area": [69, -20, -25, 161]
# }
# 
# client = cdsapi.Client()
# client.retrieve(dataset, request).download()

