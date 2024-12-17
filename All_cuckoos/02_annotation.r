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



# import cdsapi
# 
# dataset = "reanalysis-era5-single-levels"
# request = {
#   "product_type": ["reanalysis"],
#   "variable": [
#     "10m_u_component_of_wind",
#     "10m_v_component_of_wind"
#   ],
#   "year": [
#     "2015", "2016", "2017",
#     "2018"
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
#   "data_format": "netcdf",
#   "download_format": "unarchived",
#   "area": [69, -20, -25, 160]
# }
# 
# client = cdsapi.Client()
# client.retrieve(dataset, request).download()

