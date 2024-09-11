#script for visualizing Kasper's cuckoo data and preparing for annotation. data was sent by Kasper in Feb 2023
#Feb 3, 2023. Elham Nourani, PhD. Konstanz, DE
#update: April 2024

library(tidyverse)
library(mapview)
library(lubridate)
library(sf)
library(readxl)
library(move2)

wgs <- st_crs("+proj=longlat +datum=WGS84 +no_defs")
source("/home/enourani/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Projects/functions.R")

## STEP 1: prep for annotation ------------------------------------------
#second round with cleaned up data
cck <- readRDS("/home/mahle68/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Kasper_cuckoos/R_files/full_data_apr24.rds") %>% 
  mutate(row_id = row_number(),
         timestamp = format(as.POSIXct(timestamp, tz = "UTC"), "%Y-%m-%d %H:%M:%S")) %>% 
  as.data.frame()

#have a look
cck_sf <- cck %>% 
  st_as_sf(coords = c("location.long", "location.lat"), crs = wgs)

mapview(cck_sf, zcol = "tag.local.identifier")

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
