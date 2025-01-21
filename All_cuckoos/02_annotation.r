#script for visualizing Kasper's cuckoo data and preparing for annotation. data was sent by Kasper in Feb 2023
#Feb 3, 2023. Elham Nourani, PhD. Konstanz, DE
#update: April 2024

library(tidyverse)
library(mapview)
library(lubridate)
library(sf)
library(terra)
library(readxl)
library(move2)
library(ncdf4)
#library(reticulate)
library(parallel)
library(move2)
library(units)

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



## STEP 2: download wind data ------------------------------------------
#wind data was manually downloaded from CDS:

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

## STEP 3: annotate ------------------------------------------

#put the three files into one.
wind_files <- list.files("/media/enourani/Ellham's HDD/cuckoo_raw_wind", pattern = ".nc", full.names = T)
names(wind_files) <- c("2019-2022", "2015-2018", "2010-2014")
#file 1: 2019-2022
#file 2: 2015-2018
#file 3: 2010-2014

#split cuckoo data into these three time periods
cck_ls <- readRDS("/home/enourani/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Kasper_cuckoos/R_files/full_data_apr24.rds") %>% 
  mutate(year = year(timestamp),
         time_period = case_when(
           between(year, 2010, 2014) ~ "2010-2014",
           between(year, 2015,2018) ~ "2015-2018",
           between(year, 2019, 2022) ~ "2019-2022"),
         closest_hr = round(timestamp, units = "hours") %>% as.character()) %>% #calculate closest hour for annotation
  group_split(time_period)


#annotate!!!

output_path <- "/home/enourani/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Kasper_cuckoos/manual_annotation_jan25/"

#original annotation code in https://github.com/mahle68/HB_ontogeny/blob/main/MS2_laterality/L04a_env_annotation.r
(st_time <- Sys.time())
lapply(cck_ls, function(x){ 
  
  #open the corresponding wind file. open a separate raster for u and v
  u <- wind_files[names(wind_files) == unique(x$time_period)] %>% 
    rast("u10")
  
  v <- wind_files[names(wind_files) == unique(x$time_period)] %>% 
    rast("v10")
  
  #for some reason I can't extract time using the terra::time() function. Extract the time from the layer names
  times <- names(u) %>%
    str_split("time=") %>%
    map_chr(2)
  
  #reference time according to the nc file
  ref_date <- ymd_hms("1970-01-01 00:00:00")
  
  #convert the hours into date + hour
  timestamp <- ref_date + seconds(times)
  
  #rename the layers based on the converted timestamps. these are the same for both u and v. but keep the u and v in the names for clarity when extracting values
  #from the two layers that would otherwise have the same name (ie. the timestamp)
  names(u) <- paste0("u_10_", timestamp)
  names(v) <- paste0("v_10_", timestamp)
  
  
  ########### split the tracking data into unique hours #####
  
  x_ls <- split(x, x$closest_hr)
  
  # Define the number of cores to use
  num_cores <- detectCores() - 10 #run on 2 cores. due to ram needs
  
  wind_this_period <- mclapply(x_ls, function(y){ #for each hour
    
    #extract the unique hour
    unique_hr <- y$closest_hr[1]
    
    # Check whether there is any wind data for this hour
    if (any(str_detect(names(u), unique_hr))) {
      # Extract the corresponding rasters
      wind <- c(
        u[[str_which(names(u), unique_hr)]],
        v[[str_which(names(v), unique_hr)]]
      )
      #names(wind) <- c("u_900", "v_900")
      
      # Convert tracking data to SpatVector
      y_vec <- vect(y, geom = c("location.long", "location.lat"), crs = "EPSG:4326")
      
      # Extract values for each point and append directly to y
      extracted_wind <- extract(x = wind, y = y_vec, method = "bilinear", bind = FALSE, ID = FALSE)
      colnames(extracted_wind) <- c("u_10", "v_10")
      
      # Append extracted values to y
      y_df <- y %>%
        bind_cols(as.data.frame(extracted_wind))
      
    } else {
      # If there are no matching wind data for this hour, create y_df with NA values
      y_df <- y %>%
        mutate(u_10 = as.numeric(NA),
               v_10 = as.numeric(NA))
    }
    
    
    rm(wind, y)
    
    y_df
    
  }, mc.cores = num_cores) %>% 
    bind_rows()
  
  
  saveRDS(wind_this_period, file = paste0(output_path,"wind_annotated_", unique(x$time_period), ".rds")) #had some issues with 2022 so had to do it in two batches.
  
})

Sys.time() - st_time #1.03 days


## STEP 4: put all files together and calculate wind speed ------------------------------------------

output_path <- "/home/enourani/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Kasper_cuckoos/manual_annotation_jan25"


ann_df <- list.files(output_path, recursive = T, full.names = T) %>% 
  map(readRDS) %>% 
  map(bind_rows) %>% 
  bind_rows() %>% 
  select(1:29) %>% 
  mutate(wind_speed =  sqrt(u_10^2 + v_10^2),
         wind_direction = atan2(v_10, u_10)) %>% 
  as.data.frame()

# SANITY CHECK. plot 

#have a look
ann_sf <- ann_df %>% 
  st_as_sf(coords = c("location.long", "location.lat"), crs = wgs)

mapview(ann_sf, zcol = "wind_speed", cex = 5, alpha = 0)

saveRDS(ann_df, file = "/home/enourani/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Kasper_cuckoos/wind_annotated_df_Jan25.rds")



## STEP 5: calculate wind support, air speed ------------------------------------------

# Function to calculate airspeed
airspeed <- function(ground_speed, wind_u, wind_v) {
  # Calculate wind speed
  wind_speed <- sqrt(wind_u^2 + wind_v^2)
  
  # Calculate wind direction in radians
  wind_direction <- atan2(wind_v, wind_u)
  
  # Calculate the bird's airspeed using vector subtraction
  airspeed <- sqrt((ground_speed * cos(wind_direction) - wind_u)^2 + 
                     (ground_speed * sin(wind_direction) - wind_v)^2)
  
  return(airspeed)
}

#POINT OF CLARIFICATION: use tag.local.identifier or individual.local.identifier (the former has more unique values)
ann_df <- readRDS("/home/enourani/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Kasper_cuckoos/wind_annotated_df_Jan25.rds") %>% 
  mutate(tag_year_id = paste0(tag.local.identifier, "_", year(timestamp))) %>%  #create unique ID for each year for each individual
  #remove duplicated timestamps (there was only one. lol)
  group_by(tag_year_id, timestamp)%>%
  slice(1) %>% 
  ungroup() %>% 
  #arrange based on timestamp for speed and distance calculations later on
  arrange(tag_year_id, timestamp) 

#convert to a move object to calculate heading, distances, etc.
mv <- ann_df %>% 
  st_as_sf(coords = c("location.long", "location.lat"), crs = "EPSG:4326") %>% 
  mt_as_move2(time_column = "timestamp", track_id_column = "tag_year_id") %>% 
  mutate(heading_rad = mt_azimuth(.), #this is in radians
         heading_deg = set_units(heading_rad, "degrees"),
         heading_0_360 = ifelse(heading_deg < set_units(0, "degrees"), heading_deg + set_units(360, "degrees"), heading_deg),  
         step_length_m = mt_distance(.),
         ground_speed_ms = mt_speed(.) %>% set_units("m/s")) %>% 
  mutate(wind_support_ms = wind_support(u = u_10, v = v_10, heading = heading_0_360) %>% set_units("m/s"),
         cross_wind_ms = cross_wind(u = u_10, v = v_10, heading = heading_0_360) %>% set_units("m/s"),
         air_speed_ms = airspeed(as.numeric(ground_speed), u_10, v_10)) #make sure the units are both m/s


