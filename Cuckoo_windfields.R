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
library(parallel)

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
  

#--------------------open ncdf data -----
  
  setwd("/home/enourani/Documents/cuckoo_weather")
  
  file_list <- list.files(pattern = ".nc", full.names = TRUE)
  vname <- c("u10","v10", "t2m","sst", "swh", "sp")
  
  
  mycl <- makeCluster(detectCores() - 9, setup_strategy = "sequential") 
  clusterExport(mycl, c("file_list", "vname")) #define the variable that will be used within the function
  
  clusterEvalQ(mycl, {
    library(dplyr)
    library(ncdf4)
    library(lubridate)
  })
  
  (b <- Sys.time())
  
  parLapply(mycl, file_list,function(x){
    
    nc <- nc_open(x)
    
    #extract lon and lat
    lat <- ncvar_get(nc,'latitude')
    nlat <- dim(lat) 
    lon <- ncvar_get(nc,'longitude')
    nlon <- dim(lon) 
    
    #extract the time
    t <- ncvar_get(nc, "time")
    nt <- dim(t)
    
    #convert the hours into date + hour
    timestamp <- as_datetime(c(t*60*60), origin = "1900-01-01")
    
    #put everything in a large df
    row_names <- expand.grid(lon,lat,timestamp)
    
    var_df <- data.frame(cbind(
      row_names,
      matrix(as.vector(ncvar_get(nc,vname[1])), nrow = nlon * nlat * nt, ncol = 1), #array to vector to matrix
      matrix(as.vector(ncvar_get(nc,vname[2])), nrow = nlon * nlat * nt, ncol = 1),
      matrix(as.vector(ncvar_get(nc,vname[3])), nrow = nlon * nlat * nt, ncol = 1),
      matrix(as.vector(ncvar_get(nc,vname[4])), nrow = nlon * nlat * nt, ncol = 1), #array to vector to matrix
      matrix(as.vector(ncvar_get(nc,vname[5])), nrow = nlon * nlat * nt, ncol = 1),
      matrix(as.vector(ncvar_get(nc,vname[6])), nrow = nlon * nlat * nt, ncol = 1)))
    
    colnames(var_df) <- c("lon","lat","date_time",vname)   #set column names
    
    #remove points over land (NAs)
    
    df <- var_df %>%
      #na.omit() %>% #will keep points over land. points where significant wave height or sst is NA, i.e. land, will be deleted
      mutate(yday = yday(date_time),
             hour = hour(date_time),
             year = year(date_time),
             delta_t = sst - t2m) %>%
      data.frame()
    
    save(df,file = paste0("/home/enourani/ownCloud/Work/Collaborations/Olga_cuckoos/atm_data/", 
                          unlist(strsplit(unlist(strsplit(x, ".nc")), "./"))[2], ".RData"))
  })
  
  Sys.time() - b #1.8 seconds :p
  
  stopCluster(mycl) 
  
  
  #--------------------merge all three files together -----
  
  files <- list.files("/home/enourani/ownCloud/Work/Collaborations/Olga_cuckoos/atm_data/", full.names = TRUE)
  #load and merge
  atm_df <- sapply(files, function(x) mget(load(x)), simplify = TRUE) %>%
         reduce(rbind) %>% 
    mutate(wind_speed = sqrt(u10^2 + v10^2), #m/s 
           unique_hour = paste(yday, hour, sep = "_"))
  
  
  #--------------------plot each track -----
  world <- st_read("/home/enourani/ownCloud/Work/GIS_files/continent_shapefile/continent.shp") %>% 
    #st_crop(xmin = -62, xmax = 11, ymin = -51, ymax = -25) %>%
    st_union()
  
  lapply(c(sk %>% distinct(individual.local.identifier) %>% .$individual.local.identifier), function(x){
    
    #open the wind files that match the months of data in the track
    mnth <- times %>% filter(individual.local.identifier == x) %>% distinct(month) %>% .$month
    
    #atm <- files %>% filter(str_detect(times %>% filter(individual.local.identifier == x) %>% distinct(month) %>%  as.character(.$month)))
    
    
    track <- sk %>% 
      filter(individual.local.identifier == x) %>% 
      mutate(unique_hour = paste(yday(timestamp), hour(timestamp), sep = "_")) %>% 
      group_by(unique_hour) %>% 
      slice(1) %>% #hourly filter
      ungroup()
    
    #extract weather data for the duration of the track. don't get the exact matches, because the track has many empty time stamps
    wth <- atm_df %>% 
      filter(between(date_time, min(track$timestamp)-3600, max(track$timestamp))) #subtract one hour from the minimum, to make sure to get the timestamp for the first point
    
    #save the entire track data
    #save(track, file = paste0("/home/enourani/ownCloud/Work/Projects/seabirds_and_storms/paper prep/wind_fields/processed/", 
    #                         head(track$sci_name,1),"_", x, "_whole_track.RData"))
    
    #save wind data for entire track
    #save(wth, file = paste0("/home/enourani/ownCloud/Work/Projects/seabirds_and_storms/paper prep/wind_fields/processed/", 
    #                            head(track$sci_name,1),"_", x, "_whole_track_wind.RData"))
    
    region <- world %>% 
      st_crop(xmin = min(wth$lon), xmax = max(wth$lon), ymin = min(wth$lat), ymax = max(wth$lat))
    
    
    # dir.create(paste0("/home/enourani/ownCloud/Work/Projects/seabirds_and_storms/paper prep/wind_fields/animation/", 
    #                   head(track$sci_name,1),"_", x, "/"))
    
    path <- paste0("/home/enourani/ownCloud/Work/Collaborations/Olga_cuckoos/wind_fields/", x, "/")
    
    
    #for(i in unique(track$unique_hour)){
      
    for(i in unique(wth$unique_hour)){
      
      #conditional on whether there is tracking data or not
      plot <- ggplot() +
        geom_tile(data = wth %>% filter(unique_hour == i), aes(x = lon, y = lat, fill = wind_speed))+
        geom_segment(data = wth %>% filter(unique_hour == i), 
                     aes(x = lon, xend = lon+u10/10, y = lat, 
                         yend = lat+v10/10), arrow = arrow(length = unit(0.12, "cm")), size = 0.3)+
        geom_sf(data = region, fill = "grey85", col = 1)+
        geom_point(data = track %>%  filter(unique_hour == i), aes(x = location.long, y = location.lat), 
                   size = 1.2, colour = "red") +
        #geom_point(data = track %>%  filter(unique_hour == i), aes(x = location.long, y = location.lat, colour = as.factor(avoidance)), 
        #           size = 1) +
        #scale_color_manual(values = c("avoided" = "red", "not_avoided" = "forestgreen")) +
        coord_sf(xlim = range(wth$lon), ylim =  range(wth$lat))+
        scale_fill_gradientn(colours = oce::oceColorsPalette(120), limits = c(0,23), 
                             na.value = "white", name = "Speed\n (m/s)")+
        theme_bw()+
        theme(axis.text = element_text(size = 12, colour = 1),
              legend.text = element_text(size = 10, colour = 1), 
              legend.title = element_text(size = 12, colour = 1),
              legend.position = c(0.1,0.13),
              legend.background = element_rect(colour = 1, fill = "white")) +
        labs(x = NULL, y = NULL, title = wth %>%  filter(unique_hour == i) %>% .$date_time %>% .[1] %>% paste(x))
      
      ggsave(plot = plot, filename = paste0(path, x,"_",which(unique(wth$unique_hour) == i),".jpeg"), #use the index of i instead of i itself. will be easier to make the animation later on 
             height = 10, width = 6, dpi = 300)
      
    }
  })
  
  
  