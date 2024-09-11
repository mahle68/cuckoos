# Plot the wind fields for all Oriental cuckoos
# Nov. 20 . 2023, Konstanz, DE.

library(tidyverse)
library(lubridate)
library(ncdf4)
library(ecmwfr)
library(sf)
library(terra)
library(mapview)
library(rnaturalearth)

wgs <-  crs("+proj=longlat +datum=WGS84 +no_defs")

# STEP 1: open all interpolated data -------------------------------------

kr_hrly <-  readRDS("/home/enourani/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Olga_cuckoos/Korea_collab/k_hrly_wind_annotations.rds") %>% 
  rename(location.long = location_long,
         location.lat = location_lat) %>% 
  dplyr::select("location.long", "location.lat", "timestamp", "individual.local.identifier") %>% 
  mutate(source = "Korea",
         individual.local.identifier = as.character(individual.local.identifier)) #the Korean data is not in lat-lon

all_ck <- readRDS("/home/enourani/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Olga_cuckoos/hrly_wind_annotations.rds") %>% 
  dplyr::select("location.long", "location.lat", "timestamp", "individual.local.identifier") %>%
  mutate(source = "Russia") %>% 
  `rownames<-`( NULL) %>% 
  full_join(kr_hrly)

all_ck_sf <- all_ck %>% 
  st_as_sf(coords = c("location.long", "location.lat"), crs = wgs)
  
# STEP 2: download ECMWF data: surface level --------------------------------------------

output_path_surface <- "/home/enourani/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Olga_cuckoos/wind_data_ecmwf/surface/"

vars_surface <- c("10m_u_component_of_wind" , "10m_v_component_of_wind")

#find year-month combos that we need data for
years <- unique(year(all_ck$timestamp))
months <- unique(month(all_ck$timestamp))
days <- c(1:31) %>% str_pad(2,"left","0")
hours <- c(0:23) %>% str_pad(2, "left", "0") %>% paste0(":00")


#start connection with CDS
wf_set_key(service = "cds")


#loop through years, months and pressure levels

lapply(years, function(yr){
  lapply(months, function(mn){
      
      # set up the request to the archive
      
      request <- list(
        "dataset_short_name" = "reanalysis-era5-single-levels",
        "product_type"   = "reanalysis",
        "variable"       = vars_surface,
        "year"           = yr,
        "month"          = mn,
        "day"            = days,
        "time"           = hours,
        "area"           = c(57.32, 92, -19.8, 163.3), #North/West/South/East
        "format"         = "netcdf",
        "target"         = paste0(yr, "_", mn, "_surface.nc"))
      
      
      wf_request(user = "27732",
                 request = request,
                 transfer = TRUE,
                 path = output_path_surface,
                 verbose = TRUE)
      
    })
  })


# STEP 3: PLOT!!--------------------------------------------

# group the data by yr_mnth
data_per_month <- all_ck %>% 
  mutate(year = year(timestamp),
         month = month(timestamp),
         day = day(timestamp),
         hour = hour(timestamp),
         yr_mn = paste(year, month, sep = "_"),
         day_hr = paste0(day, "_", hour))

data_per_month <- split(data_per_month, data_per_month$yr_mn)

#re-write the unique group names as a character string with ORs to use as the pattern for listing files
months_to_plot <- paste(names(data_per_month), collapse = "|")

#list ERA5 files with the same month-year data as the cuckoos
file_ls <- list.files("/home/enourani/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Olga_cuckoos/wind_data_ecmwf/surface", pattern = months_to_plot,
                      full.names = TRUE)

#prepare background layers
world <- ne_countries(scale = "medium", returnclass = "sf")
region <- world %>% 
  st_crop(st_bbox(file_ls[[1]] %>% rast()))

lapply(file_ls, function(x){ #for each month

  yr_mn <- str_sub(x, 128, -12)
  
  #extract tracking data for this month
  gps <- data_per_month[names(data_per_month) == yr_mn][[1]]
  
  #extract wind data for the month. There is one layer per hour
  wind <- rast(x)
 
  #make a plot for each unique hour of the wind data
  #Identify unique hours
  #unique_hours <- paste0(day(time(wind)), "_", hour(time(wind)))
  
  #go over each unique hour and create a plot
  lapply(unique(time(wind)), function(hr){
    
    #Identify unique hour to subset the gps data with
    unique_hour <- paste0(day(hr), "_", hour(hr))
  
    #convert wind raster layers (one per u and v) to one df and calculate wind speed ALSO reduce resolution 
    wind_hr <- subset(wind, time(wind) == hr) %>% 
      as.data.frame(wind_hr, xy = T) %>% 
      rename(u = u_1, v = v_1) %>% 
      mutate(wind_speed = sqrt(u^2 + v^2)) %>% 
      mutate(lat_lres = round(y),
             lon_lres = round(x)) %>% 
      group_by(lat_lres, lon_lres) %>% 
      summarise(u = mean(u, na.rm = T),
                v = mean(v, na.rm = T),
                wind_speed = mean(wind_speed)) %>% 
      ungroup() %>% 
      rename(lat = lat_lres,
             lon = lon_lres)
      
     
    
    #this contains data for all individuals at the unique hour
    gps_hr <- gps %>% filter(day_hr == unique_hour)
    
    plot <- ggplot() +
      geom_tile(data = wind_hr, aes(x = lon, y = lat, fill = wind_speed), alpha = 0.8)+
      geom_segment(data = wind_hr, 
                   aes(x = lon, xend = lon+u/10, y = lat, 
                       yend = lat+v/10), arrow = arrow(length = unit(0.12, "cm")), size = 0.3)+
      geom_sf(data = region, fill = "gray35", col = 1)+
      geom_point(data = gps_hr, aes(x = location.long, y = location.lat), 
                 size = 1.5, color = "white", fill = "red", shape = 21,) +
      #coord_sf(xlim = range(wind_hr$lon), ylim =  range(wind_hr$lat))+
      coord_sf(xlim = range(wind_hr$lon), ylim =  range(wind_hr$lat))+
      scale_fill_gradientn(colours = oce::oceColorsPalette(120), limits = c(0,23), 
                           na.value = "white", name = "Speed\n (m/s)")+
      theme_bw()+
      theme(axis.text = element_text(size = 12, colour = 1),
            legend.text = element_text(size = 10, colour = 1), 
            legend.title = element_text(size = 12, colour = 1),
            legend.position = c(0.1,0.13),
            legend.background = element_rect(colour = 1, fill = "white")) #+
      labs(x = NULL, y = NULL, title = wth %>%  filter(unique_hour == i) %>% .$date_time %>% .[1] %>% paste(x))
    
    
  })
  
})


  
  
})





######Old
#start with wind at the 10-m above ground level: median daily wind speed 
#source the wind direction function
source("data_prep/EnvironmentalData/airspeed_windsupport_crosswind.R")

#STEP1: list all files  #####

file_list <- list.files(path = "/media/enourani/Leer/ERA5_surface", pattern = ".nc", full.names = T)[-1]

#variable names to be extracted from the nc files
vname <- c("u10", "v10")

daily_summaries <- lapply(file_list,function(x){
  
  nc <- nc_open(x)
  
  #extract lon and lat
  lat <- ncvar_get(nc,'latitude')
  nlat <- dim(lat) 
  lon <- ncvar_get(nc,'longitude')
  nlon <- dim(lon) 
  
  #extract the time
  t <- ncvar_get(nc, "time")
  nt <- dim(t)
  
  #time unit: hours since 1900-01-01
  #ncatt_get(nc,'time')
  
  #convert the hours into date + hour
  timestamp <- as_datetime(c(t*60*60), origin = "1900-01-01 00:00")
  
  row_names <- expand.grid(lon, lat, timestamp)
  
  #extract data for variables
  var_df <- lapply(vname, function(i){
    df <- data.frame(as.vector(ncvar_get(nc, i)))
    colnames(df) <- paste0("data_", i)
    df
  }) %>% 
    bind_cols() %>% 
    bind_cols(row_names)
  
  colnames(var_df) <- c(vname, "lon", "lat", "date_time") #set column names
  
  #for each cell, aggregate over all values within each day (yday, year). take the mean and max, etc.
  #(d <- Sys.time())
  daily_df <- var_df %>%
    mutate(wind_speed_10m = sqrt(u10^2 + v10^2),
           wind_dir_from_10m = wind.directionFROM(u10, v10),
           yr = year(date_time),
           yday = yday(date_time)) %>% 
    group_by(lon, lat, yr, yday) %>% #there is only one value for year, but keep it here so it is retained in the final df
    summarise_at(.vars = vars("wind_speed_10m", "wind_dir_from_10m"), list(daily_avg = mean, daily_max = max, daily_min = min, daily_median = median, daily_var = var,
                                                                           daily_quant1 = ~ quantile(.x, probs = .25) , daily_quant3 = ~ quantile(.x, probs = .75)), na.rm = T) %>% 
    as.data.frame()
  #Sys.time()- d #26 minutes for each month
  
  saveRDS(daily_df, file = paste0("/media/enourani/6EB4C1E7B4C1B23F/WIND_speed_dir/daily_summaries/", str_sub(x, -18,-12), ".rds"))
  
  gc()
  
  print(paste0("file ",  str_sub(x, -18,-12), " done!"))
  
})


