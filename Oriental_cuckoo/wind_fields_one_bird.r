#wind field animation for the one cuckoo that flew in the storm
#Elham Nourani. 11.09.2024, Konstanz, DE

library(tidyverse)
library(lubridate)
library(mapview)
library(rnaturalearth)

# STEP 1: load data for Russian birds ###################

RU_ck <- readRDS("/home/enourani/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Olga_cuckoos/hrly_wind_annotations.rds") %>% 
  dplyr::select("location.long", "location.lat", "timestamp", "individual.local.identifier")

cu06 <- RU_ck %>% 
  filter(individual.local.identifier == "CuOpt06")

RU_ck_sf <- RU_ck %>% 
  st_as_sf(coords = c("location.long", "location.lat"), crs = wgs)

mapview(RU_ck_sf, zcol = "individual.local.identifier")


cu06_fall <- cu06 %>% 
  mutate(year = year(timestamp),
         month = month(timestamp),
         day = day(timestamp),
         hour = hour(timestamp),
         yr_mn = paste(year, month, sep = "_"),
         day_hr = paste0(day, "_", hour),
         unique_hour = paste(yday(timestamp), hour, sep = "_")) %>% 
  filter(month %in% c(9,10))

#list ERA5 files with the same month-year data as the cuckoos
months_fall_migration <- c("9_2021", "10_2021")
months_to_plot <- paste(months_fall_migration, collapse = "|") #create a regular expression to include all the months of interest

#list wind files for the months of interest
file_ls <- list.files("/home/enourani/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Olga_cuckoos/atm_data", pattern = months_to_plot,
                      full.names = TRUE)

# merge two months together
atm_df <- file_ls %>%
  map(~ get(load(.x))) %>%
  reduce(bind_rows) %>%
  mutate(wind_speed = sqrt(u10^2 + v10^2), # m/s
         unique_hour = paste(yday, hour, sep = "_"))

#subset tracking data to be within the wind extent
cu06_fall <- cu06_fall %>% 
  filter(between(location.lat, min(atm_df$lat), max(atm_df$lat)),
         between(location.long, min(atm_df$lon), max(atm_df$lon)))


# STEP 2: PLOT ###################

#prepare background layers
world <- ne_countries(scale = "medium", returnclass = "sf")
region <- world %>% 
  st_crop(xmin = min(atm_df$lon), xmax = max(atm_df$lon), ymin = min(atm_df$lat), ymax = max(atm_df$lat))

path <- paste0("/home/enourani/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Olga_cuckoos/wind_fields/CuOpt06_interpolation/")

#find overlapping hours in the wind and tracking data
hrs_to_plot <- intersect(unique(cu06_fall$unique_hour), unique(atm_df$unique_hour))

for(i in hrs_to_plot){
  
  #conditional on whether there is tracking data or not
  plot <- ggplot() +
    geom_tile(data = atm_df %>% filter(unique_hour == i), aes(x = lon, y = lat, fill = wind_speed))+
    geom_segment(data = atm_df %>% filter(unique_hour == i), 
                 aes(x = lon, xend = lon+u10/10, y = lat, 
                     yend = lat+v10/10), arrow = arrow(length = unit(0.12, "cm")), linewidth = 0.3)+
    geom_sf(data = region, fill = "grey85", col = 1) +
    geom_point(data = cu06_fall %>%  filter(unique_hour == i), aes(x = location.long, y = location.lat), 
               size = 2, colour = "red") +
    #geom_point(data = track %>%  filter(unique_hour == i), aes(x = location.long, y = location.lat, colour = as.factor(avoidance)), 
    #           size = 1) +
    #scale_color_manual(values = c("avoided" = "red", "not_avoided" = "forestgreen")) +
    coord_sf(xlim = range(atm_df$lon), ylim =  range(atm_df$lat)) +
    scale_fill_gradientn(colours = oce::oceColorsPalette(120), limits = c(0,23), 
                         na.value = "white", name = "Speed\n (m/s)") +
    theme_bw()+
    theme(axis.text = element_text(size = 12, colour = 1),
          legend.text = element_text(size = 10, colour = 1), 
          legend.title = element_text(size = 12, colour = 1),
          legend.position = c(0.1,0.13),
          legend.background = element_rect(colour = 1, fill = "white")) +
    labs(x = NULL, y = NULL, title = atm_df %>%  filter(unique_hour == i) %>% .$date_time %>% .[1])
  
  #filename: replace "-", ":", and " " with "_" for compatibilty with different OSs
  filename <- gsub("[-: ]", "_", atm_df %>%  filter(unique_hour == i) %>% .$date_time)[1]
  
  ggsave(plot = plot, filename = paste0(path,"CuOpt06_", filename,".png"), #use the index of i instead of i itself. will be easier to make the animation later on 
         height = 10, width = 6, dpi = 300)
  
}


#### old
lapply(file_ls, function(x){
  
  #open the wind data for this month
  yr_month <- str_sub(x, 128, -12)
  
  #extract tracking data for this month
  gps <- cu06 %>% 
    filter(yr_month == yr_mn)
  
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
  
  