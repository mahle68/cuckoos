#wind field animation for the one cuckoo that flew in the storm
#Elham Nourani. 11.09.2024, Konstanz, DE

library(tidyverse)
library(lubridate)
library(mapview)
library(rnaturalearth)

# STEP 1: load data for Russian birds ###################

RU_ck <- readRDS("/home/enourani/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Olga_cuckoos/hrly_wind_annotations.rds") %>% 
  dplyr::select("location.long", "location.lat", "timestamp", "individual.local.identifier")

cu02 <- RU_ck %>% 
  filter(individual.local.identifier == "CuOpt02")

RU_ck_sf <- RU_ck %>% 
  st_as_sf(coords = c("location.long", "location.lat"), crs = wgs)

mapview(RU_ck_sf, zcol = "individual.local.identifier")


cu02_fall <- cu02 %>% 
  mutate(year = year(timestamp),
         month = month(timestamp),
         day = day(timestamp),
         hour = hour(timestamp),
         yr_mn = paste(year, month, sep = "_"),
         day_hr = paste0(day, "_", hour),
         unique_hour = paste(yday(timestamp), hour, sep = "_")) %>% 
  filter(between(yday(timestamp), 249, 258)) #manually subset for fall migration

#list ERA5 files with the same month-year data as the cuckoos
months_fall_migration <- c("8_2021", "9_2021")
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
cu02_fall <- cu02_fall %>% 
  filter(between(location.lat, min(atm_df$lat), max(atm_df$lat)),
         between(location.long, min(atm_df$lon), max(atm_df$lon)))


# STEP 2: PLOT ###################

#prepare background layers
world <- ne_countries(scale = "medium", returnclass = "sf")
region <- world %>% 
  st_crop(xmin = min(atm_df$lon), xmax = max(atm_df$lon), ymin = min(atm_df$lat), ymax = max(atm_df$lat))

path <- paste0("/home/enourani/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Olga_cuckoos/wind_fields/CuOpt02_interpolation/")

#find overlapping hours in the wind and tracking data.
hrs_to_plot <- intersect(unique(cu02_fall$unique_hour), unique(atm_df$unique_hour))

for(i in hrs_to_plot){
  
  #conditional on whether there is tracking data or not
  (plot <- ggplot() +
    geom_tile(data = atm_df %>% filter(unique_hour == i), aes(x = lon, y = lat, fill = wind_speed))+
    geom_segment(data = atm_df %>% filter(unique_hour == i), 
                 aes(x = lon, xend = lon+u10/10, y = lat, 
                     yend = lat+v10/10), arrow = arrow(length = unit(0.12, "cm")), linewidth = 0.3)+
    geom_sf(data = region, fill = "black", col = 1) +
    geom_point(data = cu02_fall %>%  filter(unique_hour == i), aes(x = location.long, y = location.lat), 
               size = 3.5,  colour = "white", fill = "lightsalmon4", shape = 23) +
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
    labs(x = NULL, y = NULL, title = atm_df %>%  filter(unique_hour == i) %>% .$date_time %>% .[1] %>% format("%m-%d-%Y %H:%M:%S") %>% paste("UTC"))) #formatting is needed to make sure midnight hour is printed
  
  #filename: replace "-", ":", and " " with "_" for compatibilty with different OSs
  filename <- gsub("[-: ]", "_", atm_df %>%  filter(unique_hour == i) %>% .$date_time %>% format("%m-%d-%Y %H:%M:%S"))[1] 
  
  ggsave(plot = plot, filename = paste0(path,"CuOpt02_", filename,".png"), #use the index of i instead of i itself. will be easier to make the animation later on 
         height = 10, width = 6, dpi = 300)
  
}

#create animation of the maps. run the following code in the terminal
#ffmpeg -framerate 17 -pattern_type glob -i "*.png" CuOpt02_interpolated.mp4

