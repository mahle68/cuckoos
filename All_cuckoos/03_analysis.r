#explore Cuckoos and wind
#Mar 27. 2023. Konstanz, DE
#Elham Nourani, PhD

library(tidyverse)
library(lubridate)
library(sf)
library(mapview)

wgs <- st_crs("+proj=longlat +datum=WGS84 +no_defs")

#open annotated file created in 02_annotation.r
wind_df <- readRDS("/home/mahle68/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Kasper_cuckoos/R_files/wind_annotations_jul23.rds")

#create a spatial object
wind_sf <- wind_df %>% 
  mutate(month = month(timestamp),
         tailwind = ifelse(wind_support >= 0, wind_support, NA),
         headwind = ifelse(wind_support <= 0, wind_support, NA),
         head_tail = ifelse(wind_support == 0, "zero", ifelse(wind_support < 0, "headwind", "tailwind"))) %>% 
  st_as_sf(coords = c("location.long", "location.lat"), crs = wgs)


mapview(wind_sf, zcol = "wind_support")
mapview(wind_sf, zcol = "month")
mapview(wind_sf, zcol = "tailwind")
mapview(wind_sf, zcol = "head_tail")

# how many rows of data does each track have
small_tracks <- wind_sf %>% 
  group_by(trackId) %>% 
  summarize(n = n()) %>% 
  filter(n < 9)

#convert to move2 object
mv_w <- mt_as_move2(wind_sf %>% filter(!(trackId %in% small_tracks$trackId)), 
                    time_column = "timestamp", track_id_column = "trackId") 

w_lines <- mt_track_lines(mv_w)

mapview(w_lines %>% filter(trackId == "X115590b_2014"))

#calculate distance traveled and cumulative distance traveled
dist <- mv_w %>% 
  mutate(distance = mt_distance(.),   #this is much faster than using st_distance on an sf object
         location_long = sf::st_coordinates(.)[,1],
         location_lat = sf::st_coordinates(.)[,2]) %>% 
  as.data.frame() %>%  
  group_by(trackId) %>% 
  arrange(timestamp, .by_group = TRUE) %>% 
  mutate(cs_dist = cumsum(distance) %>% set_units(km),
         cs_wspt = cumsum(tailwind),
         cs_hdwd = cumsum(abs(headwind))) %>% 
  ungroup()

plot(dist$cs_dist, dist$cs_wspt)
plot(dist$cs_dist, dist$cs_hdwd)

#entire track metrics
tracks <- dist %>% 
  group_by(trackId) %>% 
  summarize(total_dist = sum(distance, na.rm = T) %>% set_units(km),
            overall_cs_wspt = max(cs_wspt,na.rm = T)) %>% 
  mutate(avg_wspt = as.numeric(overall_cs_wspt/total_dist)) %>% 
  filter(avg_wspt < 0.16) #3rd quartile
            #avg_tailwind = mean(tailwind, na.rm = T),
            #avg_headwind = mean(abs(headwind), na.rm = T),
            #avg_wspd = mean(wind_speed, na.rm = T))

plot(tracks$total_dist, tracks$avg_wspt)
#plot(tracks$total_dist, tracks$avg_tailwind)
#plot(tracks$total_dist, tracks$avg_headwind)

ggplot(tracks, aes(total_dist, avg_wspd)) +
  geom_point() +
  geom_smooth(method = lm)


#compare wind speed distributions
df %>%
  pivot_longer(cols = pop_a:pop_total) %>%
  ggplot() + 
  geom_density(mapping = aes(x = log(value), 
                             color = name, fill = name), 
               alpha = 0.5) + 
  scale_color_manual(values = alpha(c( my_oka[1:3], "gray40"), 1),
                     labels = as_labeller(grp_names)) + 
  scale_fill_manual(values = alpha(c( my_oka[1:3], "gray40"), 0.6),
                    labels = as_labeller(grp_names)) + 
  labs(x = "Logged Population", y = "Density", 
       title = "Comparing Subgroups: Density", 
       color = "Group", 
       fill = "Group")




############## old




data <- readRDS("/home/enourani/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Kasper_cuckoos/R_files/wind_annotations.rds") %>% 
mutate(trackId = as.character(trackId)) 
  

#calculate cumulative wind support along each track. for this to work, i need to separate autumn vs spring migration tracks. 
#otherwise the cum distance is for the whole year, not one season. asked Kasper for this.

track_wind <- data %>% 
  group_by(trackId) %>% #302 tracks of 176 individuals
  summarise(Pop1 = head(Pop1, 1),
            Pop2 = head(Pop2, 1),
            cumm_distance = sum(distance, na.rm = T)/1000,
            individual.local.identifier = head(individual.local.identifier, 1),
            cumm_wspt = sum(wind_support, na.rm = T),
            avg_wspt = mean(wind_support, na.rm = T),
            max_wspt = max(wind_support, na.rm = T),
            min_wspd = min(wind_speed, na.rm = T),
            max_wspd = max(wind_speed, na.rm = T),
            avg_wspd = mean(wind_speed, na.rm = T))

#there are some extreme distance values
outliers <- track_wind %>% 
  filter(cumm_distance > 35000) #35000 is the max in Kasper's MS

outliers_sf <- data %>% 
  filter(trackId %in% outliers$trackId) %>% 
  st_as_sf(coords = c("location.long", "location.lat"), crs = wgs)

mapview(outliers_sf, zcol = "trackId")
  
#box plots
ggplot(aes(y = cumm_distance, x = Pop1), data = track_wind) + geom_boxplot()


#sample
set.seed(12)

sm <- data %>%
  filter(trackId %in% c("PJ________2017", "ICARUS.AR_2021")) %>% 
  st_as_sf(coords = c("location.long", "location.lat"), crs = wgs)

mapview(sm, zcol = "trackId")
