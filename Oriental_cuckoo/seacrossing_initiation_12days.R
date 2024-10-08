# prepare for annotating the sea-crossing step with tailwind + tailwind 12 days prior to sea-crossing
#October 5. 2022
#Elham Nourani, PhD. Konstanz DE


library(tidyverse)
library(readxl)
library(sf)
library(raster)
library(mapview)
library(lubridate)

source("/home/enourani/ownCloud/Work/Projects/functions.R")
wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs")


### STEP 1: prepare data for annotation -------------------------------------------------------------------------------------------
#open coordinates sent by Olga

target_coords <- read_excel("/home/mahle68/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Olga_cuckoos/Coordinates for Elham.xlsx", range = "A2:C3", 
                      col_names = c("origin", "lat", "lon")) %>% 
  mutate(origin = str_sub(origin, start = 8))


sc <- read_excel("/home/mahle68/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Olga_cuckoos/Coordinates for Elham.xlsx", range = "A4:D9", 
                 col_names = c("ID", "lat", "lon", "timestamp")) %>% 
  mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
         origin = ifelse(str_detect(ID, "J"), "Japan", "China"),
         ID = str_sub(ID, start = 8),
         stage = "sea_crossing") #%>% 
#  slice(rep(1:n(), each = 2)) %>% 
#  arrange(ID)
#  group_by(ID) %>% 
#  mutate(stage = c("sea_crossing", "target"),
#         lat = c(lat, ifelse(origin == "Japan", targets[targets$origin == "Japan", "lat"], 
#                              targets[targets$origin == "China", "lat"])),
#         lon =  c(lon, ifelse(origin == "Japan", targets[targets$origin == "Japan", "lon"], 
#                               targets[targets$origin == "China", "lon"])))

  
complete_df <- sc %>% 
  mutate(lat = ifelse(origin == "Japan", as.numeric(target_coords[target_coords$origin == "Japan", "lat"]), 
                                                    as.numeric(target_coords[target_coords$origin == "China", "lat"])),
         lon = ifelse(origin == "Japan", as.numeric(target_coords[target_coords$origin == "Japan", "lon"]), 
                      as.numeric(target_coords[target_coords$origin == "China", "lon"])),
         stage = "target",
         timestamp = timestamp + 1) %>% #make sure the target points happen later than the sea_crossing points 
  full_join(sc) %>% 
  group_by(ID) %>% 
  arrange(timestamp) %>% 
  mutate(heading = ifelse(row_number() == nrow(.), NA, NCEP.loxodrome.na(lat1 = lat, lat2 = lead(lat,1), lon1 = lon, lon2 = lead(lon,1))))


#sanity check
dd <- st_as_sf(complete_df, coords = c("lon","lat"), crs = wgs)
mapview(dd, zcol = "ID")

#create alternative points (in time)
n_alt <- 12 #number of alternative points to create

alternatives_df <- complete_df %>% 
  filter(stage == "sea_crossing") %>% #remove the target rows. we only needed those to calculate heading
  slice(rep(1:n(), each = n_alt)) %>%  #repeat each row 12 times
  group_by(ID) %>% 
  mutate(days_to_add = c(0:11)) %>% 
  mutate(timestamp = timestamp - days(days_to_add)) %>% 
  ungroup() %>% 
  dplyr::select(-c("days_to_add", "stage")) %>% 
  mutate(timestamp = paste0(as.character(timestamp)," 00:00:00.000"))
  as.data.frame()


#prepare for movebank

colnames(alternatives_df)[c(2,3)] <- c("location-lat", "location-long")

write.csv(alternatives_df, file = "/home/enourani/ownCloud/Work/Collaborations/Olga_cuckoos/annotate_sea_crossing.csv")



### STEP 2: calculate wind support and crosswind -------------------------------------------------------------------------------------------

data <- read.csv("/home/enourani/ownCloud/Work/Collaborations/Olga_cuckoos/annotated/900mbar_alts/annotate_sea_crossing.csv-3896691569207389862.csv") %>% 
  mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>% 
  arrange(ID,desc(timestamp)) %>% 
  group_by(ID) %>% 
  mutate(wind_support = wind_support(u = ECMWF.ERA5.PL.U.Wind, v = ECMWF.ERA5.PL.V.Wind, heading = heading),
         cross_wind= cross_wind(u = ECMWF.ERA5.PL.U.Wind, v = ECMWF.ERA5.PL.V.Wind, heading = heading),
         wind_speed = sqrt(ECMWF.ERA5.PL.U.Wind^2 + ECMWF.ERA5.PL.V.Wind^2),
         wind_direction = (270-atan2(ECMWF.ERA5.PL.V.Wind,ECMWF.ERA5.PL.U.Wind)*180/pi)%%360,
         used = c(1,rep(0,11))) %>%  #adjust 11 if n of alternatives changes
  ungroup()

saveRDS(data, "/home/enourani/ownCloud/Work/Collaborations/Olga_cuckoos/12_day_comparisons.rds")


### STEP 3: Plots -------------------------------------------------------------------------------------------

data <- readRDS("/home/enourani/ownCloud/Work/Collaborations/Olga_cuckoos/12_day_comparisons.rds")


data <- data %>% 
  arrange(ID, timestamp) %>% 
  mutate(day_of_departure = ifelse(used == 1, "Yes", "No"),
         day_of_year = yday(timestamp)) %>% 
  as.data.frame()

#plot wind support against time
png("/home/enourani/ownCloud/Work/Collaborations/Olga_cuckoos/figures/wind_support_vs_days.png", width = 6, height = 6, units = "in", res = 300)

ggplot(data, aes(x = day_of_year, y = wind_support, color = day_of_departure, fill = day_of_departure)) + 
  geom_bar(stat = "identity") +
  #scale_fill_discrete(values = c("No","Yes"), name = "day of departure") +
  facet_wrap(~ID) +
  labs(title = "Wind support on day of crossing compared to \n 12 days prior to sea-crossing", y = "wind support", x = "day of year") +
  theme_bw() +
  theme(legend.position = "bottom")

 dev.off()

#plot cross wind against time.
png("/home/enourani/ownCloud/Work/Collaborations/Olga_cuckoos/figures/cross_wind_vs_days.png", width = 6, height = 6, units = "in", res = 300)

ggplot(data, aes(x = day_of_year, y = cross_wind, color = day_of_departure, fill = day_of_departure)) + 
  geom_bar(stat = "identity") +
  #scale_fill_discrete(values = c("No","Yes"), name = "day of departure") +
  facet_wrap(~ID) +
  labs(title = "Cross wind on day of crossing compared to \n 12 days prior to sea-crossing", y = "cross wind", x = "day of year") +
  theme_bw() +
  theme(legend.position = "bottom")

dev.off()


#plot cross wind against wind speed
png("/home/enourani/ownCloud/Work/Collaborations/Olga_cuckoos/figures/wind_speed_vs_days.png", width = 6, height = 6, units = "in", res = 300)

ggplot(data, aes(x = day_of_year, y = wind_speed, color = day_of_departure, fill = day_of_departure)) + 
  geom_bar(stat = "identity") +
  #scale_fill_discrete(values = c("No","Yes"), name = "day of departure") +
  facet_wrap(~ID) +
  labs(title = "Wind speed on day of crossing compared to \n 12 days prior to sea-crossing", y = "wind speed", x = "day of year") +
  theme_bw() +
  theme(legend.position = "bottom")

dev.off()

