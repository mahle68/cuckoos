#comparison of used vs available conditions


library(tidyverse)
library(move)
library(lubridate)
library(mapview)
library(CircStats)
library(circular)
library(fitdistrplus)
library(ggridges)
library(ggnewscale)
library(oce)

meters_proj <- CRS("+proj=moll +ellps=WGS84")
wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs")
source("/home/enourani/ownCloud/Work/Projects/functions.R")

setwd("/home/enourani/ownCloud/Work/Collaborations/cuckoos/Olga_cuckoos/")

#open sea-crossing tracks! prepared in track_annotation&plots.R
cck <- readRDS("/home/enourani/ownCloud/Work/Collaborations/Olga_cuckoos/seacrossing_tracks.rds") %>% 
  as("Spatial") %>% 
  as.data.frame()  %>% 
  mutate(timestamp = as.POSIXct(strptime(timestamp,format = "%Y-%m-%d %H:%M:%S"),tz = "UTC")) %>% 
  arrange(tag.local.identifier,timestamp)

# STEP1: estimate ta and sl distributions. skip bursts-------------------------------------------------------

mv <- move(x = cck$coords.x1, y = cck$coords.x2, time = cck$timestamp, proj = wgs, data = cck, animal = cck$individual.local.identifier)

#calculate step length and turning angle for all tracks

tracks_df <- lapply(split(mv), function(track){
  
  track$step_length <- c(move::distance(track),NA) 
  
  track$turning_angle <- c(NA,turnAngleGc(track),NA)
 
  track_df <- track %>% 
    as.data.frame()
  
  track_df

  }) %>% 
  reduce(rbind)


#estimate von Mises parameters for turning angles
#calculate the averages (mu).steps: 1) convert to radians. step 2) calc mean of the cosines and sines. step 3) take the arctan. OR use circular::mean.circular
mu <- mean.circular(rad(tracks_df$turning_angle[complete.cases(tracks_df$turning_angle)]))
kappa <- est.kappa(rad(tracks_df$turning_angle[complete.cases(tracks_df$turning_angle)]))

#estimate gamma distribution for step lengths and CONVERT TO KM!!! :p
sl <- tracks_df$step_length[complete.cases(tracks_df$step_length) & between(tracks_df$step_length, 0, 2000000)]/1000 #remove 0s and NAs and the outlier
fit.gamma1 <- fitdist(sl/10, distr = "gamma", method = "mse")


par(mfrow=c(1,2))
hist(sl, freq=F, main="", xlab = "Step length (km)")
plot(function(x) dgamma(x, shape = fit.gamma1$estimate[[1]],
                        rate = fit.gamma1$estimate[[2]]), add = TRUE, from = 0.1, to = 150, col = "blue")
hist(rad(tracks_df$turning_angle[complete.cases(tracks_df$turning_angle)]),freq=F,main="",xlab="Turning angles (radians)")
plot(function(x) dvonmises(x, mu = mu, kappa = kappa), add = TRUE, from = -3.5, to = 3.5, col = "red")

# STEP2: generate alternative steps-------------------------------------------------------

#remove track #1535200750  for now. it has no points over water, even though it is a sea-crossing track. add it back after interpolation.

tracks_sf <- tracks_df %>% 
  filter(individual.local.identifier != "#1535200750") %>% 
  st_as_sf(coords = c("coords.x1","coords.x2"), crs = wgs)

#used_av_track <- parLapply(mycl, sp(), function(track){ #for each track
  
n_alt <- 50

used_av_track <- lapply(split(tracks_sf,tracks_sf$individual.local.identifier),function(track){ #for each burst,
  #assign unique step id
  track$step_id <- 1:nrow(track)
    
    
    lapply(c(2:(nrow(track)-1)), function(this_point){ #first point has no bearing to calc turning angle, last point has no used endpoint.
      
      current_point<- track[this_point,]
      previous_point <- track[this_point-1,] #this is the previous point, for calculating turning angle.
      used_point <- track[this_point+1,] #this is the next point. the observed end-point of the step starting from the current_point
      
      #calculate bearing of previous point
      #prev_bearing<-bearing(previous_point,current_point) #am I allowing negatives?... no, right? then use NCEP.loxodrome
      prev_bearing <- NCEP.loxodrome.na(st_coordinates(previous_point)[,2], st_coordinates(current_point)[,2],
                                        st_coordinates(previous_point)[,1], st_coordinates(current_point)[,1])
      
      current_point_m <- st_transform(current_point, meters_proj) #convert to meters proj
      
      #randomly generate n alternative points
      rnd <- data.frame(turning_angle = as.vector(rvonmises(n = n_alt, mu = mu, kappa = kappa)), #randomly generate n step lengths and turning angles
                        step_length = rgamma(n = n_alt, shape = fit.gamma1$estimate[[1]], rate = fit.gamma1$estimate[[2]]) * 1000) %>% 
        #find the gepgraphic location of each alternative point; calculate bearing to the next point: add ta to the bearing of the previous point
        mutate(lon = st_coordinates(current_point_m)[,1] + step_length*cos(turning_angle),
               lat = st_coordinates(current_point_m)[,2] + step_length*sin(turning_angle))
      
      
      #covnert back to lat-lon proj
       rnd_sf <- rnd %>% 
        st_as_sf(coords = c("lon", "lat"), crs = meters_proj) %>%
        st_transform(wgs)
        
      
      #check visually
      # mapview(current_point, color = "red") + mapview(previous_point, color = "orange") + mapview(used_point, color = "yellow") + mapview(rnd_sp, color = "black", cex = 0.5)
      
      #put used and available points together
      df <- used_point %>%
        st_drop_geometry() %>% 
        rename(location.long = coords.x1.1,
               location.lat = coords.x2.1) %>% 
        slice(rep(row_number(), n_alt+1)) %>% #paste each row n_alt times for the used and alternative steps
        mutate(location.long = c(head(location.long,1),st_coordinates(rnd_sf)[,1]), #the coordinates were called x and y in the previous version
               location.lat = c(head(location.lat,1),st_coordinates(rnd_sf)[,2]),
               turning_angle = c(head(turning_angle,1),deg(rnd_sf$turning_angle)),
               step_length = c(head(step_length,1),rnd_sf$step_length),
               used = c(1,rep(0,n_alt)))  %>%
        #dplyr::select(-c("location.long","coords.x2")) %>% 
        rowwise() %>% 
        mutate(heading = NCEP.loxodrome.na(lat1 = st_coordinates(current_point)[,2], lat2 = location.lat, 
                                           lon1 = st_coordinates(current_point)[,1], lon2 = location.long)) %>% 
        as.data.frame()
      
      df
      
    }) %>% 
      reduce(rbind)
    
  }) %>% 
    reduce(rbind)


used_av_track <- used_av_track %>% 
  mutate(stratum = paste(individual.local.identifier, step_id, sep = "_"))

saveRDS(used_av_track, file = "used_av_n50.rds")

#create csv file for movebank. we dont want any NAs, so just keep the columns that are necessary and have no missing values
df <- used_av_track %>% 
  mutate(timestamp = paste(as.character(timestamp),"000",sep = ".")) %>% 
  dplyr::select("timestamp", "location.long", "location.lat", "individual.local.identifier", "tag.local.identifier", 
                "stratum", "turning_angle", "step_length", "heading", "used") %>% 
  drop_na() %>% 
  as.data.frame()

#rename columns
colnames(df)[c(2,3)] <- c("location-long","location-lat")

write.csv(df, "oriental_cuckoo_used_av_n50.csv") #annotate at the 900 mb pressure level


# STEP3: plot-------------------------------------------------------

#open and prep annotated data
ann <- read.csv("/home/enourani/ownCloud/Work/Collaborations/cuckoos/Olga_cuckoos/annotated/used_available_900mb/oriental_cuckoo_used_av_n50.csv-1361729087688694248.csv") %>% 
  dplyr::select(-1) %>% 
  mutate(timestamp,timestamp = as.POSIXct(strptime(timestamp,format = "%Y-%m-%d %H:%M:%S"),tz = "UTC"),
         wind_speed = sqrt(ECMWF.ERA5.PL.U.Wind^2 + ECMWF.ERA5.PL.V.Wind^2),
         delta_t = ECMWF.ERA5.SL.Sea.Surface.Temperature - ECMWF.ERA5.SL.Temperature..2.m.above.Ground.) %>% 
  rename(sst = ECMWF.ERA5.SL.Sea.Surface.Temperature,
         t2m = ECMWF.ERA5.SL.Temperature..2.m.above.Ground.,
         wind_u_900 = ECMWF.ERA5.PL.U.Wind,
         wind_v_900 = ECMWF.ERA5.PL.V.Wind) %>%
  mutate(wind_support = wind_support(u = wind_u_900, v = wind_v_900, heading = heading),
         cross_wind = cross_wind(u = wind_u_900, v = wind_v_900, heading = heading)) %>% 
  as.data.frame()

#prepare long form dataframe
long_df <- ann %>% 
  mutate(day_of_year = yday(timestamp)) %>% 
  group_by(individual.local.identifier) %>% 
  pivot_longer(cols = c("delta_t", "wind_speed", "wind_support", "cross_wind"),
               names_to = "variable_names",
               values_to = "variable_values")


### for used distributions: use original annotated data (from track_annotation&plots.R) because the used rows in the used_available file are fewer than original

sea_tracks <- readRDS("/home/enourani/ownCloud/Work/Collaborations/cuckoos/Olga_cuckoos/seacrossing_tracks.rds") %>% 
  mutate(timestamp,timestamp = as.POSIXct(strptime(timestamp,format = "%Y-%m-%d %H:%M:%S"),tz = "UTC"))

#convert to long form for plotting
long_sea <- sea_tracks %>% 
  as("Spatial") %>% 
  as.data.frame() %>% 
  filter(individual.local.identifier != "#1535200750") %>%  #remove the track with no actual points over the sea
  mutate(day_of_year = yday(timestamp)) %>% 
  group_by(individual.local.identifier) %>% 
  pivot_longer(cols = c("delta_t", "wind_speed", "wind_support", "cross_wind"),
               names_to = "variable_names",
               values_to = "variable_values") %>% 
  mutate(used = 1) %>% 
  dplyr::select("timestamp", "variable_values", "variable_names", "individual.local.identifier", "tag.local.identifier", "used") 


### for available distributions: use the used_available file
##apend the used and avilable together

plot_input <- long_df %>%  
  filter(used == 0) %>% 
  dplyr::select("timestamp", "variable_values", "variable_names", "individual.local.identifier", "tag.local.identifier", "used") %>% 
  full_join(long_sea)

cl <- oce::oceColorsPalette(10)[2]
            
#plot with 4 panels with ridges
X11(width = 9, height = 8)

png("/home/enourani/ownCloud/Work/Collaborations/cuckoos/Olga_cuckoos/figures/along_tracks_available.png", width = 9, height = 8, units = "in", res = 300)

ggplot(plot_input, aes(x = variable_values, y = individual.local.identifier, color = used)) +
  geom_density_ridges_gradient(data = plot_input %>%  filter(used == 0),  color = "gray65", point_color = "gray65", fill =  NA,
                               jittered_points = TRUE, position = position_points_jitter(width = 0.05, height = 0), scale = 1, rel_min_height = .01,
                               point_shape = "|", point_size = 1.5, size = 0.8)  + 
  geom_density_ridges_gradient(data = plot_input %>%  filter(used == 1), fill = NA, point_color = cl, color = cl,
                               jittered_points = TRUE, position = position_points_jitter(width = 0.05, height = 0), scale = 1, rel_min_height = .01,
                               point_shape = "|", point_size = 1.5,size = 0.8) +
  facet_wrap(~ variable_names, labeller = labeller(variable_names = c( "cross_wind" = "Corss wind (m/s)", 
                                                                       "delta_t" = "Delta T (??C)", 
                                                                       "wind_speed" = "Wind speed (m/s)",
                                                                       "wind_support" = "Wind support (m/s)"))) +
  scale_colour_manual(values = c("gray65", cl)) +
  labs(title = "Atmospheric conditions experienced along the sea-crossing tracks \n (blue = used; gray = available)", y = "", x = "") +
  theme_linedraw() +
  theme(legend.position = "bottom")

dev.off()
