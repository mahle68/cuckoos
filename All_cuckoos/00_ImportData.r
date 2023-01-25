#install.packages("readxl")
library(readxl)
cookoos <- read_xlsx("./Data/Documents/All data_combined common cuckoo tracking studies_unmanipulated.xlsx")
cooked <- cookoos[,unlist(lapply(1:ncol(cookoos), function(x) any(!is.na(cookoos[,x]))))]
names(cooked) <- gsub("-", ".", names(cooked))
names(cooked) <- gsub(":", ".", names(cooked))
names(cooked) <- gsub(" ", ".", names(cooked))
names(cooked)[1] <- "Full_rec"
names(cooked)
library(move)
cooked <- as.data.frame(cooked)

cooked <- cooked[complete.cases(cbind(cooked$location.long, cooked$location.lat, as.POSIXct(cooked$timestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC"))),]
#drop obs without 
cooked <- cooked[!is.na(cooked$individual.local.identifier),]

cooked <- cooked[!duplicated(data.frame(ind=cooked$individual.local.identifier, DT=as.POSIXct(cooked$timestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC"))),]
cooked <- cooked[order(cooked$individual.local.identifier, as.POSIXct(cooked$timestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC")),]
cooks <- move(x=cooked$location.long, y=cooked$location.lat, time=as.POSIXct(cooked$timestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC"), 
              data=cooked, proj=CRS("+proj=longlat +ellps=WGS84"), animal=cooked$individual.local.identifier, sensor="ICARUS")

saveRDS(cooks, "./Data/Cookoos_as_move.rds")
  

