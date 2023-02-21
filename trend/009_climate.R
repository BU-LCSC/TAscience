rm(list = ls())

library(terra)
library(sf)

library(RColorBrewer)


############################################################
# From bash code
args <- commandArgs()
print(args)

tt <- as.numeric(substr(args[3],1,3))
# tt <- 69


############################################################
# Get tile list
mcd12q2_path <- '/projectnb/modislc/projects/sat/data/mcd12q/q2/c61'
file <- list.files(path=paste0(mcd12q2_path,'/2001'),full.names=T)
tile_list <- substr(file,74,79)

imgBase <- rast(unlist(gdal_subdatasets(file[tt]))[12],lyrs=1)



############################################################
# Get tile list
peBe <- 1201:1284
peAf <- 1369:1452

mapBstack <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmp.dat.nc',lyrs=peBe)
mapAstack <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmp.dat.nc',lyrs=peAf)

mapBmedi <- median(mapBstack)
mapAmedi <- median(mapAstack)

mapDiff <- mapAmedi - mapBmedi

plot(mapBmedi)
plot(mapAmedi)
plot(mapDiff)



