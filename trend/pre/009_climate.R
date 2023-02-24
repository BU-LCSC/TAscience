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
peBe <- 1201:1284
peAf <- 1369:1452

# Temperature
mapBsT <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmp.dat.nc',lyrs=peBe)
mapAsT <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmp.dat.nc',lyrs=peAf)

mapBmT <- median(mapBsT)
mapAmT <- median(mapAsT)

mapDmT <- mapAmT - mapBmT

plot(mapBmT)
plot(mapAmT)
plot(mapDmT)


# VPD
mapBsV <- vector('list',84)
mapAsV <- vector('list',84)
for(i in 1:length(peBe)){
  mapTb <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmp.dat.nc',lyrs=peBe[i])  
  mapVb <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.vap.dat.nc',lyrs=peBe[i])  
  
  mapTa <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmp.dat.nc',lyrs=peAf[i])  
  mapVa <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.vap.dat.nc',lyrs=peAf[i])  
  
  mapvpsatb <- (610.7 * 10^((7.5*mapTb)/(237.3+mapTb))) / 1000
  mapvpsata <- (610.7 * 10^((7.5*mapTa)/(237.3+mapTa))) / 1000
  
  mapBsV[[i]] <- mapvpsatb - mapVb*0.1
  mapAsV[[i]] <- mapvpsata - mapVa*0.1
  
  print(i)
}

mapBsV <- rast(mapBsV)
mapAsV <- rast(mapAsV)

mapBmV <- median(mapBsV)
mapAmV <- median(mapAsV)

mapDmV <- mapAmV - mapBmV

plot(mapBmV)
plot(mapAmV)
plot(mapDmV)

# SPEI
mapBsS <- vector('list',84)
mapAsS <- vector('list',84)
for(i in 1:length(peBe)){
  mapPrb <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.pre.dat.nc',lyrs=peBe[i])  
  mapPeb <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.pet.dat.nc',lyrs=peBe[i])  
  
  mapPra <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.pre.dat.nc',lyrs=peAf[i])  
  mapPea <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.pet.dat.nc',lyrs=peAf[i])  
  
  mapBsS[[i]] <- mapPrb - mapPeb*30
  mapAsS[[i]] <- mapPra - mapPea*30
  
  print(i)
}

mapBsS <- rast(mapBsS)
mapAsS <- rast(mapAsS)

mapBmS <- median(mapBsS)
mapAmS <- median(mapAsS)

mapDmS <- mapAmS - mapBmS

plot(mapBmS)
plot(mapAmS)
plot(mapDmS)


############################################################
# Get tile list
mcd12q2_path <- '/projectnb/modislc/projects/sat/data/mcd12q/q2/c61'
file <- list.files(path=paste0(mcd12q2_path,'/2001'),full.names=T)
tile_list <- substr(file,74,79)

outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/'

options(warn=-1)
mapTr <- project(mapDmT,crs(imgBase))
mapVr <- project(mapDmV,crs(imgBase))
mapSr <- project(mapDmS,crs(imgBase))

for(tt in 1:length(tile_list)){
  imgBase <- rast(unlist(gdal_subdatasets(file[tt]))[12],lyrs=1)  
  
  mapT <- crop(mapTr,imgBase)
  mapV <- crop(mapVr,imgBase)
  mapS <- crop(mapSr,imgBase)
  
  mapT <- resample(mapT,imgBase)
  mapV <- resample(mapV,imgBase)
  mapS <- resample(mapS,imgBase)
  
  writeRaster(mapT,filename=paste0(outDir,'tmp/tmp_',tile_list[tt],'.tif'),overwrite=T)
  writeRaster(mapV,filename=paste0(outDir,'vpd/vpd_',tile_list[tt],'.tif'),overwrite=T)
  writeRaster(mapS,filename=paste0(outDir,'spe/spe_',tile_list[tt],'.tif'),overwrite=T)
  
  print(tt)
}

plot(mapT)
plot(mapV)
plot(mapS)
