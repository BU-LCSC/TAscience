rm(list = ls())

library(terra)
library(sf)


############################################################
# From bash code
args <- commandArgs()
print(args)

vv <- as.numeric(substr(args[3],1,3))
# vv <- 12


############################################################
# https://crudata.uea.ac.uk/cru/data/hrg/
#
# label 	variable 	units
#
# cld 	cloud cover 	percentage (%)
# dtr 	diurnal temperature range 	degrees Celsius
# frs 	frost day frequency 	days
# pet 	potential evapotranspiration 	millimetres per day
# pre 	precipitation 	millimetres per month
# tmp 	monthly average daily mean temperature 	degrees Celsius
# tmn 	monthly average daily minimum temperature 	degrees Celsius
# tmx 	monthly average daily maximum temperature 	degrees Celsius
# vap 	vapour pressure 	hectopascals (hPa)
# wet 	wet day frequency 	days

############################################################
# Get climate data
peBe <- 1201:1284
peAf <- 1369:1452

if(vv==1){
  # cld 	cloud cover 	percentage (%)
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.01.1901.2021.cld.dat.nc',lyrs=peBe)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.01.1901.2021.cld.dat.nc',lyrs=peAf)
}else if(vv==2){
  # dtr 	diurnal temperature range 	degrees Celsius
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.dtr.dat.nc',lyrs=peBe)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.dtr.dat.nc',lyrs=peAf)
}else if(vv==3){
  # frs 	frost day frequency 	days
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.frs.dat.nc',lyrs=peBe)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.frs.dat.nc',lyrs=peAf)
}else if(vv==4){
  # pet 	potential evapotranspiration 	millimetres per day
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.pet.dat.nc',lyrs=peBe)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.pet.dat.nc',lyrs=peAf)
}else if(vv==5){
  # pre 	precipitation 	millimetres per month
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.pre.dat.nc',lyrs=peBe)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.pre.dat.nc',lyrs=peAf)
}else if(vv==6){
  # tmp 	monthly average daily mean temperature 	degrees Celsius
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmp.dat.nc',lyrs=peBe)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmp.dat.nc',lyrs=peAf)
}else if(vv==7){
  # tmn 	monthly average daily minimum temperature 	degrees Celsius
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmn.dat.nc',lyrs=peBe)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmn.dat.nc',lyrs=peAf)
}else if(vv==8){
  # tmx 	monthly average daily maximum temperature 	degrees Celsius
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmx.dat.nc',lyrs=peBe)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmx.dat.nc',lyrs=peAf)
}else if(vv==9){
  # vap 	vapour pressure 	hectopascals (hPa)
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.vap.dat.nc',lyrs=peBe)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.vap.dat.nc',lyrs=peAf)
}else if(vv==10){
  # wet 	wet day frequency 	days
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.wet.dat.nc',lyrs=peBe)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.wet.dat.nc',lyrs=peAf)
}else if(vv==11){
  # # VPD
  # mapBsV <- vector('list',84)
  # mapAsV <- vector('list',84)
  # for(i in 1:length(peBe)){
  #   mapTb <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmp.dat.nc',lyrs=peBe[i])  
  #   mapVb <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.vap.dat.nc',lyrs=peBe[i])  
  #   
  #   mapTa <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmp.dat.nc',lyrs=peAf[i])  
  #   mapVa <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.vap.dat.nc',lyrs=peAf[i])  
  #   
  #   mapvpsatb <- (610.7 * 10^((7.5*mapTb)/(237.3+mapTb))) / 1000
  #   mapvpsata <- (610.7 * 10^((7.5*mapTa)/(237.3+mapTa))) / 1000
  #   
  #   mapBsV[[i]] <- mapvpsatb - mapVb*0.1
  #   mapAsV[[i]] <- mapvpsata - mapVa*0.1
  #   
  #   print(i)
  # }
  # mapB <- rast(mapBsV)
  # mapA <- rast(mapAsV)
}else{
  # # SPEI
  # mapBsS <- vector('list',84)
  # mapAsS <- vector('list',84)
  # for(i in 1:length(peBe)){
  #   mapPrb <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.pre.dat.nc',lyrs=peBe[i:(i+11)])  
  #   mapPeb <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.pet.dat.nc',lyrs=peBe[i:(i+11)])  
  #   
  #   mapPra <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.pre.dat.nc',lyrs=peAf[i:(i+11)])  
  #   mapPea <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.pet.dat.nc',lyrs=peAf[i:(i+11)])  
  #   
  #   mapBsS[[i]] <- sum(mapPrb) - mapPeb*30
  #   mapAsS[[i]] <- mapPra - mapPea*30
  #   
  #   print(i)
  # }
  # mapB <- rast(mapBsS)
  # mapA <- rast(mapAsS)
}

############################################################
mapBm <- median(mapB)
mapAm <- median(mapA)
mapDm <- mapAmS - mapBmS

# Reproject and save changes in individual variables
outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_nor_qc/climate/'
if (!dir.exists(outDir)) {dir.create(outDir)}

writeRaster(mapBm,filename=paste0(outDir,'chg_cli_bfr_',sprintf('%02d',vv),'.tif'),overwrite=TRUE)
writeRaster(mapAm,filename=paste0(outDir,'chg_cli_afr_',sprintf('%02d',vv),'.tif'),overwrite=TRUE)
writeRaster(mapDm,filename=paste0(outDir,'chg_cli_dif_',sprintf('%02d',vv),'.tif'),overwrite=TRUE)


