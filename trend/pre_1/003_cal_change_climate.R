rm(list = ls())

library(terra)
library(sf)


############################################################
# From bash code
args <- commandArgs()
print(args)

vv <- as.numeric(substr(args[3],1,3))
# vv <- 28


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

peBeDJF <- c(1199+1+12*0,1199+2+12*0,1199+3+12*0,
             1199+1+12*1,1199+2+12*1,1199+3+12*1,
             1199+1+12*2,1199+2+12*2,1199+3+12*2,
             1199+1+12*3,1199+2+12*3,1199+3+12*3,
             1199+1+12*4,1199+2+12*4,1199+3+12*4,
             1199+1+12*5,1199+2+12*5,1199+3+12*5,
             1199+1+12*6,1199+2+12*6,1199+3+12*6)
peBeMAM <- c(1202+1+12*0,1202+2+12*0,1202+3+12*0,
             1202+1+12*1,1202+2+12*1,1202+3+12*1,
             1202+1+12*2,1202+2+12*2,1202+3+12*2,
             1202+1+12*3,1202+2+12*3,1202+3+12*3,
             1202+1+12*4,1202+2+12*4,1202+3+12*4,
             1202+1+12*5,1202+2+12*5,1202+3+12*5,
             1202+1+12*6,1202+2+12*6,1202+3+12*6)
peBeJJA <- c(1205+1+12*0,1205+2+12*0,1205+3+12*0,
             1205+1+12*1,1205+2+12*1,1205+3+12*1,
             1205+1+12*2,1205+2+12*2,1205+3+12*2,
             1205+1+12*3,1205+2+12*3,1205+3+12*3,
             1205+1+12*4,1205+2+12*4,1205+3+12*4,
             1205+1+12*5,1205+2+12*5,1205+3+12*5,
             1205+1+12*6,1205+2+12*6,1205+3+12*6)
peBeSON <- c(1208+1+12*0,1208+2+12*0,1208+3+12*0,
             1208+1+12*1,1208+2+12*1,1208+3+12*1,
             1208+1+12*2,1208+2+12*2,1208+3+12*2,
             1208+1+12*3,1208+2+12*3,1208+3+12*3,
             1208+1+12*4,1208+2+12*4,1208+3+12*4,
             1208+1+12*5,1208+2+12*5,1208+3+12*5,
             1208+1+12*6,1208+2+12*6,1208+3+12*6)

peAfDJF <- c(1367+1+12*0,1367+2+12*0,1367+3+12*0,
             1367+1+12*1,1367+2+12*1,1367+3+12*1,
             1367+1+12*2,1367+2+12*2,1367+3+12*2,
             1367+1+12*3,1367+2+12*3,1367+3+12*3,
             1367+1+12*4,1367+2+12*4,1367+3+12*4,
             1367+1+12*5,1367+2+12*5,1367+3+12*5,
             1367+1+12*6,1367+2+12*6,1367+3+12*6)
peAfMAM <- c(1370+1+12*0,1370+2+12*0,1370+3+12*0,
             1370+1+12*1,1370+2+12*1,1370+3+12*1,
             1370+1+12*2,1370+2+12*2,1370+3+12*2,
             1370+1+12*3,1370+2+12*3,1370+3+12*3,
             1370+1+12*4,1370+2+12*4,1370+3+12*4,
             1370+1+12*5,1370+2+12*5,1370+3+12*5,
             1370+1+12*6,1370+2+12*6,1370+3+12*6)
peAfJJA <- c(1373+1+12*0,1373+2+12*0,1373+3+12*0,
             1373+1+12*1,1373+2+12*1,1373+3+12*1,
             1373+1+12*2,1373+2+12*2,1373+3+12*2,
             1373+1+12*3,1373+2+12*3,1373+3+12*3,
             1373+1+12*4,1373+2+12*4,1373+3+12*4,
             1373+1+12*5,1373+2+12*5,1373+3+12*5,
             1373+1+12*6,1373+2+12*6,1373+3+12*6)
peAfSON <- c(1376+1+12*0,1376+2+12*0,1376+3+12*0,
             1376+1+12*1,1376+2+12*1,1376+3+12*1,
             1376+1+12*2,1376+2+12*2,1376+3+12*2,
             1376+1+12*3,1376+2+12*3,1376+3+12*3,
             1376+1+12*4,1376+2+12*4,1376+3+12*4,
             1376+1+12*5,1376+2+12*5,1376+3+12*5,
             1376+1+12*6,1376+2+12*6,1376+3+12*6)


if(vv==1){
  # cld 	cloud cover 	percentage (%)
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.01.1901.2021.cld.dat.nc',lyrs=peBe)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.01.1901.2021.cld.dat.nc',lyrs=peAf)
}else if(vv==2){
  # dtr 	diurnal temperature range 	degrees Celsius
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.dtr.dat.nc',lyrs=peBe)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.dtr.dat.nc',lyrs=peAf)
}else if(vv==3){ #DJF
  # dtr 	diurnal temperature range	degrees Celsius
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.dtr.dat.nc',lyrs=peBeDJF)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.dtr.dat.nc',lyrs=peAfDJF)
}else if(vv==4){ #MAM
  # dtr 	diurnal temperature range	degrees Celsius
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.dtr.dat.nc',lyrs=peBeMAM)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.dtr.dat.nc',lyrs=peAfMAM)
}else if(vv==5){ #JJA
  # dtr 	diurnal temperature range	degrees Celsius
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.dtr.dat.nc',lyrs=peBeJJA)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.dtr.dat.nc',lyrs=peAfJJA)
}else if(vv==6){ #SON
  # dtr 	diurnal temperature range	degrees Celsius
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.dtr.dat.nc',lyrs=peBeSON)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.dtr.dat.nc',lyrs=peAfSON)
}else if(vv==7){
  # frs 	frost day frequency 	days
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.frs.dat.nc',lyrs=peBe)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.frs.dat.nc',lyrs=peAf)
}else if(vv==8){
  # pet 	potential evapotranspiration 	millimetres per day
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.pet.dat.nc',lyrs=peBe)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.pet.dat.nc',lyrs=peAf)
}else if(vv==9){
  # pre 	precipitation 	millimetres per month
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.pre.dat.nc',lyrs=peBe)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.pre.dat.nc',lyrs=peAf)
}else if(vv==10){
  # tmp 	monthly average daily mean temperature 	degrees Celsius
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmp.dat.nc',lyrs=peBe)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmp.dat.nc',lyrs=peAf)
}else if(vv==11){ #DJF
  # tmp 	monthly average daily mean temperature 	degrees Celsius
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmp.dat.nc',lyrs=peBeDJF)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmp.dat.nc',lyrs=peAfDJF)
}else if(vv==12){ #MAM
  # tmp 	monthly average daily mean temperature 	degrees Celsius
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmp.dat.nc',lyrs=peBeMAM)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmp.dat.nc',lyrs=peAfMAM)
}else if(vv==13){ #JJA
  # tmp 	monthly average daily mean temperature 	degrees Celsius
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmp.dat.nc',lyrs=peBeJJA)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmp.dat.nc',lyrs=peAfJJA)
}else if(vv==14){ #SON
  # tmp 	monthly average daily mean temperature 	degrees Celsius
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmp.dat.nc',lyrs=peBeSON)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmp.dat.nc',lyrs=peAfSON)
}else if(vv==15){
  # tmn 	monthly average daily minimum temperature 	degrees Celsius
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmn.dat.nc',lyrs=peBe)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmn.dat.nc',lyrs=peAf)
}else if(vv==16){ #DJF
  # tmn 	monthly average daily minimum temperature 	degrees Celsius
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmn.dat.nc',lyrs=peBeDJF)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmn.dat.nc',lyrs=peAfDJF)
}else if(vv==17){ #MAM
  # tmn 	monthly average daily minimum temperature 	degrees Celsius
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmn.dat.nc',lyrs=peBeMAM)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmn.dat.nc',lyrs=peAfMAM)
}else if(vv==18){ #JJA
  # tmn 	monthly average daily minimum temperature 	degrees Celsius
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmn.dat.nc',lyrs=peBeJJA)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmn.dat.nc',lyrs=peAfJJA)
}else if(vv==19){ #SON
  # tmn 	monthly average daily minimum temperature 	degrees Celsius
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmn.dat.nc',lyrs=peBeSON)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmn.dat.nc',lyrs=peAfSON)  
}else if(vv==20){
  # tmx 	monthly average daily maximum temperature 	degrees Celsius
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmx.dat.nc',lyrs=peBe)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmx.dat.nc',lyrs=peAf)
}else if(vv==21){ #DJF
  # tmx 	monthly average daily maximum temperature 	degrees Celsius
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmx.dat.nc',lyrs=peBeDJF)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmx.dat.nc',lyrs=peAfDJF)
}else if(vv==22){ #MAM
  # tmx 	monthly average daily maximum temperature 	degrees Celsius
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmx.dat.nc',lyrs=peBeMAM)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmx.dat.nc',lyrs=peAfMAM)
}else if(vv==23){ #JJA
  # tmx 	monthly average daily maximum temperature 	degrees Celsius
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmx.dat.nc',lyrs=peBeJJA)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmx.dat.nc',lyrs=peAfJJA)
}else if(vv==24){ #SON
  # tmx 	monthly average daily maximum temperature 	degrees Celsius
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmx.dat.nc',lyrs=peBeSON)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.tmx.dat.nc',lyrs=peAfSON) 
}else if(vv==25){
  # vap 	vapour pressure 	hectopascals (hPa)
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.vap.dat.nc',lyrs=peBe)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.vap.dat.nc',lyrs=peAf)
}else if(vv==26){
  # wet 	wet day frequency 	days
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.wet.dat.nc',lyrs=peBe)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru_ts4.06.1901.2021.wet.dat.nc',lyrs=peAf)
}else if(vv==27){
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
  mapB <- rast(mapBsV)
  mapA <- rast(mapAsV)
}else if(vv==28){
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/gldas/GLDAS_NOAH025_M.A200101.021.nc4',lyrs=35) 
  
  
  
}


############################################################
# if(vv==3|vv==4|vv==5|vv==10){
#   mapBm <- sum(mapB)
#   mapAm <- sum(mapA)
# }else if(vv==12){
#   mapBm <- sum(mapB1) - sum(mapB2)
#   mapAm <- sum(mapA1) - sum(mapA2)
# }else{
  mapBm <- median(mapB)
  mapAm <- median(mapA)
# }
mapDm <- mapAm - mapBm

# Save changes in individual variables
outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_sig/climate/'
if (!dir.exists(outDir)) {dir.create(outDir)}

# writeRaster(mapBm,filename=paste0(outDir,'chg_cli_bfr_',sprintf('%02d',vv),'.tif'),overwrite=TRUE)
# writeRaster(mapAm,filename=paste0(outDir,'chg_cli_afr_',sprintf('%02d',vv),'.tif'),overwrite=TRUE)
# writeRaster(mapDm,filename=paste0(outDir,'chg_cli_dif_',sprintf('%02d',vv),'.tif'),overwrite=TRUE)


imgBase <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_sig/merge/1_merge_01.tif')
mapDm <- resample(mapDm,imgBase)
writeRaster(mapDm,filename=paste0(outDir,'1_chg_cli_dif_',sprintf('%02d',vv),'.tif'),overwrite=TRUE)




# ############################################################
# shp <- vect('/projectnb/modislc/users/mkmoon/TAscience/trend/data/shp/world-administrative-boundaries_edited.shp')
# 
# path <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_sig/climate/'
# vv <- c(6,7,8,5,4,10)
# 
# ########################
# # Metric changes
# rstc <- vector('list',6)
# dstc <- vector('list',6)
# for(i in 1:6){
#   file <- list.files(path,pattern=glob2rx(paste0('*dif*',sprintf('%02d',vv[i]),'.tif')),full.names=T)
#   rstc[[i]] <- rast(file)
#   
#   ds <- density(na.omit(values(rstc[[i]])))
#   dstc[[i]] <- ds
# }
# 
# 
# ##
# setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/figures/')
# png(filename=paste0('map_chg_cli_1.png'),width=14,height=8,units='in',res=300)
# 
# # average temp
# mycol <- rev(brewer.pal(11,'PiYG'))
# Pal   <- colorRampPalette(mycol)
# mycol <- Pal(300)
# 
# par(fig=c(0,0.5,0.66,1),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0))
# plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
# plot(shp,col='grey75',axes=F,add=T)
# r1 <- rstc[[1]]
# r1[r1 < -2] <- -2
# r1[r1 >  2] <-  2
# plot(r1,add=T,col=mycol,axes=F,
#      plg = list(ext = c(-30, 120, -45.5, -42), loc = "bottom", cex=0.9,
#                 at = seq(-2,2,1),bty="n",
#                 labels = c('< -2',-1:1,'> 2'),tck=2,
#                 cex.lab=1.2))
# plot(shp,add=T)
# box()
# par(fig=c(0.005,0.14,0.68,0.83),oma=c(0,0,1,1),mar=c(0,0,1,1),mgp=c(1.5,0.5,0),new=T)
# rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col='white',border=NA)
# par(fig=c(0.005,0.16,0.68,0.88),oma=c(0,0,1,1),mar=c(3,1,1,1),mgp=c(1.5,0.5,0),new=T)
# plot(dstc[[1]]$x,dstc[[1]]$y,xlim=c(-2,2),type='l',axes=F,lwd=2,xlab="Degrees",cex.lab=0.9)
# axis(1,c(-2,-1,0,1,2),cex.axis=0.9)
# abline(v=0,lty=5)
# 
# 
# # tmax
# par(fig=c(0,0.5,0.33,0.66),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0),new=T)
# plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
# plot(shp,col='grey75',axes=F,add=T)
# r1 <- rstc[[2]]
# r1[r1 < -2] <- -2
# r1[r1 >  2] <-  2
# plot(r1,add=T,col=mycol,axes=F,
#      plg = list(ext = c(-30, 120, -45.5, -42), loc = "bottom", cex=0.9,
#                 at = seq(-2,2,1),bty="n",
#                 labels = c('< -2',-1:1,'> 2'),tck=2,
#                 cex.lab=1.2))
# plot(shp,add=T)
# box()
# par(fig=c(0.005,0.14,0.34,0.49),oma=c(0,0,1,1),mar=c(0,0,1,1),mgp=c(1.5,0.5,0),new=T)
# rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col='white',border=NA)
# par(fig=c(0.005,0.16,0.34,0.54),oma=c(0,0,1,1),mar=c(3,1,1,1),mgp=c(1.5,0.5,0),new=T)
# plot(dstc[[1]]$x,dstc[[1]]$y,xlim=c(-2,2),type='l',axes=F,lwd=2,xlab="Degrees",cex.lab=0.9)
# axis(1,c(-2,-1,0,1,2),cex.axis=0.9)
# abline(v=0,lty=5)
# 
# 
# # tmin
# par(fig=c(0,0.5,0.00,0.33),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0),new=T)
# plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
# plot(shp,col='grey75',axes=F,add=T)
# r1 <- rstc[[3]]
# r1[r1 < -2] <- -2
# r1[r1 >  2] <-  2
# plot(r1,add=T,col=mycol,axes=F,
#      plg = list(ext = c(-30, 120, -45.5, -42), loc = "bottom", cex=0.9,
#                 at = seq(-2,2,1),bty="n",
#                 labels = c('< -2',-1:1,'> 2'),tck=2,
#                 cex.lab=1.2))
# plot(shp,add=T)
# box()
# par(fig=c(0.005,0.14,0.01,0.15),oma=c(0,0,1,1),mar=c(0,0,1,1),mgp=c(1.5,0.5,0),new=T)
# rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col='white',border=NA)
# par(fig=c(0.005,0.16,0.0,0.2),oma=c(0,0,1,1),mar=c(3,1,1,1),mgp=c(1.5,0.5,0),new=T)
# plot(dstc[[1]]$x,dstc[[1]]$y,xlim=c(-2,2),type='l',axes=F,lwd=2,xlab="Degrees",cex.lab=0.9)
# axis(1,c(-2,-1,0,1,2),cex.axis=0.9)
# abline(v=0,lty=5)
# 
# 
# # prcp
# mycol <- brewer.pal(11,'PiYG')
# Pal   <- colorRampPalette(mycol)
# mycol <- Pal(300)
# 
# par(fig=c(0.5,1,0.66,1),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0),new=T)
# plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
# plot(shp,col='grey75',axes=F,add=T)
# r1 <- rstc[[4]]
# r1[r1 < -10] <- -10
# r1[r1 >  10] <-  10
# plot(r1,add=T,col=mycol,axes=F,
#      plg = list(ext = c(-30, 120, -45.5, -42), loc = "bottom", cex=0.9,
#                 at = seq(-10,10,5),bty="n",
#                 labels = c('< -10',-5,0,5,'> 10'),tck=2,
#                 cex.lab=1.2))
# plot(shp,add=T)
# box()
# par(fig=c(0.51,0.64,0.68,0.83),oma=c(0,0,1,1),mar=c(0,0,1,1),mgp=c(1.5,0.5,0),new=T)
# rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col='white',border=NA)
# par(fig=c(0.51,0.66,0.68,0.88),oma=c(0,0,1,1),mar=c(3,1,1,1),mgp=c(1.5,0.5,0),new=T)
# plot(dstc[[4]]$x,dstc[[4]]$y,xlim=c(-6,6),type='l',axes=F,lwd=2,xlab="mm / month",cex.lab=0.9)
# axis(1,c(-6,-3,0,3,6),cex.axis=0.9)
# abline(v=0,lty=5)
# 
# 
# # pet
# par(fig=c(0.5,1,0.33,0.66),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0),new=T)
# plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
# plot(shp,col='grey75',axes=F,add=T)
# r1 <- rstc[[5]]
# r1[r1 < -0.4] <- -0.4
# r1[r1 >  0.4] <-  0.4
# plot(r1,add=T,col=mycol,axes=F,
#      plg = list(ext = c(-30, 120, -45.5, -42), loc = "bottom", cex=0.9,
#                 at = c(-0.4,-0.2,0,0.2,0.4),bty="n",
#                 labels = c('< -0.4',-0.2,0,0.2,'> 0.4'),tck=2,
#                 cex.lab=1.2))
# plot(shp,add=T)
# box()
# par(fig=c(0.51,0.64,0.34,0.49),oma=c(0,0,1,1),mar=c(0,0,1,1),mgp=c(1.5,0.5,0),new=T)
# rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col='white',border=NA)
# par(fig=c(0.51,0.66,0.34,0.54),oma=c(0,0,1,1),mar=c(3,1,1,1),mgp=c(1.5,0.5,0),new=T)
# plot(dstc[[5]]$x,dstc[[5]]$y,xlim=c(-0.3,0.3),type='l',axes=F,lwd=2,xlab="mm / Day",cex.lab=0.9)
# axis(1,c(-0.3,0,0.3),cex.axis=0.9)
# abline(v=0,lty=5)
# 
# 
# # wet
# par(fig=c(0.5,1,0.00,0.33),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0),new=T)
# plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
# plot(shp,col='grey75',axes=F,add=T)
# r1 <- rstc[[6]]
# r1[r1 < -2] <- -2
# r1[r1 >  2] <-  2
# plot(r1,add=T,col=mycol,axes=F,
#      plg = list(ext = c(-30, 120, -45.5, -42), loc = "bottom", cex=0.9,
#                 at = c(-2,-1,0,1,2),bty="n",
#                 labels = c('< -2',1,0,1,'> 2'),tck=2,
#                 cex.lab=1.2))
# plot(shp,add=T)
# box()
# par(fig=c(0.51,0.64,0.01,0.15),oma=c(0,0,1,1),mar=c(0,0,1,1),mgp=c(1.5,0.5,0),new=T)
# rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col='white',border=NA)
# par(fig=c(0.51,0.66,0.0,0.2),oma=c(0,0,1,1),mar=c(3,1,1,1),mgp=c(1.5,0.5,0),new=T)
# plot(dstc[[6]]$x,dstc[[6]]$y,xlim=c(-2,2),type='l',axes=F,lwd=2,xlab="Day",cex.lab=0.9)
# axis(1,c(-2,-1,0,1,2),cex.axis=0.9)
# abline(v=0,lty=5)
# 
# #
# mtext('a',3, -0.2,outer=T,adj=0.003,cex=1.5,font=2)
# mtext('b',3,-14.1,outer=T,adj=0.003,cex=1.5,font=2)
# mtext('c',3,-27.2,outer=T,adj=0.003,cex=1.5,font=2)
# mtext('d',3, -0.2,outer=T,adj=0.518,cex=1.5,font=2)
# mtext('e',3,-14.1,outer=T,adj=0.518,cex=1.5,font=2)
# mtext('f',3,-27.2,outer=T,adj=0.518,cex=1.5,font=2)
# 
# dev.off()
# 
