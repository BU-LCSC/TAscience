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
  # tmp 	monthly average daily mean temperature 	degrees Celsius
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru/cru_ts4.06.1901.2021.tmp.dat.nc',lyrs=peBe)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru/cru_ts4.06.1901.2021.tmp.dat.nc',lyrs=peAf)
}else if(vv==2){ #DJF
  # tmp 	monthly average daily mean temperature 	degrees Celsius
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru/cru_ts4.06.1901.2021.tmp.dat.nc',lyrs=peBeDJF)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru/cru_ts4.06.1901.2021.tmp.dat.nc',lyrs=peAfDJF)
}else if(vv==3){ #MAM
  # tmp 	monthly average daily mean temperature 	degrees Celsius
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru/cru_ts4.06.1901.2021.tmp.dat.nc',lyrs=peBeMAM)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru/cru_ts4.06.1901.2021.tmp.dat.nc',lyrs=peAfMAM)
}else if(vv==4){ #JJA
  # tmp 	monthly average daily mean temperature 	degrees Celsius
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru/cru_ts4.06.1901.2021.tmp.dat.nc',lyrs=peBeJJA)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru/cru_ts4.06.1901.2021.tmp.dat.nc',lyrs=peAfJJA)
}else if(vv==5){ #SON
  # tmp 	monthly average daily mean temperature 	degrees Celsius
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru/cru_ts4.06.1901.2021.tmp.dat.nc',lyrs=peBeSON)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/cru/cru_ts4.06.1901.2021.tmp.dat.nc',lyrs=peAfSON)

}else if(vv==6){
  # Radiation
  mapB <- vector('list',(7*12)); tt <- 1
  for(i in 1:7){
    for(j in 1:12){
      mth <- sprintf('%02d',j)
      tmp <- rast(paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/gldas/GLDAS_NOAH025_M.A200',i,mth,'.021.nc4'),lyrs=35)   
      mapB[[tt]] <- tmp
      tt <- tt + 1
    }  
  }
  mapB <- rast(mapB)
  
  mapA <- vector('list',(7*12)); tt <- 1
  for(i in 15:21){
    for(j in 1:12){
      mth <- sprintf('%02d',j)
      tmp <- rast(paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/gldas/GLDAS_NOAH025_M.A20',i,mth,'.021.nc4'),lyrs=35) 
      mapA[[tt]] <- tmp
      tt <- tt + 1
    }  
  }
  mapA <- rast(mapA)
  
}else if(vv==7){
  # Radiation; DJF
  mapB <- vector('list',(7*3)); tt <- 1
  for(i in 1:7){
    for(j in c(1,2,12)){
      mth <- sprintf('%02d',j)
      tmp <- rast(paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/gldas/GLDAS_NOAH025_M.A200',i,mth,'.021.nc4'),lyrs=35)   
      mapB[[tt]] <- tmp
      tt <- tt + 1
    }  
  }
  mapB <- rast(mapB)
  
  mapA <- vector('list',(7*3)); tt <- 1
  for(i in 15:21){
    for(j in c(1,2,12)){
      mth <- sprintf('%02d',j)
      tmp <- rast(paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/gldas/GLDAS_NOAH025_M.A20',i,mth,'.021.nc4'),lyrs=35) 
      mapA[[tt]] <- tmp
      tt <- tt + 1
    }  
  }
  mapA <- rast(mapA)
  
}else if(vv==8){
  # Radiation; MAM
  mapB <- vector('list',(7*3)); tt <- 1
  for(i in 1:7){
    for(j in c(3,4,5)){
      mth <- sprintf('%02d',j)
      tmp <- rast(paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/gldas/GLDAS_NOAH025_M.A200',i,mth,'.021.nc4'),lyrs=35)   
      mapB[[tt]] <- tmp
      tt <- tt + 1
    }  
  }
  mapB <- rast(mapB)
  
  mapA <- vector('list',(7*3)); tt <- 1
  for(i in 15:21){
    for(j in c(3,4,5)){
      mth <- sprintf('%02d',j)
      tmp <- rast(paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/gldas/GLDAS_NOAH025_M.A20',i,mth,'.021.nc4'),lyrs=35) 
      mapA[[tt]] <- tmp
      tt <- tt + 1
    }  
  }
  mapA <- rast(mapA)
  
}else if(vv==9){
  # Radiation; JJA
  mapB <- vector('list',(7*3)); tt <- 1
  for(i in 1:7){
    for(j in c(6,7,8)){
      mth <- sprintf('%02d',j)
      tmp <- rast(paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/gldas/GLDAS_NOAH025_M.A200',i,mth,'.021.nc4'),lyrs=35)   
      mapB[[tt]] <- tmp
      tt <- tt + 1
    }  
  }
  mapB <- rast(mapB)
  
  mapA <- vector('list',(7*3)); tt <- 1
  for(i in 15:21){
    for(j in c(6,7,8)){
      mth <- sprintf('%02d',j)
      tmp <- rast(paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/gldas/GLDAS_NOAH025_M.A20',i,mth,'.021.nc4'),lyrs=35) 
      mapA[[tt]] <- tmp
      tt <- tt + 1
    }  
  }
  mapA <- rast(mapA)

}else if(vv==10){
  # Radiation; SON
  mapB <- vector('list',(7*3)); tt <- 1
  for(i in 1:7){
    for(j in c(9,10,11)){
      mth <- sprintf('%02d',j)
      tmp <- rast(paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/gldas/GLDAS_NOAH025_M.A200',i,mth,'.021.nc4'),lyrs=35)   
      mapB[[tt]] <- tmp
      tt <- tt + 1
    }  
  }
  mapB <- rast(mapB)
  
  mapA <- vector('list',(7*3)); tt <- 1
  for(i in 15:21){
    for(j in c(9,10,11)){
      mth <- sprintf('%02d',j)
      tmp <- rast(paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/gldas/GLDAS_NOAH025_M.A20',i,mth,'.021.nc4'),lyrs=35) 
      mapA[[tt]] <- tmp
      tt <- tt + 1
    }  
  }
  mapA <- rast(mapA)
  
}else if(vv==11){ # 1m
  # SEPI
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/spei/spei01.nc',lyrs=peBe)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/spei/spei01.nc',lyrs=peAf)
}else if(vv==12){ # 3m
  # SEPI
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/spei/spei03.nc',lyrs=peBe)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/spei/spei03.nc',lyrs=peAf)
}else if(vv==13){ # 6m
  # SEPI
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/spei/spei06.nc',lyrs=peBe)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/spei/spei06.nc',lyrs=peAf)
}else if(vv==14){ # 12m
  # SEPI
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/spei/spei12.nc',lyrs=peBe)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/spei/spei12.nc',lyrs=peAf)
}else if(vv==15){ # 36m 
  # SEPI
  mapB <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/spei/spei36.nc',lyrs=peBe)
  mapA <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/spei/spei36.nc',lyrs=peAf)
}


############################################################
mapBm <- median(mapB)
mapAm <- median(mapA)
mapDm <- mapAm - mapBm

# Save changes in individual variables
outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/parts/climate/'
if (!dir.exists(outDir)) {dir.create(outDir)}

imgBase <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_sig/merge/1_merge_01.tif')
mapDm <- resample(mapDm,imgBase)
writeRaster(mapDm,filename=paste0(outDir,'1_chg_cli_dif_',sprintf('%02d',vv),'.tif'),overwrite=TRUE)


