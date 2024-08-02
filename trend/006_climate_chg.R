rm(list = ls())

library(terra)
library(sf)


############################################################
# From bash code
args <- commandArgs()
print(args)

vv <- as.numeric(substr(args[3],1,3))
# vv <- 3


############################################################
# Get climate data
peBe <- 1201:1284
peAf <- 1381:1464

if(vv==1){
  # tmp 	monthly average daily mean temperature 	degrees Celsius
  mapB <- vector('list',(7*12)); tt <- 1
  for(i in 1:7){
    for(j in 1:12){
      mth <- sprintf('%02d',j)
      tmp <- rast(paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/gldas/GLDAS_NOAH025_M.A200',i,mth,'.021.nc4'),lyrs=32)   
      mapB[[tt]] <- tmp
      tt <- tt + 1
    }  
  }
  mapB <- rast(mapB)
  
  mapA <- vector('list',(7*12)); tt <- 1
  for(i in 16:22){
    for(j in 1:12){
      mth <- sprintf('%02d',j)
      tmp <- rast(paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/gldas/GLDAS_NOAH025_M.A20',i,mth,'.021.nc4'),lyrs=32) 
      mapA[[tt]] <- tmp
      tt <- tt + 1
    }  
  }
  mapA <- rast(mapA)
  
}else if(vv==2){ #DJF
  # tmp 	monthly average daily mean temperature 	degrees Celsius
  mapB <- vector('list',(7*3)); tt <- 1
  for(i in 1:7){
    for(j in c(1,2,12)){
      mth <- sprintf('%02d',j)
      if(j==12){
        tmp <- rast(paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/gldas/GLDAS_NOAH025_M.A200',(i-1),mth,'.021.nc4'),lyrs=32)   
      }else{
        tmp <- rast(paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/gldas/GLDAS_NOAH025_M.A200',i,mth,'.021.nc4'),lyrs=32)     
      }
      mapB[[tt]] <- tmp
      tt <- tt + 1
    }  
  }
  mapB <- rast(mapB)
  
  mapA <- vector('list',(7*3)); tt <- 1
  for(i in 16:22){
    for(j in c(1,2,12)){
      mth <- sprintf('%02d',j)
      if(j==12){
        tmp <- rast(paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/gldas/GLDAS_NOAH025_M.A20',(i-1),mth,'.021.nc4'),lyrs=32)   
      }else{
        tmp <- rast(paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/gldas/GLDAS_NOAH025_M.A20',i,mth,'.021.nc4'),lyrs=32)   
      }
      mapA[[tt]] <- tmp
      tt <- tt + 1
    }  
  }
  mapA <- rast(mapA)
  
}else if(vv==3){ #MAM
  # tmp 	monthly average daily mean temperature 	degrees Celsius
  mapB <- vector('list',(7*3)); tt <- 1
  for(i in 1:7){
    for(j in c(3,4,5)){
      mth <- sprintf('%02d',j)
      tmp <- rast(paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/gldas/GLDAS_NOAH025_M.A200',i,mth,'.021.nc4'),lyrs=32)   
      mapB[[tt]] <- tmp
      tt <- tt + 1
    }  
  }
  mapB <- rast(mapB)
  
  mapA <- vector('list',(7*3)); tt <- 1
  for(i in 16:22){
    for(j in c(3,4,5)){
      mth <- sprintf('%02d',j)
      tmp <- rast(paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/gldas/GLDAS_NOAH025_M.A20',i,mth,'.021.nc4'),lyrs=32) 
      mapA[[tt]] <- tmp
      tt <- tt + 1
    }  
  }
  mapA <- rast(mapA)
 
}else if(vv==4){ #JJA
  # tmp 	monthly average daily mean temperature 	degrees Celsius
  mapB <- vector('list',(7*3)); tt <- 1
  for(i in 1:7){
    for(j in c(6,7,8)){
      mth <- sprintf('%02d',j)
      tmp <- rast(paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/gldas/GLDAS_NOAH025_M.A200',i,mth,'.021.nc4'),lyrs=32)   
      mapB[[tt]] <- tmp
      tt <- tt + 1
    }  
  }
  mapB <- rast(mapB)
  
  mapA <- vector('list',(7*3)); tt <- 1
  for(i in 16:22){
    for(j in c(6,7,8)){
      mth <- sprintf('%02d',j)
      tmp <- rast(paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/gldas/GLDAS_NOAH025_M.A20',i,mth,'.021.nc4'),lyrs=32) 
      mapA[[tt]] <- tmp
      tt <- tt + 1
    }  
  }
  mapA <- rast(mapA)
 
}else if(vv==5){ #SON
  # tmp 	monthly average daily mean temperature 	degrees Celsius
  mapB <- vector('list',(7*3)); tt <- 1
  for(i in 1:7){
    for(j in c(9,10,11)){
      mth <- sprintf('%02d',j)
      tmp <- rast(paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/gldas/GLDAS_NOAH025_M.A200',i,mth,'.021.nc4'),lyrs=32)   
      mapB[[tt]] <- tmp
      tt <- tt + 1
    }  
  }
  mapB <- rast(mapB)
  
  mapA <- vector('list',(7*3)); tt <- 1
  for(i in 16:22){
    for(j in c(9,10,11)){
      mth <- sprintf('%02d',j)
      tmp <- rast(paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/gldas/GLDAS_NOAH025_M.A20',i,mth,'.021.nc4'),lyrs=32) 
      mapA[[tt]] <- tmp
      tt <- tt + 1
    }  
  }
  mapA <- rast(mapA)

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
      if(j==12){
        tmp <- rast(paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/gldas/GLDAS_NOAH025_M.A200',(i-1),mth,'.021.nc4'),lyrs=35)   
      }else{
        tmp <- rast(paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/gldas/GLDAS_NOAH025_M.A200',i,mth,'.021.nc4'),lyrs=35)     
      }
      mapB[[tt]] <- tmp
      tt <- tt + 1
    }  
  }
  mapB <- rast(mapB)
  
  mapA <- vector('list',(7*3)); tt <- 1
  for(i in 15:21){
    for(j in c(1,2,12)){
      mth <- sprintf('%02d',j)
      if(j==12){
        tmp <- rast(paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/gldas/GLDAS_NOAH025_M.A20',(i-1),mth,'.021.nc4'),lyrs=35) 
      }else{
        tmp <- rast(paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/climate/gldas/GLDAS_NOAH025_M.A20',i,mth,'.021.nc4'),lyrs=35)   
      }
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
outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/re/rasters/climate/'
if (!dir.exists(outDir)) {dir.create(outDir)}

imgBase <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/re/rasters/merge/1_merge_sd_01.tif')
mapDm <- resample(mapDm,imgBase)
writeRaster(mapDm,filename=paste0(outDir,'1_chg_cli_dif_',sprintf('%02d',vv),'.tif'),overwrite=TRUE)


############################################################
chg <- rast('/projectnb/modislc/users/mkmoon/TAscience/trend/data/re/rasters/climate/1_chg_cli_dif_11.tif')
plot(chg)
hist(chg)
