rm(list = ls())

library(terra)
library(sf)


############################################################
# From bash code
args <- commandArgs()
print(args)

tt <- as.numeric(substr(args[3],1,3))
mm <- as.numeric(substr(args[3],4,5))
# tt <- 67; mm <- 2


############################################################
# Get tile list
mcd12q2_path <- '/projectnb/modislc/projects/sat/data/mcd12q/q2/c61'
file <- list.files(path=paste0(mcd12q2_path,'/2001'),full.names=T)
tile_list <- substr(file,74,79)

# # Northern hemisphere tiles
# file <- file[which(as.numeric(substr(tile_list,5,6))<6)]
# tile_list <- tile_list[which(as.numeric(substr(tile_list,5,6))<6)]

imgBase <- rast(unlist(gdal_subdatasets(file[tt]))[10],lyrs=1)

############################################################
refReg <- vect('/projectnb/modislc/users/mkmoon/TAscience/trend/data/shp/IPCC-WGI-reference-regions-v4.shp')
pr2       <- rast(crs=crs(imgBase),extent=ext(imgBase),resolution=10000)
pr3       <- project(pr2,crs(refReg))
res(pr3)  <- 0.1

# Load metrics
dat1  <- matrix(NA,(ncol(pr3)*nrow(pr3)),21)
years <- 2001:2021
for(yy in 1:21){
  path <- paste0(mcd12q2_path,'/',years[yy])
  sstr <- paste0('*',tile_list[tt],'*')
  file <- list.files(path=path,pattern=glob2rx(sstr),full.names=T)
  sds  <- unlist(gdal_subdatasets(file))
  
  rQA <- rast(sds[12],lyrs=1)
  doy_offset <- as.integer(as.Date(paste(years[yy], "-1-1", sep="")) - as.Date("1970-1-1"))
  
  # DOY metrics
  if(mm==1)       {r1 <- rast(sds[ 2],lyrs=1) - doy_offset               # Greenup
  }else if(mm== 2){r1 <- rast(sds[ 3],lyrs=1) - doy_offset               # MidGreenup
  }else if(mm== 3){r1 <- rast(sds[ 4],lyrs=1) - doy_offset               # Peak
  }else if(mm== 4){r1 <- rast(sds[ 5],lyrs=1) - doy_offset               # Maturity
  }else if(mm== 5){r1 <- rast(sds[ 6],lyrs=1) - doy_offset               # Senescence
  }else if(mm== 6){r1 <- rast(sds[ 7],lyrs=1) - doy_offset               # MidGreendown
  }else if(mm== 7){r1 <- rast(sds[ 8],lyrs=1) - doy_offset               # Dormancy
  }else if(mm== 8){r1 <- rast(sds[ 8],lyrs=1) -  rast(sds[ 2],lyrs=1) # gsl_long
  }else if(mm== 9){r1 <- rast(sds[ 7],lyrs=1) -  rast(sds[ 3],lyrs=1) # gsl
  }else if(mm==10){r1 <- rast(sds[ 6],lyrs=1) -  rast(sds[ 5],lyrs=1) # gsl_peak
  }else if(mm==11){r1 <- rast(sds[ 5],lyrs=1) -  rast(sds[ 2],lyrs=1) # greenup_period
  }else if(mm==12){r1 <- rast(sds[ 8],lyrs=1) -  rast(sds[ 6],lyrs=1) # greendown_period
  # EVI metrics
  }else if(mm==13){r1 <- rast(sds[ 9],lyrs=1)                                 # EVI_Minimum
  }else if(mm==14){r1 <- rast(sds[10],lyrs=1)                                 # EVI_Amplitude
  }else if(mm==15){r1 <- rast(sds[11],lyrs=1)                                 # EVI_Area
  }else if(mm==16){r1 <- rast(sds[ 9],lyrs=1) +  rast(sds[10],lyrs=1)      # EVI_Max
  }else if(mm==17){r1 <- rast(sds[ 9],lyrs=1) +  rast(sds[10],lyrs=1)*0.15 # EVI_at 15%
  }else if(mm==18){r1 <- rast(sds[ 9],lyrs=1) +  rast(sds[10],lyrs=1)*0.50 # EVI_at 50%
  }else if(mm==19){r1 <- rast(sds[ 9],lyrs=1) +  rast(sds[10],lyrs=1)*0.90 # EVI_at 90%
  # Rates
  }else if(mm==20){
    eviamp <- rast(sds[10],lyrs=1)*0.90 - rast(sds[10],lyrs=1)*0.15
    r1 <- eviamp / (rast(sds[ 5],lyrs=1) -  rast(sds[ 2],lyrs=1)) # rate_greenup_long
  }else if(mm==21){
    eviamp <- rast(sds[10],lyrs=1)*0.50 - rast(sds[10],lyrs=1)*0.15
    r1 <- eviamp / (rast(sds[ 3],lyrs=1) -  rast(sds[ 2],lyrs=1)) # rate_greenup_early
  }else if(mm==22){
    eviamp <- rast(sds[10],lyrs=1)*0.90 - rast(sds[10],lyrs=1)*0.50
    r1 <- eviamp / (rast(sds[ 5],lyrs=1) -  rast(sds[ 3],lyrs=1)) # rate_greenup_late
  }else if(mm==23){
    eviamp <- rast(sds[10],lyrs=1)*0.90 - rast(sds[10],lyrs=1)*0.15
    r1 <- eviamp / (rast(sds[ 8],lyrs=1) -  rast(sds[ 6],lyrs=1)) # rate_greendown_long
  }else if(mm==24){
    eviamp <- rast(sds[10],lyrs=1)*0.90 - rast(sds[10],lyrs=1)*0.50
    r1 <- eviamp / (rast(sds[ 7],lyrs=1) -  rast(sds[ 6],lyrs=1)) # rate_greendown_early
  }else           {
    eviamp <- rast(sds[10],lyrs=1)*0.50 - rast(sds[10],lyrs=1)*0.15
    r1 <- eviamp / (rast(sds[ 8],lyrs=1) -  rast(sds[ 7],lyrs=1)) # rate_greendown_late  
  }
  
  # Only best or good
  r1[rQA!=0 & rQA!=1] <- NA 
  
  # Aggregate and save summation of change
  r2 <- aggregate(r1,fact=20,fun='median',na.rm=T)
  r3 <- project(r2,pr3)
  
  dat1[,yy] <- values(r3)
  print(years[yy])
}



############################################################
## Calculate change and significance
dat2 <- matrix(NA,(ncol(pr3)*nrow(pr3)),7)
for(i in 1:(ncol(pr3)*nrow(pr3))){
  d1 <- dat1[i,c(1:7)]
  d2 <- dat1[i,c(15:21)]
  d3 <- dat1[i,]
  
  if(sum(is.na(d1))<4 & sum(is.na(d2))<4 & sd(d3,na.rm=T)<=90){
    dat2[i,1] <- mean(d1)
    dat2[i,2] <- median(d1)
    
    dat2[i,3] <- mean(d2)
    dat2[i,4] <- median(d3)
    
    dat2[i,5] <- dat2[i,3] - dat2[i,1]
    dat2[i,6] <- dat2[i,4] - dat2[i,2]
    
    log <- try({
      tR <- t.test(d1,d2)
      dat2[i,7] <- tR$p.value
    },silent=T)
    if(inherits(log,'try-error')){
      dat2[i,7] <- NA
    }
    
  }else{
    dat2[i,] <- NA
  }
  
  # if(i%%5000==0) print(i)
}

############################################################  
## save
# data
outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/metrics/filt_21yrs_sig/',tile_list[tt])
if (!dir.exists(outDir)) {dir.create(outDir)}  
metric <- sprintf('%02d',mm)
save(dat1,dat2,
     file=paste0(outDir,'/metrics_',metric,'.rda'))


# raster
outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_sig/res_cor/'
if (!dir.exists(outDir)) {dir.create(outDir)}
outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_sig/res_cor/',metric)
if (!dir.exists(outDir)) {dir.create(outDir)}

mapChg <- setValues(pr3,dat2[,6])
writeRaster(mapChg,filename=paste0(outDir,'/chg_mdi_',tile_list[tt],'.tif'),overwrite=TRUE)
mapSig <- setValues(pr3,dat2[,7])
writeRaster(mapChg,filename=paste0(outDir,'/chg_sig_',tile_list[tt],'.tif'),overwrite=TRUE)


