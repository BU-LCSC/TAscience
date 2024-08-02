rm(list = ls())

library(terra)
library(sf)

library(Rcpp)
library(RcppRoll)

library(remotePARTS)


############################################################
# From bash code
args <- commandArgs()
print(args)

tt <- as.numeric(substr(args[3],1,3))
mm <- as.numeric(substr(args[3],4,5))
# tt <- 32; mm <- 3


############################################################
# Get tile list
mcd12q2_path <- '/projectnb/modislc/projects/sat/data/mcd12q/q2/c61'
file <- list.files(path=paste0(mcd12q2_path,'/2001'),full.names=T)
tile_list <- substr(file,74,79)
imgBase <- rast(unlist(gdal_subdatasets(file[tt]))[10],lyrs=1)

# Load metrics
dat1  <- matrix(NA,(2400*2400),22)
years <- 2001:2022
for(yy in 1:22){
  path <- paste0(mcd12q2_path,'/',years[yy])
  sstr <- paste0('*',tile_list[tt],'*')
  file <- list.files(path=path,pattern=glob2rx(sstr),full.names=T)
  sds  <- unlist(gdal_subdatasets(file))

  rQA <- rast(sds[12],lyrs=1)
  doy_offset <- as.integer(as.Date(paste(years[yy], "-1-1", sep="")) - as.Date("1970-1-1"))

  # DOY metrics
  if(mm== 1)      {r1 <- rast(sds[ 3],lyrs=1) - doy_offset              # MidGreenup
  }else if(mm== 2){r1 <- rast(sds[ 7],lyrs=1) - doy_offset              # MidGreendown
  }else if(mm== 3){r1 <- rast(sds[ 7],lyrs=1) -  rast(sds[ 3],lyrs=1)   # gsl
  # EVI metrics
  }else if(mm==4){r1 <- rast(sds[ 9],lyrs=1) +  rast(sds[10],lyrs=1)    # EVI_Max
  }else if(mm==5){r1 <- rast(sds[10],lyrs=1)                            # EVI_Amplitude
  }else if(mm==6){r1 <- rast(sds[11],lyrs=1)                            # EVI_Area
  
  }

  # Only best or good
  r1[rQA!=0 & rQA!=1] <- NA
  
  dat1[,yy] <- values(r1)
  print(years[yy])
}

# save
outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/re/metrics/',tile_list[tt])
if (!dir.exists(outDir)) {dir.create(outDir)}
metric <- sprintf('%02d',mm)
save(dat1,file=paste0(outDir,'/metrics_',metric,'.rda'))

gc()


############################################################
# LCT
mcd12q1_path <- '/projectnb/modislc/projects/sat/data/mcd12q/q1/'
mat_lct <- matrix(NA,(2400*2400),22)
for(i in 1:22){
  path <- paste0(mcd12q1_path,(i+2000),'.01.01',sep='')
  file <- list.files(path,pattern = glob2rx(paste0('*',tile_list[tt],'*')),full.names=T)
  sds  <- unlist(gdal_subdatasets(file))
  lct  <- rast(sds[1])
  mat_lct[,i] <- values(lct)
}
lct_ch     <- matrix(NA,(2400*2400),2)
lct_ch[,2] <- mat_lct[,22]
for(i in 1:(2400*2400)){
  tmp <- mat_lct[i,]
  if(length(unique(tmp))==1){
    lct_ch[i,1] <- tmp[1]
  }
}
rm(mat_lct)

# AR regression
cCase <- complete.cases(dat1)
cCase <- c(1:(2400*2400))[cCase]
print(length(cCase))

mStat <- matrix(NA,(2400*2400),8+22); xx <- 1:22; j <- 1
for(i in cCase){
  yy <- dat1[i,]
  ll <- lct_ch[i,1]
  
  if(!is.na(ll)){
    ARfit <- fitAR(yy ~ xx)
    mStat[i,1] <- ARfit$coefficients[1]
    mStat[i,2] <- ARfit$coefficients[2]
    mStat[i,3] <- ARfit$pval[2]
    mStat[i,4] <- ARfit$coefficients[2]/mean(yy)
    mStat[i,5] <- mean(yy)
    mStat[i,6] <- median(yy)
    mStat[i,7] <- sd(yy)
    mStat[i,8] <- ll
    mStat[i,xx+8] <- ARfit$residuals
  }
  j <- j + 1 
  if(j%%100000==0) print(j)
}

rm(dat1,lct_ch)
gc()


############################################################
# IPCC reference regions
refReg <- vect('/projectnb/modislc/users/mkmoon/TAscience/trend/data/shp/IPCC-WGI-reference-regions-v4.shp')
pr1       <- rast(crs=crs(imgBase),extent=ext(imgBase),res=res(imgBase))
pr2       <- aggregate(pr1,fact=22)
pr3       <- project(pr2,crs(refReg))
res(pr3)  <- 0.1

ecoR <- project(refReg,imgBase)
ecoR <- crop(ecoR,imgBase)
ee   <- rasterize(ecoR,imgBase,'Acronym')
e1   <- as.data.frame(ee)
e1   <- levels(e1$Acronym)
e2   <- values(ee)

# coordinates
coords  <- crds(imgBase,na.rm=F)
shp <- vect(coords,crs=crs(imgBase))
shp <- project(shp,'EPSG:4326')
coords <- crds(shp)


############################################################
# Save
outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/re/vals/'
if (!dir.exists(outDir)) {dir.create(outDir)}
outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/re/vals/',metric)
if (!dir.exists(outDir)) {dir.create(outDir)}
save(mStat,coords,e1,e2,
     file=paste0(outDir,'/coef_coords_lc_',tile_list[tt],'.rda'))


############################################################
## raster
# Original
outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/re/rasters/res_org/'
if (!dir.exists(outDir)) {dir.create(outDir)}
outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/re/rasters/res_org/',metric)
if (!dir.exists(outDir)) {dir.create(outDir)}

mapMid  <- setValues(imgBase,mStat[,6])
writeRaster(mapMid,filename=paste0(outDir,'/chg_mdi_',tile_list[tt],'.tif'),overwrite=TRUE)
mapTrd  <- setValues(imgBase,mStat[,2])
writeRaster(mapTrd,filename=paste0(outDir,'/chg_trd_',tile_list[tt],'.tif'),overwrite=TRUE)
mapSig  <- setValues(imgBase,mStat[,3])
writeRaster(mapSig,filename=paste0(outDir,'/chg_sig_',tile_list[tt],'.tif'),overwrite=TRUE)
mapSD   <- setValues(imgBase,mStat[,7])
writeRaster(mapSD,filename=paste0(outDir,'/chg_sd_',tile_list[tt],'.tif'),overwrite=TRUE)

# At 10k
outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/re/rasters/res_cor/'
if (!dir.exists(outDir)) {dir.create(outDir)}
outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/re/rasters/res_cor/',metric)
if (!dir.exists(outDir)) {dir.create(outDir)}

mapMid <- aggregate(mapMid,fact=21,fun='median',na.rm=T)
writeRaster(mapMid,filename=paste0(outDir,'/chg_mdi_',tile_list[tt],'.tif'),overwrite=TRUE)
mapTrd <- aggregate(mapTrd,fact=21,fun='median',na.rm=T)
writeRaster(mapTrd,filename=paste0(outDir,'/chg_trd_',tile_list[tt],'.tif'),overwrite=TRUE)
mapSig <- aggregate(mapSig,fact=21,fun='median',na.rm=T)
writeRaster(mapSig,filename=paste0(outDir,'/chg_sig_',tile_list[tt],'.tif'),overwrite=TRUE)
mapSD  <- aggregate(mapSD,fact=21,fun='median',na.rm=T)
writeRaster(mapSD,filename=paste0(outDir,'/chg_sd_',tile_list[tt],'.tif'),overwrite=TRUE)

mapMid <- project(mapMid,pr3)
writeRaster(mapMid,filename=paste0(outDir,'/1_chg_mdi_',tile_list[tt],'.tif'),overwrite=TRUE)
mapTrd <- project(mapTrd,pr3)
writeRaster(mapTrd,filename=paste0(outDir,'/1_chg_trd_',tile_list[tt],'.tif'),overwrite=TRUE)
mapSig <- project(mapSig,pr3)
writeRaster(mapSig,filename=paste0(outDir,'/1_chg_sig_',tile_list[tt],'.tif'),overwrite=TRUE)
mapSD  <- project(mapSD,pr3)
writeRaster(mapSD,filename=paste0(outDir,'/1_chg_sd_',tile_list[tt],'.tif'),overwrite=TRUE)

