rm(list = ls())

library(terra)
library(sf)

library(Rcpp)
library(RcppRoll)

# library(doMC)
# library(doParallel)

library(remotePARTS)


# ############################################################
# # From bash code
# args <- commandArgs()
# print(args)
# 
# tt <- as.numeric(substr(args[3],1,3))
# mm <- as.numeric(substr(args[3],4,6))
# # tt <- 67; mm <- 15
# 
# 
# ############################################################
# # Get tile list
# mcd12q2_path <- '/projectnb/modislc/projects/sat/data/mcd12q/q2/c61'
# file <- list.files(path=paste0(mcd12q2_path,'/2001'),full.names=T)
# tile_list <- substr(file,74,79)
# 
# imgBase <- rast(unlist(gdal_subdatasets(file[tt]))[10],lyrs=1)
# 
# refReg <- vect('/projectnb/modislc/users/mkmoon/TAscience/trend/data/shp/IPCC-WGI-reference-regions-v4.shp')
# pr1       <- rast(crs=crs(imgBase),extent=ext(imgBase),res=res(imgBase))
# pr2       <- aggregate(pr1,fact=21)
# pr3       <- project(pr2,crs(refReg))
# res(pr3)  <- 0.1
# 
# if(mm==2){
#   # LCT
#   ## Get MCD12Q1
#   mcd12q1_path <- '/projectnb/modislc/projects/sat/data/mcd12q/q1/'
#   mat_lct <- matrix(NA,(2400*2400),21)
#   for(i in 1:21){
#     path <- paste0(mcd12q1_path,(i+2000),'.01.01',sep='')
#     file <- list.files(path,pattern = glob2rx(paste0('*',tile_list[tt],'*')),full.names=T)
#     sds  <- unlist(gdal_subdatasets(file))
#     lct  <- rast(sds[1])
#     mat_lct[,i] <- values(lct)
#   }
#   lct_ch     <- matrix(NA,(2400*2400),2)
#   lct_ch[,2] <- mat_lct[,21]
#   for(i in 1:(2400*2400)){
#     tmp <- mat_lct[i,]
#     if(length(unique(tmp))==1){
#       lct_ch[i,1] <- tmp[1]
#     }
#   }
#   rm(mat_lct)
# }
# 
# 
# # Ecoregion
# ecoR <- project(refReg,imgBase)
# ecoR <- crop(ecoR,imgBase)
# ee   <- rasterize(ecoR,imgBase,'Acronym')
# e1   <- as.data.frame(ee)
# e1   <- levels(e1$Acronym)
# e2   <- values(ee)
# 
# 
# ############################################################
# # data
# outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/metrics/filt_21yrs_sig/',tile_list[tt])
# metric <- sprintf('%02d',mm)
# load(paste0(outDir,'/metrics_',metric,'.rda'))
# 
# cCase <- complete.cases(dat1)
# cCase <- c(1:(2400*2400))[cCase]
# 
# print(length(cCase))
# 
# ##
# mStat <- matrix(NA,(2400*2400),7+21)
# xx <- 1:21
# 
# for(i in cCase){
# # registerDoMC(6)
# # foreach(i=cCase[1:1000]) %dopar%{
#     
#   yy <- dat1[i,]
# 
#   # if(sum(!is.na(yy))==21){
#     ARfit <- fitAR(yy ~ xx)
# 
#     mStat[i,1] <- ARfit$coefficients[1]
#     mStat[i,2] <- ARfit$coefficients[2]
#     mStat[i,3] <- ARfit$pval[2]
#     mStat[i,4] <- ARfit$coefficients[2]/mean(yy)
#     mStat[i,5] <- mean(yy)
#     mStat[i,6] <- median(yy)
#     mStat[i,7] <- sd(yy)
#     mStat[i,xx+7] <- ARfit$residuals
# 
#   # }else{
#   #   tmp <- rep(NA,28)
#   # }
#   if(i%%100000==0) print(i)
# }
# 
# ##
# if(mm==2){
#   coords  <- crds(imgBase,na.rm=F)
#   shp <- vect(coords,crs=crs(imgBase))
#   shp <- project(shp,'EPSG:4326')
#   coords <- crds(shp)
# }
# 
# #
# print(dim(mStat))
# 
# #
# outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/parts/vals/'
# if (!dir.exists(outDir)) {dir.create(outDir)}
# outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/parts/vals/',metric)
# if (!dir.exists(outDir)) {dir.create(outDir)}
# if(mm==2){
#   save(mStat,coords,lct_ch,e1,e2,
#        file=paste0(outDir,'/coef_coords_lc_',tile_list[tt],'.rda'))
# }else{
#   save(mStat,e1,e2,
#        file=paste0(outDir,'/coef_coords_lc_',tile_list[tt],'.rda'))
# }
# 
# 
# ############################################################
# ## raster
# # Original
# outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/parts/res_org/'
# if (!dir.exists(outDir)) {dir.create(outDir)}
# outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/parts/res_org/',metric)
# if (!dir.exists(outDir)) {dir.create(outDir)}
# 
# mapMid  <- setValues(imgBase,mStat[,6])
# writeRaster(mapMid,filename=paste0(outDir,'/chg_mdi_',tile_list[tt],'.tif'),overwrite=TRUE)
# mapTrd  <- setValues(imgBase,mStat[,2])
# writeRaster(mapTrd,filename=paste0(outDir,'/chg_trd_',tile_list[tt],'.tif'),overwrite=TRUE)
# mapSig  <- setValues(imgBase,mStat[,3])
# writeRaster(mapSig,filename=paste0(outDir,'/chg_sig_',tile_list[tt],'.tif'),overwrite=TRUE)
# mapSD   <- setValues(imgBase,mStat[,7])
# writeRaster(mapSD,filename=paste0(outDir,'/chg_sd_',tile_list[tt],'.tif'),overwrite=TRUE)
# 
# 
# # At 10k
# outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/parts/res_cor/'
# if (!dir.exists(outDir)) {dir.create(outDir)}
# outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/parts/res_cor/',metric)
# if (!dir.exists(outDir)) {dir.create(outDir)}
# 
# mapMid <- aggregate(mapMid,fact=21,fun='median',na.rm=T)
# writeRaster(mapMid,filename=paste0(outDir,'/chg_mdi_',tile_list[tt],'.tif'),overwrite=TRUE)
# mapTrd <- aggregate(mapTrd,fact=21,fun='median',na.rm=T)
# writeRaster(mapTrd,filename=paste0(outDir,'/chg_trd_',tile_list[tt],'.tif'),overwrite=TRUE)
# mapSig <- aggregate(mapSig,fact=21,fun='median',na.rm=T)
# writeRaster(mapSig,filename=paste0(outDir,'/chg_sig_',tile_list[tt],'.tif'),overwrite=TRUE)
# mapSD  <- aggregate(mapSD,fact=21,fun='median',na.rm=T)
# writeRaster(mapSD,filename=paste0(outDir,'/chg_sd_',tile_list[tt],'.tif'),overwrite=TRUE)
# 
# mapMid <- project(mapMid,pr3)
# writeRaster(mapMid,filename=paste0(outDir,'/1_chg_mdi_',tile_list[tt],'.tif'),overwrite=TRUE)
# mapTrd <- project(mapTrd,pr3)
# writeRaster(mapTrd,filename=paste0(outDir,'/1_chg_trd_',tile_list[tt],'.tif'),overwrite=TRUE)
# mapSig <- project(mapSig,pr3)
# writeRaster(mapSig,filename=paste0(outDir,'/1_chg_sig_',tile_list[tt],'.tif'),overwrite=TRUE)
# mapSD  <- project(mapSD,pr3)
# writeRaster(mapSD,filename=paste0(outDir,'/1_chg_sd_',tile_list[tt],'.tif'),overwrite=TRUE)


############################################################
# From bash code
args <- commandArgs()
print(args)

vv <- as.numeric(args[3])
# vv <- 14

##############################
##
outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/parts/res_cor/',sprintf('%02d',vv))
files  <- list.files(outDir,pattern=glob2rx('chg_trd*.tif'),full.names=T)

print(length(files))

rlist <- vector('list',length(files))
for(i in 1:length(files)){
  rlist[[i]]<- rast(files[i])
}
rsrc <- sprc(rlist)
rMerge <- merge(rsrc)

print(rMerge)

ds <- density(na.omit(values(rMerge)))

outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/parts/merge')
if (!dir.exists(outDir)) {dir.create(outDir)}
writeRaster(rMerge,filename=paste0(outDir,'/merge_',sprintf('%02d',vv),'.tif'),overwrite=TRUE)
save(ds,file=paste0(outDir,'/merge_dnt_trd_',sprintf('%02d',vv),'.rda'))


##
outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/parts/res_cor/',sprintf('%02d',vv))
files  <- list.files(outDir,pattern=glob2rx('1_chg_trd*.tif'),full.names=T)

print(length(files))

rlist <- vector('list',length(files))
for(i in 1:length(files)){
  rlist[[i]]<- rast(files[i])
}
rsrc <- sprc(rlist)
rMerge <- merge(rsrc)

print(rMerge)

outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/parts/merge')
if (!dir.exists(outDir)) {dir.create(outDir)}
writeRaster(rMerge,filename=paste0(outDir,'/1_merge_trd_',sprintf('%02d',vv),'.tif'),overwrite=TRUE)


##############################
##
outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/parts/res_cor/',sprintf('%02d',vv))
files  <- list.files(outDir,pattern=glob2rx('chg_sd*.tif'),full.names=T)

print(length(files))

rlist <- vector('list',length(files))
for(i in 1:length(files)){
  rlist[[i]]<- rast(files[i])
}
rsrc <- sprc(rlist)
rMerge <- merge(rsrc)

print(rMerge)

ds <- density(na.omit(values(rMerge)))

outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/parts/merge')
if (!dir.exists(outDir)) {dir.create(outDir)}
writeRaster(rMerge,filename=paste0(outDir,'/merge_',sprintf('%02d',vv),'.tif'),overwrite=TRUE)
save(ds,file=paste0(outDir,'/merge_dnt_sd_',sprintf('%02d',vv),'.rda'))

##
outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/parts/res_cor/',sprintf('%02d',vv))
files  <- list.files(outDir,pattern=glob2rx('1_chg_sd*.tif'),full.names=T)

print(length(files))

rlist <- vector('list',length(files))
for(i in 1:length(files)){
  rlist[[i]]<- rast(files[i])
}
rsrc <- sprc(rlist)
rMerge <- merge(rsrc)

print(rMerge)

outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/parts/merge')
if (!dir.exists(outDir)) {dir.create(outDir)}
writeRaster(rMerge,filename=paste0(outDir,'/1_merge_sd_',sprintf('%02d',vv),'.tif'),overwrite=TRUE)


##############################
##
outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/parts/res_cor/',sprintf('%02d',vv))
files  <- list.files(outDir,pattern=glob2rx('1_chg_sig*.tif'),full.names=T)

print(length(files))

rlist <- vector('list',length(files))
for(i in 1:length(files)){
  rlist[[i]]<- rast(files[i])
}
rsrc <- sprc(rlist)
rMerge <- merge(rsrc)

print(rMerge)

outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/parts/merge')
if (!dir.exists(outDir)) {dir.create(outDir)}
writeRaster(rMerge,filename=paste0(outDir,'/1_merge_sig_',sprintf('%02d',vv),'.tif'),overwrite=TRUE)
