rm(list = ls())

library(terra)
library(sf)

library(Rcpp)
library(RcppRoll)

library(zyp)
library(Kendall)

# ############################################################
# # From bash code
# args <- commandArgs()
# print(args)
# 
# tt <- as.numeric(substr(args[3],1,3))
# mm <- as.numeric(substr(args[3],4,5))
# # tt <- 81; mm <- 4
# 
# 
# ############################################################
# # Get tile list
# mcd12q2_path <- '/projectnb/modislc/projects/sat/data/mcd12q/q2/c61'
# file <- list.files(path=paste0(mcd12q2_path,'/2001'),full.names=T)
# tile_list <- substr(file,74,79)
# 
# 
# ## panel grid
# sds <- unlist(gdal_subdatasets(file[tt]))
# imgBase <- rast(sds[1])
# 
# cxx <- 20
# cyy <- 20
# panel_grid <- matrix(1:length(imgBase),dim(imgBase)[1]/cxx,dim(imgBase)[2]/cyy)
# panel_grid <- rast(panel_grid,crs=crs(imgBase),extent=ext(imgBase))
# 
# 
# ############################################################
# # Load metrics
# dat1 <- vector('list',21)
# years <- 2001:2021
# for(yy in 1:21){
#   path <- paste0(mcd12q2_path,'/',years[yy])
#   sstr <- paste0('*',tile_list[tt],'*')
#   file <- list.files(path=path,pattern=glob2rx(sstr),full.names=T)
#   sds  <- unlist(gdal_subdatasets(file))
#   
#   rQA <- rast(sds[12],lyrs=1)
#   doy_offset <- as.integer(as.Date(paste(years[yy], "-1-1", sep="")) - as.Date("1970-1-1"))
#   
#   # DOY metrics
#   if(mm==1)      {r1 <- rast(sds[ 3],lyrs=1) - doy_offset               # MidGreenup
#   }else if(mm==2){r1 <- rast(sds[ 7],lyrs=1) - doy_offset               # MidGreendown
#   }else if(mm==3){r1 <- rast(sds[ 7],lyrs=1) -  rast(sds[ 3],lyrs=1)    # GSL
#   # EVI metrics
#   }else if(mm==4){r1 <- rast(sds[ 9],lyrs=1)                            # EVI_Minimum
#   }else if(mm==5){r1 <- rast(sds[11],lyrs=1)                            # EVI_Area
#   }else if(mm==6){r1 <- rast(sds[ 9],lyrs=1) +  rast(sds[10],lyrs=1)    # EVI_Max
#   # Rates
#   }else if(mm==7){
#     eviamp <- rast(sds[10],lyrs=1)*0.90 - rast(sds[10],lyrs=1)*0.15
#     r1 <- eviamp / (rast(sds[ 5],lyrs=1) -  rast(sds[ 2],lyrs=1)) # rate_greenup_long
#   }else if(mm==8){
#     eviamp <- rast(sds[10],lyrs=1)*0.90 - rast(sds[10],lyrs=1)*0.15
#     r1 <- eviamp / (rast(sds[ 8],lyrs=1) -  rast(sds[ 6],lyrs=1)) # rate_greendown_long
#   }
#   
#   # Only best or good
#   r1[rQA!=0 & rQA!=1] <- NA
#   
#   #
#   datMat <- as.matrix(r1)
#   dat    <- vector('list',(ncol(panel_grid)*nrow(panel_grid)));  ii <- 1
#   for(i in 1:dim(panel_grid)[1]){
#     for(j in 1:dim(panel_grid)[2]){
#       dat[[ii]] <- r1[(cxx*(i-1)+1):(cxx*i),(cyy*(j-1)+1):(cyy*j)]
#       ii <- ii + 1 
#     }
#   }
#   dat1[[yy]] <- dat
#   
#   print(years[yy])
# }
# 
# ############################################################
# ## Calculate change
# # Normalize & filtering
# 
# options(warn=-1)
# 
# dat2 <- matrix(NA,(ncol(panel_grid)*nrow(panel_grid)),9)
# for(i in 1:(ncol(panel_grid)*nrow(panel_grid))){
#   p1 <- c(); p2 <- c()
#   for(j in  1: 7){p1 <- c(p1,unlist(dat1[[j]][i]))}
#   for(j in 15:21){p2 <- c(p2,unlist(dat1[[j]][i]))}
#   
#   q1 <- quantile(p1,c(0.025,0.975),na.rm=T)
#   q2 <- quantile(p2,c(0.025,0.975),na.rm=T)
#   
#   p1[p1<q1[1]|p1>q1[2]] <- NA
#   p2[p2<q2[1]|p2>q2[2]] <- NA
#   
#   p3 <- c()
#   for(j in  1:21){
#     tmp <- unlist(dat1[[j]][i])
#     q3  <- quantile(tmp,c(0.025,0.975),na.rm=T)
#     tmp[tmp<q3[1]|tmp>q3[2]] <- NA
#     
#     if(sum(!is.na(tmp))>=8){
#       p3[j] <- median(tmp,na.rm=T)  
#     }else{
#       p3[j] <- NA
#     }
#   }
#   
#   if(sum(!is.na(p1))>=280 & sum(!is.na(p2))>=280){
#     # p1 <- sample(p1,280,replace=T)
#     # p2 <- sample(p2,280,replace=T)
#     
#     dat2[i,1] <- median(p1,na.rm=T) 
#     dat2[i,2] <- median(p2,na.rm=T) 
#     dat2[i,3] <- median(p3,na.rm=T) 
#     
#     dat2[i,4] <- sd(p1,na.rm=T) 
#     dat2[i,5] <- sd(p2,na.rm=T) 
#     dat2[i,6] <- sd(p3,na.rm=T) 
#     
#     log <- try({
#       tT <- t.test(p1,p2)  
#       dat2[i,7] <- tT$p.value
#       },silent=T)
#     log <- try({
#       x <- 1:21
#       z <- zyp.sen(p3~x)
#       dat2[i,8] <- as.numeric(z$coefficients[2])
#       Mann <- MannKendall(p3)
#       dat2[i,9] <- as.numeric(Mann$sl[1])
#     },silent=T)
#     
#   }
#   
#   if(i%%3000==0) print(i)
# }
# 
# 
# ############################################################
# ## save
# # data
# outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/metrics/filt_21yrs_sig_10k/',tile_list[tt])
# if (!dir.exists(outDir)) {dir.create(outDir)}
# metric <- sprintf('%02d',mm)
# save(dat1,dat2,
#      file=paste0(outDir,'/metrics_',metric,'.rda'))
# 
# 
# ## raster
# # At 10k
# outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_sig_10k/res_cor/'
# if (!dir.exists(outDir)) {dir.create(outDir)}
# outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_sig_10k/res_cor/',metric)
# if (!dir.exists(outDir)) {dir.create(outDir)}
# 
# mapChg <- setValues(panel_grid,(dat2[,2]-dat2[,1]))
# writeRaster(mapChg,filename=paste0(outDir,'/chg_mdi_',tile_list[tt],'.tif'),overwrite=TRUE)
# mapSig <- setValues(panel_grid,dat2[,7])
# writeRaster(mapSig,filename=paste0(outDir,'/chg_sig_',tile_list[tt],'.tif'),overwrite=TRUE)
# mapSD  <- setValues(panel_grid,dat2[,6])
# writeRaster(mapSD,filename=paste0(outDir,'/chg_sd_',tile_list[tt],'.tif'),overwrite=TRUE)
# mapTs  <- setValues(panel_grid,dat2[,8])
# writeRaster(mapTs,filename=paste0(outDir,'/chg_ts_',tile_list[tt],'.tif'),overwrite=TRUE)
# mapMa  <- setValues(panel_grid,dat2[,9])
# writeRaster(mapMa,filename=paste0(outDir,'/chg_ma_',tile_list[tt],'.tif'),overwrite=TRUE)
# 
# 
# 
# # Reproject
# refReg <- vect('/projectnb/modislc/users/mkmoon/TAscience/trend/data/shp/IPCC-WGI-reference-regions-v4.shp')
# pr3       <- project(panel_grid,crs(refReg))
# res(pr3)  <- 0.1
# 
# mapChg <- project(mapChg,pr3)
# writeRaster(mapChg,filename=paste0(outDir,'/1_chg_mdi_',tile_list[tt],'.tif'),overwrite=TRUE)
# mapSig <- project(mapSig,pr3)
# writeRaster(mapSig,filename=paste0(outDir,'/1_chg_sig_',tile_list[tt],'.tif'),overwrite=TRUE)
# mapSD  <- project(mapSD,pr3)
# writeRaster(mapSD,filename=paste0(outDir,'/1_chg_sd_',tile_list[tt],'.tif'),overwrite=TRUE)
# mapTs  <- project(mapTs,pr3)
# writeRaster(mapTs,filename=paste0(outDir,'/1_chg_ts_',tile_list[tt],'.tif'),overwrite=TRUE)
# mapMa  <- project(mapMa,pr3)
# writeRaster(mapMa,filename=paste0(outDir,'/1_chg_ma_',tile_list[tt],'.tif'),overwrite=TRUE)


  
  
##########################################################
# From bash code
args <- commandArgs()
print(args)

vv <- as.numeric(args[3])
# vv <- 1

##
outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_sig_10k/res_cor/',sprintf('%02d',vv))
files  <- list.files(outDir,pattern=glob2rx('1_chg_sd*.tif'),full.names=T)

print(length(files))

rlist <- vector('list',length(files))
for(i in 1:length(files)){
  rlist[[i]]<- rast(files[i])
}
rsrc <- sprc(rlist)
rMerge <- merge(rsrc)

print(rMerge)

outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_sig/merge')
if (!dir.exists(outDir)) {dir.create(outDir)}
writeRaster(rMerge,filename=paste0(outDir,'/1_sd_merge_',sprintf('%02d',vv),'.tif'),overwrite=TRUE)



