rm(list = ls())

library(terra)
library(sf)

library(Rcpp)
library(RcppRoll)


############################################################
# From bash code
args <- commandArgs()
print(args)

vv <- as.numeric(args[3])
# vv <- 1

##############################
##
outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/re/rasters/res_cor/',sprintf('%02d',vv))
files  <- list.files(outDir,pattern=glob2rx('chg_trd*.tif'),full.names=T)

rlist <- c()
for(i in 1:length(files)){
  tmp <- values(rast(files[i]))
  tmp <- na.omit(tmp)
  rlist <- c(rlist,tmp)
}
ds <- density(rlist,n=10000)

outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/re/rasters/merge')
if (!dir.exists(outDir)) {dir.create(outDir)}
save(ds,file=paste0(outDir,'/merge_dnt_trd_',sprintf('%02d',vv),'.rda'))


##
outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/re/rasters/res_cor/',sprintf('%02d',vv))
files  <- list.files(outDir,pattern=glob2rx('1_chg_trd*.tif'),full.names=T)

rlist <- vector('list',length(files))
for(i in 1:length(files)){
  rlist[[i]]<- rast(files[i])
}
rsrc <- sprc(rlist)
rMerge <- merge(rsrc)

outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/re/rasters/merge')
writeRaster(rMerge,filename=paste0(outDir,'/1_merge_trd_',sprintf('%02d',vv),'.tif'),overwrite=TRUE)


##############################
##
outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/re/rasters/res_cor/',sprintf('%02d',vv))
files  <- list.files(outDir,pattern=glob2rx('chg_sd*.tif'),full.names=T)

rlist <- c()
for(i in 1:length(files)){
  tmp <- values(rast(files[i]))
  tmp <- na.omit(tmp)
  rlist <- c(rlist,tmp)
}
ds <- density(rlist,n=10000)

outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/re/rasters/merge')
save(ds,file=paste0(outDir,'/merge_dnt_sd_',sprintf('%02d',vv),'.rda'))

##
outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/re/rasters/res_cor/',sprintf('%02d',vv))
files  <- list.files(outDir,pattern=glob2rx('1_chg_sd*.tif'),full.names=T)

rlist <- vector('list',length(files))
for(i in 1:length(files)){
  rlist[[i]]<- rast(files[i])
}
rsrc <- sprc(rlist)
rMerge <- merge(rsrc)

outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/re/rasters/merge')
writeRaster(rMerge,filename=paste0(outDir,'/1_merge_sd_',sprintf('%02d',vv),'.tif'),overwrite=TRUE)


##############################
##
outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/re/rasters/res_cor/',sprintf('%02d',vv))
files  <- list.files(outDir,pattern=glob2rx('chg_sig*.tif'),full.names=T)

rlist <- c()
for(i in 1:length(files)){
  tmp <- values(rast(files[i]))
  tmp <- na.omit(tmp)
  rlist <- c(rlist,tmp)
}
ds <- density(rlist,n=10000)

outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/re/rasters/merge')
save(ds,file=paste0(outDir,'/merge_dnt_sig_',sprintf('%02d',vv),'.rda'))

##
outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/re/rasters/res_cor/',sprintf('%02d',vv))
files  <- list.files(outDir,pattern=glob2rx('1_chg_sig*.tif'),full.names=T)

rlist <- vector('list',length(files))
for(i in 1:length(files)){
  rlist[[i]]<- rast(files[i])
}
rsrc <- sprc(rlist)
rMerge <- merge(rsrc)

outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/re/rasters/merge')
writeRaster(rMerge,filename=paste0(outDir,'/1_merge_sig_',sprintf('%02d',vv),'.tif'),overwrite=TRUE)
