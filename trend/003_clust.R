rm(list = ls())

library(sp)
library(raster)
library(terra)
library(sf)

library(doMC)
library(doParallel)

library(mclust)


############################################################
# From bash code
args <- commandArgs()
print(args)

tt <- as.numeric(args[3])
# tt <- 22

# ############################################################
# path <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/nh_dc/vals'
# file <- list.files(path=path,pattern=glob2rx('*_validrow.rda'),full.names=T)
# 
# for(i in 1:length(file)){
#   load(file[i])
# 
#   if(i==1){
#     valROW <- valRow
#   }else{
#     valROW <- rbind(valROW,valRow)
#   }
# 
#   print(i)
# }
# valRow <- valROW
# path <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/nh_dc'
# save(valRow,file=paste0(path,'/valid_pixels.rda'))


# ####
# path <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/nh_dc'
# load(paste0(path,'/valid_pixels.rda'))
# 
# set.seed(456789)
# ranNum <- sample(1:dim(valRow)[1],round(dim(valRow)[1])*0.02)
# ranNum <- ranNum[order(ranNum)]
# valRow <- valRow[ranNum,]
# tiles <- unique(valRow[,1])
# 
# files <- list.files(paste0(path,'/vals'),pattern=glob2rx('*.rda'),full.names=T)
# files <- files[seq(4,length(files),4)]
# 
# ##
# registerDoMC()
# 
# dat <- foreach(i=tiles, .combine=rbind) %dopar% {
# # dat <- c()
# # for(i in tiles){
# 
#   id <- which(valRow[,1]==i)
# 
#   load(files[i])
# 
#   if(i==4 & length(id)>0){
#     dat <- datOrg[valRow[id,2],]
#     # dat <- datNor[valRow[id,2],]
#   }else if(length(id)>0){
#     # tmp <- datOrg[valRow[id,2],]
#     # dat <- rbind(dat,tmp)
#     dat <- datOrg[valRow[id,2],]
#     # dat <- datNor[valRow[id,2],]
#   }
# 
#   # print(i)
# }
# save(dat,file=paste0(path,'/valid_pixels_sampled_2pc_org.rda'))



# ############################################################
# path <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/nh_dc'
# load(paste0(path,'/valid_pixels_sampled_2pc_org.rda'))
#
#
# ##
# pcp     <- prcomp(dat,center = T,scale. = T)
# pcpDat <- cbind(as.numeric(pcp$x[,1]),as.numeric(pcp$x[,2]),as.numeric(pcp$x[,3]),as.numeric(pcp$x[,4]),as.numeric(pcp$x[,5]))
#
# var_explained <- pcp$sdev^2 / sum(pcp$sdev^2)
# plot(round(var_explained*100,1))
# sum(var_explained[1:5])
# plot(pcp$rotation[,1])
# save(pcp,pcpDat,file=paste0(path,'/val_pcp_2pc_org.rda'))

# ##
# path <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/nh_dc'
# load(paste0(path,'/val_pcp_2pc_org.rda'))
# 
# X   <- pcpDat
# BIC <- mclustBIC(X)
# # plot(BIC)
# # summary(BIC)
# save(BIC,file=paste0(path,'/val_mclust_bic_2pc_org.rda'))
# 
# mod1 <- Mclust(X, x = BIC)
# # summary(mod1, parameters = TRUE)
# save(mod1,file=paste0(path,'/val_mclust_mod1_2pc_org.rda'))



############################################################
path <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/nh_dc'

load(paste0(path,'/valid_pixels.rda'))
load(paste0(path,'/val_pcp_2pc_org.rda'))
load(paste0(path,'/val_mclust_mod1_2pc_org.rda'))
load(paste0(path,'/val_mclust_bic_2pc_org.rda'))

# ## Eigenvectors
# setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/figures/')
# png(filename='eigen_forest_nlc.png',width=12,height=6,units='in',res=300)
# barplot(pcp$rotation[,1],ylim=c(-0.35,0.35),
#         names=c(1:25),cex.axis=1.3)
# abline(h=0);box()
# dev.off()

##
files <- list.files(path=paste0(path,'/vals'),pattern=glob2rx('*.rda'),full.names=T)
files <- files[seq(4,length(files),4)]

p1 <- which(valRow[,1]==tt)
p2 <- valRow[p1,2]

print(length(p2))

if(length(p2)>1){
  load(files[tt])

  dat <- as.data.frame(datOrg[p2,])

  prPC <- predict(pcp,dat)
  pcpDat <- prPC[,c(1:5)]

  prMC <- predict(mod1,pcpDat)

  #
  dat <- matrix(NA,(2400*2400),6)
  dat[p2,1]      <- prMC$classification
  dat[p2,c(2:6)] <- pcpDat
  colnames(dat) <- c('class','PC1','PC2','PC3','PC4','PC5')
}else{
  #
  dat <- matrix(NA,(2400*2400),6)
  colnames(dat) <- c('class','PC1','PC2','PC3','PC4','PC5')
}


##
mcd12q2_path <- '/projectnb/modislc/projects/sat/data/mcd12q/q2/c61'
file <- list.files(path=paste0(mcd12q2_path,'/2001'),full.names=T)
tile_list <- substr(file,74,79)

# Northern hemisphere tiles
file <- file[which(as.numeric(substr(tile_list,5,6))<6)]
tile_list <- tile_list[which(as.numeric(substr(tile_list,5,6))<6)]

lct <- raster(unlist(gdal_subdatasets(file[tt]))[10])

mapClust <- setValues(lct,dat[,1])
mapPC1   <- setValues(lct,dat[,2])
mapPC2   <- setValues(lct,dat[,3])
mapPC3   <- setValues(lct,dat[,4])
mapPC4   <- setValues(lct,dat[,5])
mapPC5   <- setValues(lct,dat[,6])


#
outDir <- paste0(path,'/clust_2pc/vals')
if (!dir.exists(outDir)) {dir.create(outDir)}
save(dat,file=paste0(outDir,'/val_clust_',tile_list[tt],'.rda'))

outDir <- paste0(path,'/clust_2pc/map_clust')
if (!dir.exists(outDir)) {dir.create(outDir)}
writeRaster(mapClust,filename=paste0(outDir,'/map_clust_',tile_list[tt],'.tif'), format="GTiff", overwrite=TRUE)
outDir <- paste0(path,'/clust_2pc/map_pc1')
if (!dir.exists(outDir)) {dir.create(outDir)}
writeRaster(mapPC1,filename=paste0(outDir,'/map_pc1_',tile_list[tt],'.tif'), format="GTiff", overwrite=TRUE)
outDir <- paste0(path,'/clust_2pc/map_pc2')
if (!dir.exists(outDir)) {dir.create(outDir)}
writeRaster(mapPC2,filename=paste0(outDir,'/map_pc2_',tile_list[tt],'.tif'), format="GTiff", overwrite=TRUE)
outDir <- paste0(path,'/clust_2pc/map_pc3')
if (!dir.exists(outDir)) {dir.create(outDir)}
writeRaster(mapPC3,filename=paste0(outDir,'/map_pc3_',tile_list[tt],'.tif'), format="GTiff", overwrite=TRUE)
outDir <- paste0(path,'/clust_2pc/map_pc4')
if (!dir.exists(outDir)) {dir.create(outDir)}
writeRaster(mapPC4,filename=paste0(outDir,'/map_pc4_',tile_list[tt],'.tif'), format="GTiff", overwrite=TRUE)
outDir <- paste0(path,'/clust_2pc/map_pc5')
if (!dir.exists(outDir)) {dir.create(outDir)}
writeRaster(mapPC5,filename=paste0(outDir,'/map_pc5_',tile_list[tt],'.tif'), format="GTiff", overwrite=TRUE)



############################################################
# Reproject
pr3 <- projectExtent(lct,crs(lct))
res(pr3) <- 10000
r1 <- resample(mapClust,pr3,method='ngb')
r2 <- resample(mapPC1,pr3)
r3 <- resample(mapPC2,pr3)
r4 <- resample(mapPC3,pr3)
r5 <- resample(mapPC4,pr3)
r6 <- resample(mapPC5,pr3)


robin_crs = CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
pr3 <- projectExtent(lct,robin_crs)
res(pr3) <- 10000
rast1 <- projectRaster(r1,pr3,method='ngb')
rast2 <- projectRaster(r2,pr3)
rast3 <- projectRaster(r3,pr3)
rast4 <- projectRaster(r4,pr3)
rast5 <- projectRaster(r5,pr3)
rast6 <- projectRaster(r6,pr3)
# rast7 <- projectRaster(r7,pr3)

outDir <- paste0(path,'/clust_2pc/map_coarse')
if (!dir.exists(outDir)) {dir.create(outDir)}
outDir <- paste0(path,'/clust_2pc/map_coarse/map_clust')
if (!dir.exists(outDir)) {dir.create(outDir)}
writeRaster(rast1,filename=paste0(outDir,'/1_map_clust_',tile_list[tt],'.tif'), format="GTiff", overwrite=TRUE)
outDir <- paste0(path,'/clust_2pc/map_coarse/map_pc1')
if (!dir.exists(outDir)) {dir.create(outDir)}
writeRaster(rast2,filename=paste0(outDir,'/1_map_pc1_',tile_list[tt],'.tif'), format="GTiff", overwrite=TRUE)
outDir <- paste0(path,'/clust_2pc/map_coarse/map_pc2')
if (!dir.exists(outDir)) {dir.create(outDir)}
writeRaster(rast3,filename=paste0(outDir,'/1_map_pc2_',tile_list[tt],'.tif'), format="GTiff", overwrite=TRUE)
outDir <- paste0(path,'/clust_2pc/map_coarse/map_pc3')
if (!dir.exists(outDir)) {dir.create(outDir)}
writeRaster(rast4,filename=paste0(outDir,'/1_map_pc3_',tile_list[tt],'.tif'), format="GTiff", overwrite=TRUE)
outDir <- paste0(path,'/clust_2pc/map_coarse/map_pc4')
if (!dir.exists(outDir)) {dir.create(outDir)}
writeRaster(rast5,filename=paste0(outDir,'/1_map_pc4_',tile_list[tt],'.tif'), format="GTiff", overwrite=TRUE)
outDir <- paste0(path,'/clust_2pc/map_coarse/map_pc5')
if (!dir.exists(outDir)) {dir.create(outDir)}
writeRaster(rast6,filename=paste0(outDir,'/1_map_pc5_',tile_list[tt],'.tif'), format="GTiff", overwrite=TRUE)



