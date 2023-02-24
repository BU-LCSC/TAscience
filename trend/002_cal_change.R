rm(list = ls())

library(terra)
library(sf)

library(RColorBrewer)

############################################################
# From bash code
args <- commandArgs()
print(args)

tt <- as.numeric(substr(args[3],1,3))
# tt <- 69


# ############################################################
# # Get tile list
# mcd12q2_path <- '/projectnb/modislc/projects/sat/data/mcd12q/q2/c61'
# file <- list.files(path=paste0(mcd12q2_path,'/2001'),full.names=T)
# tile_list <- substr(file,74,79)
# 
# # # Northern hemisphere tiles
# # file <- file[which(as.numeric(substr(tile_list,5,6))<6)]
# # tile_list <- tile_list[which(as.numeric(substr(tile_list,5,6))<6)]
# 
# # # North America tiles
# # tile_list <- c('h10v02', 'h10v03', 'h10v04', 'h10v05', 'h10v06', 'h10v07', 'h10v08', 
# #                'h11v02', 'h11v03', 'h11v04', 'h11v05', 'h11v06', 'h11v07', 
# #                'h12v01', 'h12v02', 'h12v03', 'h12v04', 'h12v05', 'h12v07', 
# #                'h13v01', 'h13v02', 'h13v03', 'h13v04', 
# #                'h14v01', 'h14v02', 'h14v03', 'h14v04', 
# #                'h15v01', 'h15v02', 'h15v03', 'h16v00', 'h16v01', 'h16v02', 
# #                'h17v00', 'h17v01', 'h17v02', 'h28v03', 'h29v03', 
# #                'h06v03', 'h07v03', 'h07v05', 'h07v06', 'h07v07', 
# #                'h08v03', 'h08v04', 'h08v05', 'h08v06', 'h08v07', 
# #                'h09v02', 'h09v03', 'h09v04', 'h09v05', 'h09v06', 'h09v07', 'h09v08')
# 
# 
# imgBase <- rast(unlist(gdal_subdatasets(file[tt]))[10],lyrs=1)
# 
# 
# ############################################################
# # ## Get MCD12Q1
# # mat_lct <- matrix(NA,(2400*2400),21)
# # for(i in 1:21){
# #   mcd12q1_path <- paste('/projectnb/modislc/projects/sat/data/mcd12q/q1/',(i+2000),'.01.01',sep='')
# #   file <- list.files(path=mcd12q1_path,pattern=glob2rx(paste0('*',tile_list[tt],'*')),full.names=T)
# #   sds <- unlist(gdal_subdatasets(file))
# #   lct <- rast(sds[1])
# #   mat_lct[,i] <- values(lct)
# #   
# #   print(i)
# # }
# # lct_val <- mat_lct[,21]
# # 
# # lct_ch <- matrix(NA,(2400*2400),1)
# # for(i in 1:(2400*2400)){
# #   temp <- mat_lct[i,]
# #   if(length(unique(temp))==1){
# #     lct_ch[i] <- 1
# #   }else{
# #     lct_ch[i] <- 0
# #   }
# # }
# # lct_ch <- setValues(lct,lct_ch)
# # rm(mat_lct)
# 
# # Deciduous Tree cover: IGBP 3,4,5,8,9
# # Shrub               : IGBP 6,7,10
# # slct <- which(values(lct)!=3 & values(lct)!=4 & values(lct)!=5 & values(lct)!=6 & values(lct)!=7 & values(lct)!=8 & values(lct)!=9 & values(lct)!=10) # vegetation 
# # slct <- which(values(lct)!=3 & values(lct)!=4 & values(lct)!=5 & values(lct)!=8 & values(lct)!=9) # DTC
# 
# 
# ############################################################
# path <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/metrics/filt_21yrs_nor/',tile_list[tt])
# file <- list.files(path=path,pattern=glob2rx('*'),full.names=T)
# 
# #
# datOrg <- matrix(NA,2400*2400,25)
# # datNor <- matrix(NA,2400*2400,25)
# 
# for(i in 1:25){                           
#   load(file[i])                            
#   
#   datOrg[,i] <- dat2OrgMedi - dat1OrgMedi
#   # datNor[,i] <- dat2NorMedi - dat1NorMedi   
#   
#   print(i)
# }
# 
# ## Only include pixels having all 25 variables 
# # and Forested pixels
# tmp1   <- data.frame(1:(2400*2400),datOrg)
# caseCP <- tmp1[complete.cases(tmp1),1]
# caseNC <- c(1:(2400*2400))
# caseNC <- caseNC[-caseCP]
# 
# datOrg[caseNC,] <- NA
# # datNor[caseNC,] <- NA
# # datOrg[slct,] <- NA
# # datNor[slct,] <- NA
# # datOrg[values(lct_ch)==0,] <- NA
# # datNor[values(lct_ch)==0,] <- NA
# 
# valRow <- cbind(rep(tt,sum(!is.na(datOrg[,1]))),which(!is.na(datOrg[,1]))) # for save
# 
# # ##
# # datNorAb <- abs(datNor)
# # datNorAbSum  <- apply(datNorAb,1,sum,na.rm=T)
# # mapChgNor3  <- setValues(imgBase,datNorAbSum)
# 
# 
# ############################################################
# # save original values
# outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_nor_qc/vals'
# if (!dir.exists(outDir)) {dir.create(outDir)}
# save(datOrg  ,file=paste0(outDir,'/vals_',tile_list[tt],'.rda'))
# # save(datNor  ,file=paste0(outDir,'/vals_',tile_list[tt],'_nor.rda'))
# # save(datNorAb,file=paste0(outDir,'/vals_',tile_list[tt],'_nor_ab.rda'))
# save(valRow  ,file=paste0(outDir,'/vals_',tile_list[tt],'_validrow.rda'))
# 
# 
# # Reproject and save summation of change
# refReg <- vect('/projectnb/modislc/users/mkmoon/TAscience/trend/data/shp/IPCC-WGI-reference-regions-v4.shp')
# pr2       <- rast(crs=crs(imgBase),extent=ext(imgBase),resolution=10000)
# # robin_crs <- '+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs'
# # pr3       <- project(pr2,robin_crs)
# # res(pr3)  <- 10000
# pr3       <- project(pr2,crs(refReg))
# res(pr3)  <- 0.1
# 
# # outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_nor_qc/chg_nor_sum'
# # if (!dir.exists(outDir)) {dir.create(outDir)}
# # # r1 <- resample(mapChgNor3,pr2)
# # # writeRaster(r1,filename=paste0(outDir,'/chg_nor_sum_',tile_list[tt],'.tif'),overwrite=TRUE)
# # rast1 <- project(mapChgNor3,pr3)
# # writeRaster(rast1,filename=paste0(outDir,'/1_chg_nor_sum_',tile_list[tt],'.tif'),overwrite=TRUE)
# 
# 
# # Reproject and save changes in individual variables
# outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_nor_qc/res_org/'
# if (!dir.exists(outDir)) {dir.create(outDir)}
# outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_nor_qc/res_cor/'
# if (!dir.exists(outDir)) {dir.create(outDir)}
# 
# for(i in 1:25){
#   vv <- sprintf('%02d',i)
#   # #
#   outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_nor_qc/res_org/',vv)
#   if (!dir.exists(outDir)) {dir.create(outDir)}
#   # mapChgNor1 <- setValues(imgBase,datNorAb[,i])
#   # writeRaster(mapChgNor1,filename=paste0(outDir,'/chg_nor_abs_',tile_list[tt],'.tif'),overwrite=TRUE)
# 
#   mapChgOrg1 <- setValues(imgBase,datOrg[,i])
#   writeRaster(mapChgOrg1,filename=paste0(outDir,'/chg_org_',tile_list[tt],'.tif'),overwrite=TRUE)
#   
#   # #
#   outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_nor_qc/res_cor/',vv)
#   if (!dir.exists(outDir)) {dir.create(outDir)}
#   # values(mapChgNor1)[is.na(values(mapChgNor1))] <- 0
#   # rast1 <- project(mapChgNor1,pr3)
#   # writeRaster(rast1,filename=paste0(outDir,'/1_chg_nor_abs_',tile_list[tt],'.tif'),overwrite=TRUE)
#   
#   mapChgOrg1 <- setValues(imgBase,datOrg[,i])
#   # values(mapChgOrg1)[is.na(values(mapChgOrg1))] <- 0
#   r1 <- aggregate(mapChgOrg1,fact=20,fun='median',na.rm=T)
#   r2 <- project(r1,pr3)
#   
#   writeRaster(r2,filename=paste0(outDir,'/1_chg_org_',tile_list[tt],'.tif'),overwrite=TRUE)
#   
#   print(i)
# }



############################################################
vv <- c(1:25)

outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_nor_qc/res_cor/',sprintf('%02d',vv[tt]))
files  <- list.files(outDir,pattern=glob2rx('1_chg_org*.tif'),full.names=T)
  
print(length(files))

rlist <- vector('list',length(files))
for(i in 1:length(files)){
  rlist[[i]]<- rast(files[i])
}
rsrc <- sprc(rlist)
rMerge <- merge(rsrc)

print(rMerge)

rs <- disagg(rMerge,10,method='bilinear')

print(rs)

ds <- density(na.omit(values(rMerge)))
  
outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_nor_qc/merge')
if (!dir.exists(outDir)) {dir.create(outDir)}
writeRaster(rs,filename=paste0(outDir,'/merge_',sprintf('%02d',vv[tt]),'.tif'),overwrite=TRUE)
save(ds,file=paste0(outDir,'/merge_dnt_',sprintf('%02d',vv[tt]),'.rda'))


# ############################################################
# shp <- vect('/projectnb/modislc/users/mkmoon/TAscience/trend/data/shp/world-administrative-boundaries_edited.shp')
# 
# setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/figures/')
# png(filename=paste0('map_val_org_all_1.png'),width=13,height=8,units='in',res=300)
# 
# # MGU
# mycol <- rev(brewer.pal(11,'PiYG'))
# Pal   <- colorRampPalette(mycol)
# mycol <- Pal(300)
# 
# par(fig=c(0,0.5,0.66,1),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0))
# plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
# plot(shp,col='grey75',axes=F,add=T)
# plot(rstack[[1]],add=T,col=mycol,axes=F,range=c(-15,15),
#      plg = list(ext = c(-30, 120, -47, -43), loc = "bottom", cex=1.2,
#                 at = seq(-15,15,7.5),bty="n", 
#                 labels = c('<-15',-7.5,0,7.5,'> 15'),tck=2,
#                 cex.lab=1.2))
# plot(shp,add=T)
# box()
# par(fig=c(0,0.14,0.68,0.83),oma=c(0,0,1,1),mar=c(0,0,1,1),mgp=c(1.5,0.5,0),new=T)
# rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col='white',border=NA)
# par(fig=c(0,0.16,0.68,0.88),oma=c(0,0,1,1),mar=c(3,1,1,1),mgp=c(1.5,0.5,0),new=T)
# plot(dstack[[1]]$x,dstack[[1]]$y,xlim=c(-20,20),type='l',axes=F,lwd=2,xlab="Days",cex.lab=1.2)
# axis(1,c(-20,-10,0,10,20))
# abline(v=0,lty=5)
# 
# 
# # MGD
# mycol <- brewer.pal(11,'PiYG')
# Pal   <- colorRampPalette(mycol)
# mycol <- Pal(300)
# 
# par(fig=c(0,0.5,0.33,0.66),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0))
# plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
# plot(shp,col='grey75',axes=F,add=T)
# plot(r1,add=T,col=mycol,axes=F,range=c(-15,15),
#      plg = list(ext = c(-30, 120, -47, -43), loc = "bottom", cex=1.2,
#                 at = seq(-15,15,7.5),bty="n", 
#                 labels = c('<-15',-7.5,0,7.5,'> 15'),tck=2,
#                 cex.lab=1.2))
# plot(shp,add=T)
# box()
# par(fig=c(0,0.14,0.35,0.50),oma=c(0,0,1,1),mar=c(0,0,1,1),mgp=c(1.5,0.5,0),new=T)
# rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col='white',border=NA)
# par(fig=c(0,0.16,0.35,0.55),oma=c(0,0,1,1),mar=c(3,1,1,1),mgp=c(1.5,0.5,0),new=T)
# plot(den$x,den$y,xlim=c(-20,20),type='l',axes=F,lwd=2,xlab="Days",cex.lab=1.2)
# axis(1,c(-20,-10,0,10,20))
# abline(v=0,lty=5)
# 
# 
# 
# dev.off()
  