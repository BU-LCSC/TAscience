rm(list = ls())

library(terra)
library(sf)

library(remotePARTS)

library(RColorBrewer)
library(scales)


############################################################
refReg <- vect('/projectnb/modislc/users/mkmoon/TAscience/trend/data/shp/IPCC-WGI-reference-regions-v4.shp')
rrName <- refReg$Acronym
rrType <- refReg$Type
rName <- rep(rrName,each=4)
rType <- rep(rrType,each=4)

dat <- matrix(NA,length(rName),36)
for(i in 1:6){
  path  <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/re/parts/',sprintf('%02d',i))
  files <- list.files(path,pattern=glob2rx('spa*'),full.names=T)

  for(j in 1:length(files)){
    load(files[j])
    dat[which(rName==mDat[1,1]),((i-1)*6+1):(i*6)] <- as.matrix(mDat[,2:7])
  }
  print(i)
}

dat <- data.frame(rName,rType,dat)
write.csv(dat,file='/projectnb/modislc/users/mkmoon/TAscience/trend/data/re/parts/stat_pval_300_22yrs.csv')


############################################################
shp <- vect('/projectnb/modislc/users/mkmoon/TAscience/trend/data/shp/world-administrative-boundaries_edited.shp')
refReg <- vect('/projectnb/modislc/users/mkmoon/TAscience/trend/data/shp/IPCC-WGI-reference-regions-v4.shp')

sDat <- read.csv('/projectnb/modislc/users/mkmoon/TAscience/trend/data/re/parts/stat_pval_300_22yrs_cleaned.csv',header=T)

mycol1 <- rev(brewer.pal(11,'RdYlGn'))
mycol1 <- colorRampPalette(mycol1)
mycol1 <- mycol1(200)
mycol2 <- brewer.pal(11,'RdYlGn')
mycol2 <- colorRampPalette(mycol2)
mycol2 <- mycol2(200)

pval <- sDat[1:44,c(6,10,14,18,22,26)]
tmag <- sDat[1:44,c(4,8,12,16,20,24)]
tcol <- matrix(NA,dim(pval)[1],6)
for(i in 1:6){
  tmp <- tmag[,i]
  tmx <- quantile(abs(tmag[,i]),0.5,na.rm=T)
  tmp <- tmp/tmx
  tmp <- round(tmp*100)
  tmp[tmp >   100] <-  100 
  tmp[tmp < - 100] <- -100 
  tmp <- ifelse(tmp<0,tmp+101,tmp+100)
  # tmp[refReg$Type=='Ocean'] <- NA
  tcol[,i] <- tmp
}


##
setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/figures/')
png(filename=paste0('map_sig_ipcc_4_re.png'),width=14,height=8,units='in',res=300)

par(fig=c(0,0.5,0.66,1),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0))
plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
plot(shp,col=NA,axes=F,add=T)
refReg$sig <- ifelse(pval[,1]<0.05,1,0)
refReg$sig[refReg$Type=='Ocean'] <- NA
refReg$sig[is.na(tcol[,1])] <- NA
for(i in 1:44){
  tmp <- crop(shp,refReg[i])
  plot(tmp,col=alpha(mycol1[tcol[i,1]],0.7),axes=NULL,add=T,lwd=0.3)
  plot(tmp,density=c(35*refReg$sig[i]),angle=c(45*refReg$sig[i]),axes=NULL,add=T,lwd=0.5,border='gray55')
}
plot(shp,col=NA,axes=F,add=T,border='black')
box()

par(fig=c(0,0.5,0.33,0.66),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0),new=T)
plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
plot(shp,col='grey75',axes=F,add=T)
refReg$sig <- ifelse(pval[,2]<0.05,1,0)
refReg$sig[refReg$Type=='Ocean'] <- NA
refReg$sig[is.na(tcol[,2])] <- NA
for(i in 1:44){
  tmp <- crop(shp,refReg[i])
  plot(tmp,col=alpha(mycol2[tcol[i,2]],0.7),axes=NULL,add=T,lwd=0.3)
  plot(tmp,density=c(35*refReg$sig[i]),angle=c(45*refReg$sig[i]),axes=NULL,add=T,lwd=0.5,border='gray55')
}
plot(shp,col=NA,axes=F,add=T,border='black')
box()

par(fig=c(0,0.5,0.00,0.33),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0),new=T)
plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
plot(shp,col='grey75',axes=F,add=T)
refReg$sig <- ifelse(pval[,3]<0.05,1,0)
refReg$sig[refReg$Type=='Ocean'] <- NA
refReg$sig[is.na(tcol[,3])] <- NA
for(i in 1:44){
  tmp <- crop(shp,refReg[i])
  plot(tmp,col=alpha(mycol2[tcol[i,3]],0.7),axes=NULL,add=T,lwd=0.3)
  plot(tmp,density=c(35*refReg$sig[i]),angle=c(45*refReg$sig[i]),axes=NULL,add=T,lwd=0.5,border='gray55')
}
plot(shp,col=NA,axes=F,add=T,border='black')
box()

par(fig=c(0.5,1,0.66,1),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0),new=T)
plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
plot(shp,col='grey75',axes=F,add=T)
refReg$sig <- ifelse(pval[,4]<0.05,1,0)
refReg$sig[refReg$Type=='Ocean'] <- NA
refReg$sig[is.na(tcol[,4])] <- NA
for(i in 1:44){
  tmp <- crop(shp,refReg[i])
  plot(tmp,col=alpha(mycol2[tcol[i,4]],0.7),axes=NULL,add=T,lwd=0.3)
  plot(tmp,density=c(35*refReg$sig[i]),angle=c(45*refReg$sig[i]),axes=NULL,add=T,lwd=0.5,border='gray55')
}
plot(shp,col=NA,axes=F,add=T,border='black')
box()

par(fig=c(0.5,1,0.33,0.66),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0),new=T)
plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
plot(shp,col='grey75',axes=F,add=T)
refReg$sig <- ifelse(pval[,5]<0.05,1,0)
refReg$sig[refReg$Type=='Ocean'] <- NA
refReg$sig[is.na(tcol[,5])] <- NA
for(i in 1:44){
  tmp <- crop(shp,refReg[i])
  plot(tmp,col=alpha(mycol2[tcol[i,5]],0.7),axes=NULL,add=T,lwd=0.3)
  plot(tmp,density=c(35*refReg$sig[i]),angle=c(45*refReg$sig[i]),axes=NULL,add=T,lwd=0.5,border='gray55')
}
plot(shp,col=NA,axes=F,add=T,border='black')
box()

par(fig=c(0.5,1,0.00,0.33),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0),new=T)
plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
plot(shp,col='grey75',axes=F,add=T)
refReg$sig <- ifelse(pval[,6]<0.05,1,0)
refReg$sig[refReg$Type=='Ocean'] <- NA
refReg$sig[is.na(tcol[,6])] <- NA
for(i in 1:44){
  tmp <- crop(shp,refReg[i])
  plot(tmp,col=alpha(mycol2[tcol[i,6]],0.7),axes=NULL,add=T,lwd=0.3)
  plot(tmp,density=c(35*refReg$sig[i]),angle=c(45*refReg$sig[i]),axes=NULL,add=T,lwd=0.5,border='gray55')
}
plot(shp,col=NA,axes=F,add=T,border='black')
box()

mtext('a',3, -1.3,outer=T,adj=0.003,cex=1.5,font=2)
mtext('b',3,-15.3,outer=T,adj=0.003,cex=1.5,font=2)
mtext('c',3,-28.3,outer=T,adj=0.003,cex=1.5,font=2)
mtext('d',3, -1.3,outer=T,adj=0.51,cex=1.5,font=2)
mtext('e',3,-15.3,outer=T,adj=0.51,cex=1.5,font=2)
mtext('f',3,-28.3,outer=T,adj=0.51,cex=1.5,font=2)

dev.off()



# ############################################################
# ## Map
# shp <- vect('/projectnb/modislc/users/mkmoon/TAscience/trend/data/shp/world-administrative-boundaries_edited.shp')
# path <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/parts/merge/'
# vv <- c(2,6,9,16,14,15)
# 
# # Metric changes
# rstc <- vector('list',6)
# dstc <- vector('list',6)
# for(i in 1:6){
#   file <- list.files(path,pattern=glob2rx(paste0('1_merge_trd*',sprintf('%02d',vv[i]),'.tif')),full.names=T)
#   rstc[[i]] <- rast(file)
# 
#   file <- list.files(path,pattern=glob2rx(paste0('merge_dnt_trd*',sprintf('%02d',vv[i]),'.rda')),full.names=T)
#   load(file)
#   dstc[[i]] <- ds
# }
# 
# ############################################################
# refReg <- vect('/projectnb/modislc/users/mkmoon/TAscience/trend/data/shp/IPCC-WGI-reference-regions-v4.shp')
# rrName <- unique(refReg$Acronym)
# rName  <- rrName[2]
# 
# ##
# setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/figures/')
# png(filename=paste0('map_trend_days_3_nwn.png'),width=14,height=8,units='in',res=300)
# 
# # MGU
# mycol <- rev(brewer.pal(11,'PiYG'))
# Pal   <- colorRampPalette(mycol)
# mycol <- Pal(300)
# 
# par(fig=c(0,0.5,0.66,1),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0))
# plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
# plot(shp,col='grey75',axes=F,add=T)
# r1 <- focal(rstc[[1]],3,fun='median',na.rm=T)
# r1[r1 < -0.8] <- -0.8
# r1[r1 >  0.8] <-  0.8
# plot(r1,add=T,col=mycol,axes=F,smooth=T,
#      plg = list(ext = c(-30, 120, -45.5, -42), loc = "bottom", cex=0.9,
#                 at = seq(-0.8,0.8,0.4),bty="n",
#                 labels = c('< -0.8',-0.4,0,0.4,'> 0.8'),tck=2,
#                 cex.lab=1.2))
# plot(shp,add=T)
# plot(refReg,border=c(NA,'red',rep(NA,56)),axes=NULL,add=T,lwd=3)
# box()
# par(fig=c(0.005,0.14,0.68,0.83),oma=c(0,0,1,1),mar=c(0,0,1,1),mgp=c(1.5,0.5,0),new=T)
# rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col='white',border=NA)
# par(fig=c(0.005,0.16,0.68,0.88),oma=c(0,0,1,1),mar=c(3,1,1,1),mgp=c(1.5,0.5,0),new=T)
# plot(dstc[[1]]$x,dstc[[1]]$y,xlim=c(-1.4,1.4),type='l',axes=F,lwd=2,xlab="Days per year",cex.lab=0.9)
# axis(1,c(-1.4,-0.7,0,0.7,1.4),cex.axis=0.9)
# abline(v=0,lty=5)
# 
# 
# # MGD
# mycol <- brewer.pal(11,'PiYG')
# Pal   <- colorRampPalette(mycol)
# mycol <- Pal(300)
# 
# par(fig=c(0,0.5,0.33,0.66),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0),new=T)
# plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
# plot(shp,col='grey75',axes=F,add=T)
# r1 <- focal(rstc[[2]],3,fun='median',na.rm=T)
# r1[r1 < -0.8] <- -0.8
# r1[r1 >  0.8] <-  0.8
# plot(r1,add=T,col=mycol,axes=F,smooth=T,
#      plg = list(ext = c(-30, 120, -45.5, -42), loc = "bottom", cex=0.9,
#                 at = seq(-0.8,0.8,0.4),bty="n",
#                 labels = c('< -0.8',-0.4,0,0.4,'> 0.8'),tck=2,
#                 cex.lab=1.2))
# plot(shp,add=T)
# plot(refReg,border=c(NA,'red',rep(NA,56)),axes=NULL,add=T,lwd=3)
# box()
# par(fig=c(0.005,0.14,0.34,0.49),oma=c(0,0,1,1),mar=c(0,0,1,1),mgp=c(1.5,0.5,0),new=T)
# rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col='white',border=NA)
# par(fig=c(0.005,0.16,0.34,0.54),oma=c(0,0,1,1),mar=c(3,1,1,1),mgp=c(1.5,0.5,0),new=T)
# plot(dstc[[2]]$x,dstc[[2]]$y,xlim=c(-1.4,1.4),type='l',axes=F,lwd=2,xlab="Days per year",cex.lab=0.9)
# axis(1,c(-1.4,-0.7,0,0.7,1.4),cex.axis=0.9)
# abline(v=0,lty=5)
# 
# 
# # GSL
# par(fig=c(0,0.5,0.00,0.33),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0),new=T)
# plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
# plot(shp,col='grey75',axes=F,add=T)
# r1 <- focal(rstc[[3]],3,fun='median',na.rm=T)
# r1[r1 < -0.8] <- -0.8
# r1[r1 >  0.8] <-  0.8
# plot(r1,add=T,col=mycol,axes=F,
#      plg = list(ext = c(-30, 120, -45.5, -42), loc = "bottom", cex=0.9,
#                 at = seq(-0.8,0.8,0.4),bty="n",
#                 labels = c('< -0.8',-0.4,0,0.4,'> 0.8'),tck=2,
#                 cex.lab=1.2))
# plot(shp,add=T)
# plot(refReg,border=c(NA,'red',rep(NA,56)),axes=NULL,add=T,lwd=3)
# box()
# par(fig=c(0.005,0.14,0.01,0.15),oma=c(0,0,1,1),mar=c(0,0,1,1),mgp=c(1.5,0.5,0),new=T)
# rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col='white',border=NA)
# par(fig=c(0.005,0.16,0.0,0.2),oma=c(0,0,1,1),mar=c(3,1,1,1),mgp=c(1.5,0.5,0),new=T)
# plot(dstc[[3]]$x,dstc[[3]]$y,xlim=c(-1.4,1.4),type='l',axes=F,lwd=2,xlab="Days per year",cex.lab=0.9)
# axis(1,c(-1.4,-0.7,0,0.7,1.4),cex.axis=0.9)
# abline(v=0,lty=5)
# 
# ####
# lct <- c('ENF','EBF','DNF','DBF','MF','CSH','OSH','WSA','SAV','GRA',
#          'WET','CRO','UBN','CNM','SNW','BAR','WAR')
# mm <- 2
# metric <- sprintf('%02d',mm)
# outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/parts/vals/by_eco/',metric)
# file1  <- list.files(outDir,pattern=glob2rx(paste0('dat_',rName,'.rda')),full.names=T)
# outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/parts/vals/by_eco/lct/'
# file3  <- list.files(outDir,pattern=glob2rx(paste0('lct_',rName,'.rda')),full.names=T)
# load(file1)
# load(file3)
# lct <- unlist(lct)
# lct[lct==5] <- 4 
# lct[lct==7] <- 6 
# lct[lct==9|lct==10] <- 8 
# nlct <- table(lct)
# frqLand <- as.numeric(names(sort(nlct,decreasing=TRUE)[1:3]))
# datF$land[datF$land==5] <- 4
# datF$land[datF$land==7] <- 6
# datF$land[datF$land==9|datF$land==10] <- 8
# subD1 <- datF[which(datF$land==frqLand[1]),]
# subD2 <- datF[which(datF$land==frqLand[2]),]
# subD3 <- datF[which(datF$land==frqLand[3]),]
# 
# par(fig=c(0.5,1,0.66,1),oma=c(2,2,1,2),mar=c(4,4,0,0),mgp=c(2,0.7,0),new=T)
# boxplot(subD3[,1],subD2[,1],subD1[,1],datF[,1],
#         horizontal=T,cex.axis=0.9,
#         outline=F,ylim=c(-1.5,1.5),names=rev(c('All','WS','SH','ENF')),
#         ylab='Land Cover Type',xlab='Trend (days/year)',cex.lab=1.1)
# abline(v=0,lty=5,)
# 
# mm <- 6
# metric <- sprintf('%02d',mm)
# outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/parts/vals/by_eco/',metric)
# file1  <- list.files(outDir,pattern=glob2rx(paste0('dat_',rName,'.rda')),full.names=T)
# load(file1)
# datF$land[datF$land==5] <- 4
# datF$land[datF$land==7] <- 6
# datF$land[datF$land==9|datF$land==10] <- 8
# subD1 <- datF[which(datF$land==frqLand[1]),]
# subD2 <- datF[which(datF$land==frqLand[2]),]
# subD3 <- datF[which(datF$land==frqLand[3]),]
# par(fig=c(0.5,1,0.3,0.64),oma=c(2,2,1.2,2),mar=c(4,4,0,0),mgp=c(2.5,1,0),new=T)
# boxplot(subD3[,1],subD2[,1],subD1[,1],datF[,1],
#         horizontal=T,cex.axis=0.9,
#         outline=F,ylim=c(-1.5,1.5),names=rev(c('All','WS','SH','ENF')),
#         ylab='Land Cover Type',xlab='Trend (days/year)',cex.lab=1.1)
# abline(v=0,lty=5,)
# 
# mm <- 9
# metric <- sprintf('%02d',mm)
# outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/parts/vals/by_eco/',metric)
# file1  <- list.files(outDir,pattern=glob2rx(paste0('dat_',rName,'.rda')),full.names=T)
# load(file1)
# datF$land[datF$land==5] <- 4
# datF$land[datF$land==7] <- 6
# datF$land[datF$land==9|datF$land==10] <- 8
# subD1 <- datF[which(datF$land==frqLand[1]),]
# subD2 <- datF[which(datF$land==frqLand[2]),]
# subD3 <- datF[which(datF$land==frqLand[3]),]
# par(fig=c(0.5,1,0,0.32),oma=c(0,2,2,2),mar=c(4,4,0,0),mgp=c(2.5,1,0),new=T)
# boxplot(subD3[,1],subD2[,1],subD1[,1],datF[,1],
#         horizontal=T,cex.axis=0.9,
#         outline=F,ylim=c(-1.5,1.5),names=rev(c('All','WS','SH','ENF')),
#         ylab='Land Cover Type',xlab='Trend (days/year)',cex.lab=1.1)
# abline(v=0,lty=5,)
# 
# dev.off()
# 
# 
# ############################################################
# refReg <- vect('/projectnb/modislc/users/mkmoon/TAscience/trend/data/shp/IPCC-WGI-reference-regions-v4.shp')
# rrName <- unique(refReg$Acronym)
# rName  <- rrName[29]
# 
# ##
# setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/figures/')
# png(filename=paste0('map_trend_days_3_rar.png'),width=14,height=8,units='in',res=300)
# 
# # MGU
# mycol <- rev(brewer.pal(11,'PiYG'))
# Pal   <- colorRampPalette(mycol)
# mycol <- Pal(300)
# 
# par(fig=c(0,0.5,0.66,1),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0))
# plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
# plot(shp,col='grey75',axes=F,add=T)
# r1 <- focal(rstc[[1]],3,fun='median',na.rm=T)
# r1[r1 < -0.8] <- -0.8
# r1[r1 >  0.8] <-  0.8
# plot(r1,add=T,col=mycol,axes=F,smooth=T,
#      plg = list(ext = c(-30, 120, -45.5, -42), loc = "bottom", cex=0.9,
#                 at = seq(-0.8,0.8,0.4),bty="n",
#                 labels = c('< -0.8',-0.4,0,0.4,'> 0.8'),tck=2,
#                 cex.lab=1.2))
# plot(shp,add=T)
# plot(refReg,border=c(rep(NA,28),'red',rep(NA,29)),axes=NULL,add=T,lwd=3)
# box()
# par(fig=c(0.005,0.14,0.68,0.83),oma=c(0,0,1,1),mar=c(0,0,1,1),mgp=c(1.5,0.5,0),new=T)
# rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col='white',border=NA)
# par(fig=c(0.005,0.16,0.68,0.88),oma=c(0,0,1,1),mar=c(3,1,1,1),mgp=c(1.5,0.5,0),new=T)
# plot(dstc[[1]]$x,dstc[[1]]$y,xlim=c(-1.4,1.4),type='l',axes=F,lwd=2,xlab="Days per year",cex.lab=0.9)
# axis(1,c(-1.4,-0.7,0,0.7,1.4),cex.axis=0.9)
# abline(v=0,lty=5)
# 
# 
# # MGD
# mycol <- brewer.pal(11,'PiYG')
# Pal   <- colorRampPalette(mycol)
# mycol <- Pal(300)
# 
# par(fig=c(0,0.5,0.33,0.66),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0),new=T)
# plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
# plot(shp,col='grey75',axes=F,add=T)
# r1 <- focal(rstc[[2]],3,fun='median',na.rm=T)
# r1[r1 < -0.8] <- -0.8
# r1[r1 >  0.8] <-  0.8
# plot(r1,add=T,col=mycol,axes=F,smooth=T,
#      plg = list(ext = c(-30, 120, -45.5, -42), loc = "bottom", cex=0.9,
#                 at = seq(-0.8,0.8,0.4),bty="n",
#                 labels = c('< -0.8',-0.4,0,0.4,'> 0.8'),tck=2,
#                 cex.lab=1.2))
# plot(shp,add=T)
# plot(refReg,border=c(rep(NA,28),'red',rep(NA,29)),axes=NULL,add=T,lwd=3)
# box()
# par(fig=c(0.005,0.14,0.34,0.49),oma=c(0,0,1,1),mar=c(0,0,1,1),mgp=c(1.5,0.5,0),new=T)
# rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col='white',border=NA)
# par(fig=c(0.005,0.16,0.34,0.54),oma=c(0,0,1,1),mar=c(3,1,1,1),mgp=c(1.5,0.5,0),new=T)
# plot(dstc[[2]]$x,dstc[[2]]$y,xlim=c(-1.4,1.4),type='l',axes=F,lwd=2,xlab="Days per year",cex.lab=0.9)
# axis(1,c(-1.4,-0.7,0,0.7,1.4),cex.axis=0.9)
# abline(v=0,lty=5)
# 
# 
# # GSL
# par(fig=c(0,0.5,0.00,0.33),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0),new=T)
# plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
# plot(shp,col='grey75',axes=F,add=T)
# r1 <- focal(rstc[[3]],3,fun='median',na.rm=T)
# r1[r1 < -0.8] <- -0.8
# r1[r1 >  0.8] <-  0.8
# plot(r1,add=T,col=mycol,axes=F,
#      plg = list(ext = c(-30, 120, -45.5, -42), loc = "bottom", cex=0.9,
#                 at = seq(-0.8,0.8,0.4),bty="n",
#                 labels = c('< -0.8',-0.4,0,0.4,'> 0.8'),tck=2,
#                 cex.lab=1.2))
# plot(shp,add=T)
# plot(refReg,border=c(rep(NA,28),'red',rep(NA,29)),axes=NULL,add=T,lwd=3)
# box()
# par(fig=c(0.005,0.14,0.01,0.15),oma=c(0,0,1,1),mar=c(0,0,1,1),mgp=c(1.5,0.5,0),new=T)
# rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col='white',border=NA)
# par(fig=c(0.005,0.16,0.0,0.2),oma=c(0,0,1,1),mar=c(3,1,1,1),mgp=c(1.5,0.5,0),new=T)
# plot(dstc[[3]]$x,dstc[[3]]$y,xlim=c(-1.4,1.4),type='l',axes=F,lwd=2,xlab="Days per year",cex.lab=0.9)
# axis(1,c(-1.4,-0.7,0,0.7,1.4),cex.axis=0.9)
# abline(v=0,lty=5)
# 
# 
# ############################################################
# lct <- c('ENF','EBF','DNF','DBF','MF','CSH','OSH','WSA','SAV','GRA',
#          'WET','CRO','UBN','CNM','SNW','BAR','WAR')
# 
# mm <- 2
# metric <- sprintf('%02d',mm)
# outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/parts/vals/by_eco/',metric)
# file1  <- list.files(outDir,pattern=glob2rx(paste0('dat_',rName,'.rda')),full.names=T)
# outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/parts/vals/by_eco/lct/'
# file3  <- list.files(outDir,pattern=glob2rx(paste0('lct_',rName,'.rda')),full.names=T)
# load(file1)
# load(file3)
# lct <- unlist(lct)
# lct[lct==5] <- 4 
# lct[lct==7] <- 6 
# lct[lct==9|lct==10] <- 8 
# nlct <- table(lct)
# frqLand <- as.numeric(names(sort(nlct,decreasing=TRUE)[1:3]))
# datF$land[datF$land==5] <- 4
# datF$land[datF$land==7] <- 6
# datF$land[datF$land==9|datF$land==10] <- 8
# subD1 <- datF[which(datF$land==frqLand[1]),]
# subD2 <- datF[which(datF$land==frqLand[2]),]
# subD3 <- datF[which(datF$land==frqLand[3]),]
# 
# par(fig=c(0.5,1,0.66,1),oma=c(2,2,1,2),mar=c(4,4,0,0),mgp=c(2,0.7,0),new=T)
# boxplot(subD3[,1],subD2[,1],subD1[,1],datF[,1],
#         horizontal=T,cex.axis=0.9,
#         outline=F,ylim=c(-1.5,1.5),names=rev(c('All','SH','WS','PW')),
#         ylab='Land Cover Type',xlab='Trend (days/year)',cex.lab=1.1)
# abline(v=0,lty=5,)
# 
# mm <- 6
# metric <- sprintf('%02d',mm)
# outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/parts/vals/by_eco/',metric)
# file1  <- list.files(outDir,pattern=glob2rx(paste0('dat_',rName,'.rda')),full.names=T)
# load(file1)
# datF$land[datF$land==5] <- 4
# datF$land[datF$land==7] <- 6
# datF$land[datF$land==9|datF$land==10] <- 8
# subD1 <- datF[which(datF$land==frqLand[1]),]
# subD2 <- datF[which(datF$land==frqLand[2]),]
# subD3 <- datF[which(datF$land==frqLand[3]),]
# par(fig=c(0.5,1,0.3,0.64),oma=c(2,2,1.2,2),mar=c(4,4,0,0),mgp=c(2.5,1,0),new=T)
# boxplot(subD3[,1],subD2[,1],subD1[,1],datF[,1],
#         horizontal=T,cex.axis=0.9,
#         outline=F,ylim=c(-1.5,1.5),names=rev(c('All','SH','WS','PW')),
#         ylab='Land Cover Type',xlab='Trend (days/year)',cex.lab=1.1)
# abline(v=0,lty=5,)
# 
# mm <- 9
# metric <- sprintf('%02d',mm)
# outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/parts/vals/by_eco/',metric)
# file1  <- list.files(outDir,pattern=glob2rx(paste0('dat_',rName,'.rda')),full.names=T)
# load(file1)
# datF$land[datF$land==5] <- 4
# datF$land[datF$land==7] <- 6
# datF$land[datF$land==9|datF$land==10] <- 8
# subD1 <- datF[which(datF$land==frqLand[1]),]
# subD2 <- datF[which(datF$land==frqLand[2]),]
# subD3 <- datF[which(datF$land==frqLand[3]),]
# par(fig=c(0.5,1,0,0.32),oma=c(0,2,2,2),mar=c(4,4,0,0),mgp=c(2.5,1,0),new=T)
# boxplot(subD3[,1],subD2[,1],subD1[,1],datF[,1],
#         horizontal=T,cex.axis=0.9,
#         outline=F,ylim=c(-1.5,1.5),names=rev(c('All','SH','WS','PW')),
#         ylab='Land Cover Type',xlab='Trend (days/year)',cex.lab=1.1)
# abline(v=0,lty=5,)
# 
# dev.off()
# 
# 
# ############################################################
# refReg <- vect('/projectnb/modislc/users/mkmoon/TAscience/trend/data/shp/IPCC-WGI-reference-regions-v4.shp')
# rrName <- unique(refReg$Acronym)
# rName  <- rrName[6]
# 
# ##
# setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/figures/')
# png(filename=paste0('map_trend_days_3_ena.png'),width=14,height=8,units='in',res=300)
# 
# # MGU
# mycol <- rev(brewer.pal(11,'PiYG'))
# Pal   <- colorRampPalette(mycol)
# mycol <- Pal(300)
# 
# par(fig=c(0,0.5,0.66,1),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0))
# plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
# plot(shp,col='grey75',axes=F,add=T)
# r1 <- focal(rstc[[1]],3,fun='median',na.rm=T)
# r1[r1 < -0.8] <- -0.8
# r1[r1 >  0.8] <-  0.8
# plot(r1,add=T,col=mycol,axes=F,smooth=T,
#      plg = list(ext = c(-30, 120, -45.5, -42), loc = "bottom", cex=0.9,
#                 at = seq(-0.8,0.8,0.4),bty="n",
#                 labels = c('< -0.8',-0.4,0,0.4,'> 0.8'),tck=2,
#                 cex.lab=1.2))
# plot(shp,add=T)
# plot(refReg,border=c(rep(NA,5),'red',rep(NA,52)),axes=NULL,add=T,lwd=3)
# box()
# par(fig=c(0.005,0.14,0.68,0.83),oma=c(0,0,1,1),mar=c(0,0,1,1),mgp=c(1.5,0.5,0),new=T)
# rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col='white',border=NA)
# par(fig=c(0.005,0.16,0.68,0.88),oma=c(0,0,1,1),mar=c(3,1,1,1),mgp=c(1.5,0.5,0),new=T)
# plot(dstc[[1]]$x,dstc[[1]]$y,xlim=c(-1.4,1.4),type='l',axes=F,lwd=2,xlab="Days per year",cex.lab=0.9)
# axis(1,c(-1.4,-0.7,0,0.7,1.4),cex.axis=0.9)
# abline(v=0,lty=5)
# 
# 
# # MGD
# mycol <- brewer.pal(11,'PiYG')
# Pal   <- colorRampPalette(mycol)
# mycol <- Pal(300)
# 
# par(fig=c(0,0.5,0.33,0.66),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0),new=T)
# plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
# plot(shp,col='grey75',axes=F,add=T)
# r1 <- focal(rstc[[2]],3,fun='median',na.rm=T)
# r1[r1 < -0.8] <- -0.8
# r1[r1 >  0.8] <-  0.8
# plot(r1,add=T,col=mycol,axes=F,smooth=T,
#      plg = list(ext = c(-30, 120, -45.5, -42), loc = "bottom", cex=0.9,
#                 at = seq(-0.8,0.8,0.4),bty="n",
#                 labels = c('< -0.8',-0.4,0,0.4,'> 0.8'),tck=2,
#                 cex.lab=1.2))
# plot(shp,add=T)
# plot(refReg,border=c(rep(NA,5),'red',rep(NA,52)),axes=NULL,add=T,lwd=3)
# box()
# par(fig=c(0.005,0.14,0.34,0.49),oma=c(0,0,1,1),mar=c(0,0,1,1),mgp=c(1.5,0.5,0),new=T)
# rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col='white',border=NA)
# par(fig=c(0.005,0.16,0.34,0.54),oma=c(0,0,1,1),mar=c(3,1,1,1),mgp=c(1.5,0.5,0),new=T)
# plot(dstc[[2]]$x,dstc[[2]]$y,xlim=c(-1.4,1.4),type='l',axes=F,lwd=2,xlab="Days per year",cex.lab=0.9)
# axis(1,c(-1.4,-0.7,0,0.7,1.4),cex.axis=0.9)
# abline(v=0,lty=5)
# 
# 
# # GSL
# par(fig=c(0,0.5,0.00,0.33),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0),new=T)
# plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
# plot(shp,col='grey75',axes=F,add=T)
# r1 <- focal(rstc[[3]],3,fun='median',na.rm=T)
# r1[r1 < -0.8] <- -0.8
# r1[r1 >  0.8] <-  0.8
# plot(r1,add=T,col=mycol,axes=F,
#      plg = list(ext = c(-30, 120, -45.5, -42), loc = "bottom", cex=0.9,
#                 at = seq(-0.8,0.8,0.4),bty="n",
#                 labels = c('< -0.8',-0.4,0,0.4,'> 0.8'),tck=2,
#                 cex.lab=1.2))
# plot(shp,add=T)
# plot(refReg,border=c(rep(NA,5),'red',rep(NA,52)),axes=NULL,add=T,lwd=3)
# box()
# par(fig=c(0.005,0.14,0.01,0.15),oma=c(0,0,1,1),mar=c(0,0,1,1),mgp=c(1.5,0.5,0),new=T)
# rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col='white',border=NA)
# par(fig=c(0.005,0.16,0.0,0.2),oma=c(0,0,1,1),mar=c(3,1,1,1),mgp=c(1.5,0.5,0),new=T)
# plot(dstc[[3]]$x,dstc[[3]]$y,xlim=c(-1.4,1.4),type='l',axes=F,lwd=2,xlab="Days per year",cex.lab=0.9)
# axis(1,c(-1.4,-0.7,0,0.7,1.4),cex.axis=0.9)
# abline(v=0,lty=5)
# 
# 
# ############################################################
# lct <- c('ENF','EBF','DNF','DBF','MF','CSH','OSH','WSA','SAV','GRA',
#          'WET','CRO','UBN','CNM','SNW','BAR','WAR')
# 
# mm <- 2
# metric <- sprintf('%02d',mm)
# outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/parts/vals/by_eco/',metric)
# file1  <- list.files(outDir,pattern=glob2rx(paste0('dat_',rName,'.rda')),full.names=T)
# outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/parts/vals/by_eco/lct/'
# file3  <- list.files(outDir,pattern=glob2rx(paste0('lct_',rName,'.rda')),full.names=T)
# load(file1)
# load(file3)
# lct <- unlist(lct)
# lct[lct==5] <- 4 
# lct[lct==7] <- 6 
# lct[lct==9|lct==10] <- 8 
# nlct <- table(lct)
# frqLand <- as.numeric(names(sort(nlct,decreasing=TRUE)[1:3]))
# datF$land[datF$land==5] <- 4
# datF$land[datF$land==7] <- 6
# datF$land[datF$land==9|datF$land==10] <- 8
# subD1 <- datF[which(datF$land==frqLand[1]),]
# subD2 <- datF[which(datF$land==frqLand[2]),]
# subD3 <- datF[which(datF$land==frqLand[3]),]
# 
# par(fig=c(0.5,1,0.66,1),oma=c(2,2,1,2),mar=c(4,4,0,0),mgp=c(2,0.7,0),new=T)
# boxplot(subD3[,1],subD2[,1],subD1[,1],datF[,1],
#         horizontal=T,cex.axis=0.9,
#         outline=F,ylim=c(-1.5,1.5),names=rev(c('All','MF','WS','CR')),
#         ylab='Land Cover Type',xlab='Trend (days/year)',cex.lab=1.1)
# abline(v=0,lty=5,)
# 
# mm <- 6
# metric <- sprintf('%02d',mm)
# outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/parts/vals/by_eco/',metric)
# file1  <- list.files(outDir,pattern=glob2rx(paste0('dat_',rName,'.rda')),full.names=T)
# load(file1)
# datF$land[datF$land==5] <- 4
# datF$land[datF$land==7] <- 6
# datF$land[datF$land==9|datF$land==10] <- 8
# subD1 <- datF[which(datF$land==frqLand[1]),]
# subD2 <- datF[which(datF$land==frqLand[2]),]
# subD3 <- datF[which(datF$land==frqLand[3]),]
# par(fig=c(0.5,1,0.3,0.64),oma=c(2,2,1.2,2),mar=c(4,4,0,0),mgp=c(2.5,1,0),new=T)
# boxplot(subD3[,1],subD2[,1],subD1[,1],datF[,1],
#         horizontal=T,cex.axis=0.9,
#         outline=F,ylim=c(-1.5,1.5),names=rev(c('All','MF','WS','CR')),
#         ylab='Land Cover Type',xlab='Trend (days/year)',cex.lab=1.1)
# abline(v=0,lty=5,)
# 
# mm <- 9
# metric <- sprintf('%02d',mm)
# outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/parts/vals/by_eco/',metric)
# file1  <- list.files(outDir,pattern=glob2rx(paste0('dat_',rName,'.rda')),full.names=T)
# load(file1)
# datF$land[datF$land==5] <- 4
# datF$land[datF$land==7] <- 6
# datF$land[datF$land==9|datF$land==10] <- 8
# subD1 <- datF[which(datF$land==frqLand[1]),]
# subD2 <- datF[which(datF$land==frqLand[2]),]
# subD3 <- datF[which(datF$land==frqLand[3]),]
# par(fig=c(0.5,1,0,0.32),oma=c(0,2,2,2),mar=c(4,4,0,0),mgp=c(2.5,1,0),new=T)
# boxplot(subD3[,1],subD2[,1],subD1[,1],datF[,1],
#         horizontal=T,cex.axis=0.9,
#         outline=F,ylim=c(-1.5,1.5),names=rev(c('All','MF','WS','CR')),
#         ylab='Land Cover Type',xlab='Trend (days/year)',cex.lab=1.1)
# abline(v=0,lty=5,)
# 
# dev.off()