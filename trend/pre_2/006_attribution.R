rm(list = ls())

library(terra)
library(sf)

library(RColorBrewer)

library(randomForest)
library(fastshap)

library(Ternary)
library(scales)


# ############################################################
# # From bash code
# args <- commandArgs()
# print(args)
# 
# mm <- as.numeric(substr(args[3],1,3))
# rr <- as.numeric(substr(args[3],4,6))
# # mm <- 1; rr <- 2
# 
# ############################################################
# refReg <- vect('/projectnb/modislc/users/mkmoon/TAscience/trend/data/shp/IPCC-WGI-reference-regions-v4.shp')
# rrName <- unique(refReg$Acronym)
# rName  <- rrName[rr]
# 
# print(rName)
# 
# subReg <- subset(refReg, refReg$Acronym == rName)
# 
# 
# ############################################################
# # Metric changes
# path <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/parts/merge/'
# vv <- c(2,6,9,16,14,15)
# 
# rstc <- vector('list',6)
# for(i in 1:6){
#   file <- list.files(path,pattern=glob2rx(paste0('1_merge_trd*',sprintf('%02d',vv[i]),'.tif')),full.names=T)
#   rstc[[i]] <- rast(file)
# }
# 
# # Climate changes
# path <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/parts/climate/'
# files <- list.files(path,pattern=glob2rx(paste0('1_chg_cli_dif*')),full.names=T)
# 
# rcli <- vector('list',15)
# for(i in 1:15){
#   rcli[[i]] <- rast(files[i])
# }
# 
# 
# ############################################################
# e1 <- mask(rstc[[1]],subReg)
# 
# dat1 <- matrix(NA,length(values(e1)),6)
# for(i in 1:6){
#   tmp <- mask(rstc[[i]],subReg)
#   dat1[,i] <- values(tmp)
# }
# 
# dat2 <- matrix(NA,length(values(e1)),15)
# for(i in 1:15){
#   tmp <- mask(rcli[[i]],subReg)
#   dat2[,i] <- values(tmp)
# }
# 
# 
# ############################################################
# dat <- data.frame(dat1[,mm],dat2)
# colnames(dat) <- c('phe',
#                    'tmp','tmp1','tmp2','tmp3','tmp4',
#                    'rad','rad1','rad2','rad3','rad4',
#                    'sp1','sp3','sp6','sp12','sp36')
# dat <- na.omit(dat)
# if(dim(dat)[1]>3000){
#   dat <- dat[sample(1:dim(dat)[1],3000),]
# }
# 
# print(dim(dat))
# 
# ranF <- randomForest(phe ~ .,data=dat,importance=T,ntree=3000)
# 
# set.seed(101)  # for reproducibility
# shap <- explain(ranF,X=dat[,-1],nsim=100,pred_wrapper=predict)
# 
# # save
# outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/parts/att/three/dat/'
# save(dat,ranF,shap,file=paste0(outDir,'rf_',rName,'_',mm,'.rda'))
# 
# ##
# # figure
# setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/data/parts/att/three/fig/')
# png(filename=paste0('att_',rName,'_',mm,'.png'),width=13,height=8,units='in',res=300)
# 
# limv <- quantile(abs(dat[,1]),0.99)
# 
# par(mfrow=c(1,2),oma=c(1,2,1,2))
# plot(dat[,1],ranF$predicted,xlim=c(-limv,limv),ylim=c(-limv,limv),
#      main=paste0(rName,'_',mm),xlab='MODIS',ylab='Modeled',
#      cex.lab=1.3,cex.main=1.3)
# abline(0,1)
# reg <- summary(lm(ranF$predicted~dat[,1]))
# legend('bottomright',legend=round(reg$adj.r.squared,3),cex=1.5,bty='n')
# 
# oD <- order(ranF$importance[,1])
# barplot(ranF$importance[oD[6:15],1],horiz=T)
# 
# dev.off()



############################################################
shp <- vect('/projectnb/modislc/users/mkmoon/TAscience/trend/data/shp/world-administrative-boundaries_edited.shp')

refReg <- vect('/projectnb/modislc/users/mkmoon/TAscience/trend/data/shp/IPCC-WGI-reference-regions-v4.shp')
rrName <- unique(refReg$Acronym)

# aDAT <- vector('list',6)
# for(mm in 1:6){
#   aDat <- matrix(NA,44,4)
#   for(rr in 1:44){
#     rName  <- rrName[rr]
# 
#     ##
#     path  <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/parts/att/three/dat'
#     fName <- list.files(path,pattern=glob2rx(paste0('*',rName,'_',mm,'.rda')),full.names=T)
# 
#     load(fName)
# 
#     reg <- summary(lm(ranF$predicted~dat[,1]))
#     
#     dd   <- apply(abs(shap),2,mean)
#     aTmp <- sum(dd[c(1:5)])
#     aRad <- sum(dd[c(6:10)])
#     aMoi <- sum(dd[c(11:15)])
#     aDat[rr,] <- c(aTmp,aRad,aMoi,reg$adj.r.squared)
# 
#   }
#   aDAT[[mm]] <- aDat
# 
#   print(mm)
# }
# #
# path  <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/parts/att/three/'
# save(aDAT,file=paste0(path,'stat_att.rda'))

load('/projectnb/modislc/users/mkmoon/TAscience/trend/data/parts/att/three/stat_att.rda')


############################################################
# # figure
# setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/figures/')
# png(filename=paste0('att_importance_1.png'),width=13,height=8,units='in',res=300)
# 
# phe <- c('MGU','MGD','GSL','EVImax','EVIamp','EVIarea')
# 
# par(mfrow=c(2,3))
# for(i in 1:6){
#   boxplot(aDAT[[i]][,1:3],main=phe[i],cex.main=2,
#           names=c('Temp','Radi','Mois'),cex.axis=1.5)
# }
# 
# dev.off()

rgbWhite <- function (r, g, b,alpha) {
  highest <- apply(rbind(r, g, b), 2L, max)
  rgb(r/highest, g/highest, b/highest,alpha)
}
values <- TernaryPointValues(rgbWhite, resolution = 200)

##
setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/figures/')
png(filename=paste0('map_att_ipcc_2.png'),width=14,height=8,units='in',res=300)

par(fig=c(0,0.5,0.66,1),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0))
plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
plot(shp,axes=F,add=T,col='grey75')
for(i in 1:44){
  dd  <- aDAT[[1]][i,]
  c1  <- dd[1:3]
  c1  <- c1/sum(c1)
  
  tmp <- crop(shp,refReg[i])
  plot(tmp,col=rgbWhite(c1[1],c1[2],c1[3],1),axes=NULL,add=T,lwd=0.3,border='gray55')
}
plot(shp,col=NA,axes=F,add=T,border='black')
box()
par(fig=c(0.005,0.14,0.68,0.83),oma=c(0,0,1,1),mar=c(0,0,1,1),mgp=c(1.5,0.5,0),new=T)
rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col='white',border=NA)
par(fig=c(0.000,0.17,0.65,0.945),oma=c(0,0,1,1),mar=c(1,0,1,1),mgp=c(0.5,0.5,0),new=T)
TernaryPlot(point = "up", grid.minor.lines = 0,
            grid.lty = 5, col = rgb(1,1,1),grid.lines = 0, 
            axis.col = NULL, padding = 0.15)
# cols <- TernaryPointValues(rgb)
# ColourTernary(cols, spectrum = NULL)
ColourTernary(values, spectrum = NULL)
dd <- aDAT[[1]][,1:3]
c1 <- apply(dd,1,sum)
dd <- dd/c1
TernaryPoints(dd,pch=1,cex=0.5,lwd=0.7)

par(fig=c(0,0.5,0.33,0.66),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0),new=T)
plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
plot(shp,axes=F,add=T,col='grey75')
for(i in 1:44){
  dd  <- aDAT[[2]][i,]
  c1  <- dd[1:3]
  c1  <- c1/sum(c1)

  tmp <- crop(shp,refReg[i])
  plot(tmp,col=rgbWhite(c1[1],c1[2],c1[3],1),axes=NULL,add=T,lwd=0.3,border='gray55')
}
plot(shp,col=NA,axes=F,add=T,border='black')
box()
par(fig=c(0.005,0.14,0.34,0.49),oma=c(0,0,1,1),mar=c(0,0,1,1),mgp=c(1.5,0.5,0),new=T)
rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col='white',border=NA)
par(fig=c(0.000,0.17,0.31,0.605),oma=c(0,0,1,1),mar=c(1,0,1,1),mgp=c(0.5,0.5,0),new=T)
TernaryPlot(point = "up", grid.minor.lines = 0,
            grid.lty = 5, col = rgb(1,1,1),grid.lines = 0, 
            axis.col = NULL, padding = 0.15)
# cols <- TernaryPointValues(rgb)
# ColourTernary(cols, spectrum = NULL)
ColourTernary(values, spectrum = NULL)
dd <- aDAT[[2]][,1:3]
c1 <- apply(dd,1,sum)
dd <- dd/c1
TernaryPoints(dd,pch=1,cex=0.5,lwd=0.7)

par(fig=c(0,0.5,0.00,0.33),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0),new=T)
plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
plot(shp,axes=F,add=T,col='grey75')
for(i in 1:44){
  dd  <- aDAT[[3]][i,]
  c1  <- dd[1:3]
  c1  <- c1/sum(c1)

  tmp <- crop(shp,refReg[i])
  plot(tmp,col=rgbWhite(c1[1],c1[2],c1[3],1),axes=NULL,add=T,lwd=0.3,border='gray55')
}
plot(shp,col=NA,axes=F,add=T,border='black')
box()
par(fig=c(0.005,0.14,0.01,0.15),oma=c(0,0,1,1),mar=c(0,0,1,1),mgp=c(1.5,0.5,0),new=T)
rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col='white',border=NA)
par(fig=c(0.000,0.17,0.0,0.295),oma=c(0,0,1,1),mar=c(0,0,2,1),mgp=c(0.5,0.5,0),new=T)
TernaryPlot(point = "up", grid.minor.lines = 0,
            grid.lty = 5, col = rgb(1,1,1),grid.lines = 0, 
            axis.col = NULL, padding = 0.15)
# cols <- TernaryPointValues(rgb)
# ColourTernary(cols, spectrum = NULL)
ColourTernary(values, spectrum = NULL)
dd <- aDAT[[3]][,1:3]
c1 <- apply(dd,1,sum)
dd <- dd/c1
TernaryPoints(dd,pch=1,cex=0.5,lwd=0.7)

par(fig=c(0.5,1,0.66,1),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0),new=T)
plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
plot(shp,axes=F,add=T,col='grey75')
for(i in 1:44){
  dd  <- aDAT[[4]][i,]
  c1  <- dd[1:3]
  c1  <- c1/sum(c1)
  
  tmp <- crop(shp,refReg[i])
  plot(tmp,col=rgbWhite(c1[1],c1[2],c1[3],1),axes=NULL,add=T,lwd=0.3,border='gray55')
}
plot(shp,col=NA,axes=F,add=T,border='black')
box()
par(fig=c(0.51,0.64,0.68,0.83),oma=c(0,0,1,1),mar=c(0,0,1,1),mgp=c(1.5,0.5,0),new=T)
rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col='white',border=NA)
par(fig=c(0.505,0.675,0.65,0.945),oma=c(0,0,1,1),mar=c(1,0,1,1),mgp=c(0.5,0.5,0),new=T)
TernaryPlot(point = "up", grid.minor.lines = 0,
            grid.lty = 5, col = rgb(1,1,1),grid.lines = 0, 
            axis.col = NULL, padding = 0.15)
# cols <- TernaryPointValues(rgb)
# ColourTernary(cols, spectrum = NULL)
ColourTernary(values, spectrum = NULL)
dd <- aDAT[[4]][,1:3]
c1 <- apply(dd,1,sum)
dd <- dd/c1
TernaryPoints(dd,pch=1,cex=0.5,lwd=0.7)

par(fig=c(0.5,1,0.33,0.66),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0),new=T)
plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
plot(shp,axes=F,add=T,col='grey75')
for(i in 1:44){
  dd  <- aDAT[[5]][i,]
  c1  <- dd[1:3]
  c1  <- c1/sum(c1)
  
  tmp <- crop(shp,refReg[i])
  plot(tmp,col=rgbWhite(c1[1],c1[2],c1[3],1),axes=NULL,add=T,lwd=0.3,border='gray55')
}
plot(shp,col=NA,axes=F,add=T,border='black')
box()
par(fig=c(0.51,0.64,0.34,0.49),oma=c(0,0,1,1),mar=c(0,0,1,1),mgp=c(1.5,0.5,0),new=T)
rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col='white',border=NA)
par(fig=c(0.505,0.675,0.31,0.605),oma=c(0,0,1,1),mar=c(1,0,1,1),mgp=c(0.5,0.5,0),new=T)
TernaryPlot(point = "up", grid.minor.lines = 0,
            grid.lty = 5, col = rgb(1,1,1),grid.lines = 0, 
            axis.col = NULL, padding = 0.15)
# cols <- TernaryPointValues(rgb)
# ColourTernary(cols, spectrum = NULL)
ColourTernary(values, spectrum = NULL)
dd <- aDAT[[5]][,1:3]
c1 <- apply(dd,1,sum)
dd <- dd/c1
TernaryPoints(dd,pch=1,cex=0.5,lwd=0.7)

par(fig=c(0.5,1,0.00,0.33),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0),new=T)
plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
plot(shp,axes=F,add=T,col='grey75')
for(i in 1:44){
  dd  <- aDAT[[6]][i,]
  c1  <- dd[1:3]
  c1  <- c1/sum(c1)
  
  tmp <- crop(shp,refReg[i])
  plot(tmp,col=rgbWhite(c1[1],c1[2],c1[3],1),axes=NULL,add=T,lwd=0.3,border='gray55')
}
plot(shp,col=NA,axes=F,add=T,border='black')
box()
par(fig=c(0.51,0.64,0.01,0.15),oma=c(0,0,1,1),mar=c(0,0,1,1),mgp=c(1.5,0.5,0),new=T)
rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col='white',border=NA)
par(fig=c(0.505,0.675,0.0,0.295),oma=c(0,0,1,1),mar=c(0,0,2,1),mgp=c(0.5,0.5,0),new=T)
TernaryPlot(point = "up", grid.minor.lines = 0,
            grid.lty = 5, col = rgb(1,1,1),grid.lines = 0, 
            axis.col = NULL, padding = 0.15)
# cols <- TernaryPointValues(rgb)
# ColourTernary(cols, spectrum = NULL)
ColourTernary(values, spectrum = NULL)
dd <- aDAT[[6]][,1:3]
c1 <- apply(dd,1,sum)
dd <- dd/c1
TernaryPoints(dd,pch=1,cex=0.5,lwd=0.7)

mtext('a',3, -0.2,outer=T,adj=0.003,cex=1.5,font=2)
mtext('b',3,-14.1,outer=T,adj=0.003,cex=1.5,font=2)
mtext('c',3,-27.2,outer=T,adj=0.003,cex=1.5,font=2)
mtext('d',3, -0.2,outer=T,adj=0.518,cex=1.5,font=2)
mtext('e',3,-14.1,outer=T,adj=0.518,cex=1.5,font=2)
mtext('f',3,-27.2,outer=T,adj=0.518,cex=1.5,font=2)

dev.off()


############################################################
dStat1 <- matrix(NA,44,6)
dStat2 <- matrix(NA,44,6)
dStat3 <- matrix(NA,44,6)
for(mm in 1:6){
  for(rr in 1:44){
    rName  <- rrName[rr]
    path1  <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/parts/att/three/dat'
    fName1 <- list.files(path1,pattern=glob2rx(paste0('*',rName,'_',mm,'.rda')),full.names=T)
    path2  <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/parts/att/noPhe/dat'
    fName2 <- list.files(path2,pattern=glob2rx(paste0('*',rName,'_',mm,'.rda')),full.names=T)
    path3  <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/parts/att/withPhe/dat'
    fName3 <- list.files(path3,pattern=glob2rx(paste0('*',rName,'_',mm,'.rda')),full.names=T)
    
    load(fName1)
    reg <- summary(lm(ranF$predicted~dat[,1]))
    dStat1[rr,mm] <- reg$adj.r.squared
    
    load(fName2)
    reg <- summary(lm(ranF$predicted~dat[,1]))
    dStat2[rr,mm] <- reg$adj.r.squared
    
    load(fName3)
    reg <- summary(lm(ranF$predicted~dat[,1]))
    dStat3[rr,mm] <- reg$adj.r.squared
  }
  print(mm)
}


##
setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/figures/')
png(filename=paste0('att_rsq.png'),width=14,height=8,units='in',res=300)

par(mfrow=c(3,3),oma=c(1,1,1,1),mar=c(4,4,1,1),mgp=c(2.5,0.5,0))
for(i in 1:3){
  plot(dStat2[,i],dStat3[,i],xlim=c(0,1),ylim=c(0,1),xlab='noPhe',ylab='WithPhe',cex.lab=2,cex.axis=1.5)
  abline(0,1)
  plot(dStat1[,i],dStat3[,i],xlim=c(0,1),ylim=c(0,1),xlab='three',ylab='WithPhe',cex.lab=2,cex.axis=1.5)
  abline(0,1)
  plot(dStat1[,i],dStat2[,i],xlim=c(0,1),ylim=c(0,1),xlab='three',ylab='noPhe',cex.lab=2,cex.axis=1.5)
  abline(0,1)
}

dev.off()


