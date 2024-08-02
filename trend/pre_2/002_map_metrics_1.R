rm(list = ls())

library(terra)
library(sf)

library(RColorBrewer)

############################################################
# From bash code
args <- commandArgs()
print(args)

vv <- as.numeric(substr(args[3],1,3))
# vv <- 2


############################################################
shp <- vect('/projectnb/modislc/users/mkmoon/TAscience/trend/data/shp/world-administrative-boundaries_edited.shp')

path <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/parts/merge/'
vv <- c(2,6,9,16,14,15)

########################
# Metric changes
rstc <- vector('list',6)
dstc <- vector('list',6)
for(i in 1:6){
  file <- list.files(path,pattern=glob2rx(paste0('1_merge_trd*',sprintf('%02d',vv[i]),'.tif')),full.names=T)
  rstc[[i]] <- rast(file)
  
  file <- list.files(path,pattern=glob2rx(paste0('merge_dnt_trd*',sprintf('%02d',vv[i]),'.rda')),full.names=T)
  load(file)
  dstc[[i]] <- ds
}


##
setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/figures/')
png(filename=paste0('map_val_org_all_4.png'),width=14,height=8,units='in',res=300)

# MGU
mycol <- rev(brewer.pal(11,'PiYG'))
Pal   <- colorRampPalette(mycol)
mycol <- Pal(300)

par(fig=c(0,0.5,0.66,1),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0))
plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
plot(shp,col='grey75',axes=F,add=T)
r1 <- focal(rstc[[1]],3,fun='median',na.rm=T)
r1[r1 < -0.8] <- -0.8
r1[r1 >  0.8] <-  0.8
plot(r1,add=T,col=mycol,axes=F,smooth=T,
     plg = list(ext = c(-30, 120, -45.5, -42), loc = "bottom", cex=0.9,
                at = seq(-0.8,0.8,0.4),bty="n",
                labels = c('< -0.8',-0.4,0,0.4,'> 0.8'),tck=2,
                cex.lab=1.2))
plot(shp,add=T)
box()
par(fig=c(0.005,0.14,0.68,0.83),oma=c(0,0,1,1),mar=c(0,0,1,1),mgp=c(1.5,0.5,0),new=T)
rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col='white',border=NA)
par(fig=c(0.005,0.16,0.68,0.88),oma=c(0,0,1,1),mar=c(3,1,1,1),mgp=c(1.5,0.5,0),new=T)
plot(dstc[[1]]$x,dstc[[1]]$y,xlim=c(-1.4,1.4),type='l',axes=F,lwd=2,xlab="Days per year",cex.lab=0.9)
axis(1,c(-1.4,-0.7,0,0.7,1.4),cex.axis=0.9)
abline(v=0,lty=5)


# MGD
mycol <- brewer.pal(11,'PiYG')
Pal   <- colorRampPalette(mycol)
mycol <- Pal(300)

par(fig=c(0,0.5,0.33,0.66),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0),new=T)
plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
plot(shp,col='grey75',axes=F,add=T)
r1 <- focal(rstc[[2]],3,fun='median',na.rm=T)
r1[r1 < -0.8] <- -0.8
r1[r1 >  0.8] <-  0.8
plot(r1,add=T,col=mycol,axes=F,smooth=T,
     plg = list(ext = c(-30, 120, -45.5, -42), loc = "bottom", cex=0.9,
                at = seq(-0.8,0.8,0.4),bty="n",
                labels = c('< -0.8',-0.4,0,0.4,'> 0.8'),tck=2,
                cex.lab=1.2))
plot(shp,add=T)
box()
par(fig=c(0.005,0.14,0.34,0.49),oma=c(0,0,1,1),mar=c(0,0,1,1),mgp=c(1.5,0.5,0),new=T)
rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col='white',border=NA)
par(fig=c(0.005,0.16,0.34,0.54),oma=c(0,0,1,1),mar=c(3,1,1,1),mgp=c(1.5,0.5,0),new=T)
plot(dstc[[2]]$x,dstc[[2]]$y,xlim=c(-1.4,1.4),type='l',axes=F,lwd=2,xlab="Days per year",cex.lab=0.9)
axis(1,c(-1.4,-0.7,0,0.7,1.4),cex.axis=0.9)
abline(v=0,lty=5)


# GSL
par(fig=c(0,0.5,0.00,0.33),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0),new=T)
plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
plot(shp,col='grey75',axes=F,add=T)
r1 <- focal(rstc[[3]],3,fun='median',na.rm=T)
r1[r1 < -0.8] <- -0.8
r1[r1 >  0.8] <-  0.8
plot(r1,add=T,col=mycol,axes=F,smooth=T,
     plg = list(ext = c(-30, 120, -45.5, -42), loc = "bottom", cex=0.9,
                at = seq(-0.8,0.8,0.4),bty="n",
                labels = c('< -0.8',-0.4,0,0.4,'> 0.8'),tck=2,
                cex.lab=1.2))
plot(shp,add=T)
box()
par(fig=c(0.005,0.14,0.01,0.15),oma=c(0,0,1,1),mar=c(0,0,1,1),mgp=c(1.5,0.5,0),new=T)
rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col='white',border=NA)
par(fig=c(0.005,0.16,0.0,0.2),oma=c(0,0,1,1),mar=c(3,1,1,1),mgp=c(1.5,0.5,0),new=T)
plot(dstc[[3]]$x,dstc[[3]]$y,xlim=c(-1.4,1.4),type='l',axes=F,lwd=2,xlab="Days per year",cex.lab=0.9)
axis(1,c(-1.4,-0.7,0,0.7,1.4),cex.axis=0.9)
abline(v=0,lty=5)


# EVImax
par(fig=c(0.5,1,0.66,1),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0),new=T)
plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
plot(shp,col='grey75',axes=F,add=T)
r1 <- focal(rstc[[4]],3,fun='median',na.rm=T)*1000
r1[r1 < -3] <- -3
r1[r1 >  3] <-  3
plot(r1,add=T,col=mycol,axes=F,smooth=T,
     plg = list(ext = c(-30, 120, -45.5, -42), loc = "bottom", cex=0.9,
                at = seq(-3,3,1.5),bty="n",
                labels = c('< -3',-1.5,0,1.5,'> 3'),tck=2,
                cex.lab=1.2))
plot(shp,add=T)
box()
par(fig=c(0.51,0.64,0.68,0.83),oma=c(0,0,1,1),mar=c(0,0,1,1),mgp=c(1.5,0.5,0),new=T)
rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col='white',border=NA)
par(fig=c(0.51,0.66,0.68,0.88),oma=c(0,0,1,1),mar=c(3,1,1,1),mgp=c(1.5,0.5,0),new=T)
plot(dstc[[4]]$x,dstc[[4]]$y,xlim=c(-0.006,0.006),type='l',axes=F,lwd=2,xlab="EVI2 per year (x 1000)",cex.lab=0.9)
axis(1,at=c(-0.006,-0.003,0,0.003,0.006),labels=c(-6,-3,0,3,6),cex.axis=0.9)
abline(v=0,lty=5)


# EVIamp
par(fig=c(0.5,1,0.33,0.66),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0),new=T)
plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
plot(shp,col='grey75',axes=F,add=T)
r1 <- focal(rstc[[5]],3,fun='median',na.rm=T)*1000
r1[r1 < -3] <- -3
r1[r1 >  3] <-  3
plot(r1,add=T,col=mycol,axes=F,smooth=T,
     plg = list(ext = c(-30, 120, -45.5, -42), loc = "bottom", cex=0.9,
                at = seq(-3,3,1.5),bty="n",
                labels = c('< -3',-1.5,0,1.5,'> 3'),tck=2,
                cex.lab=1.2))
plot(shp,add=T)
box()
par(fig=c(0.51,0.64,0.34,0.49),oma=c(0,0,1,1),mar=c(0,0,1,1),mgp=c(1.5,0.5,0),new=T)
rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col='white',border=NA)
par(fig=c(0.51,0.66,0.34,0.54),oma=c(0,0,1,1),mar=c(3,1,1,1),mgp=c(1.5,0.5,0),new=T)
plot(dstc[[5]]$x,dstc[[5]]$y,xlim=c(-0.006,0.006),type='l',axes=F,lwd=2,xlab="EVI2 per year (x 1000)",cex.lab=0.9)
axis(1,at=c(-0.006,-0.003,0,0.003,0.006),labels=c(-6,-3,0,3,6),cex.axis=0.9)
abline(v=0,lty=5)


# EVIarea
par(fig=c(0.5,1,0.00,0.33),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0),new=T)
plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
plot(shp,col='grey75',axes=F,add=T)
r1 <- focal(rstc[[6]],3,fun='median',na.rm=T)
r1[r1 < -0.3] <- -0.3
r1[r1 >  0.3] <-  0.3
plot(r1,add=T,col=mycol,axes=F,smooth=T,
     plg = list(ext = c(-30, 120, -45.5, -42), loc = "bottom", cex=0.9,
                at = c(-0.3,-0.15,0,0.15,0.3),bty="n",
                labels = c('< -0.3',-0.15,0,0.15,'> 0.3'),tck=2,
                cex.lab=1.2))
plot(shp,add=T)
box()
par(fig=c(0.51,0.64,0.01,0.15),oma=c(0,0,1,1),mar=c(0,0,1,1),mgp=c(1.5,0.5,0),new=T)
rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col='white',border=NA)
par(fig=c(0.51,0.66,0.0,0.2),oma=c(0,0,1,1),mar=c(3,1,1,1),mgp=c(1.5,0.5,0),new=T)
plot(dstc[[6]]$x,dstc[[6]]$y,xlim=c(-0.48,0.48),type='l',axes=F,lwd=2,xlab="EVI2 per year",cex.lab=0.9)
axis(1,at=c(-0.5,-0.25,0,0.25,0.5),cex.axis=0.9)
abline(v=0,lty=5)

#
mtext('a',3, -0.2,outer=T,adj=0.003,cex=1.5,font=2)
mtext('b',3,-14.1,outer=T,adj=0.003,cex=1.5,font=2)
mtext('c',3,-27.2,outer=T,adj=0.003,cex=1.5,font=2)
mtext('d',3, -0.2,outer=T,adj=0.518,cex=1.5,font=2)
mtext('e',3,-14.1,outer=T,adj=0.518,cex=1.5,font=2)
mtext('f',3,-27.2,outer=T,adj=0.518,cex=1.5,font=2)
# mtext('MGU',3, -0.2,outer=T,adj=0.003,cex=1.5,font=2)
# mtext('MGD',3,-14.1,outer=T,adj=0.003,cex=1.5,font=2)
# mtext('GSL',3,-27.2,outer=T,adj=0.003,cex=1.5,font=2)
# mtext('EVImax',3, -0.2,outer=T,adj=0.518,cex=1.5,font=2)
# mtext('EVImin',3,-14.1,outer=T,adj=0.518,cex=1.5,font=2)
# mtext('EVIarea',3,-27.2,outer=T,adj=0.518,cex=1.5,font=2)

dev.off()



########################
# Significance
rstc <- vector('list',6)
for(i in 1:6){
  file <- list.files(path,pattern=glob2rx(paste0('1_merge_sig*',sprintf('%02d',vv[i]),'.tif')),full.names=T)
  rstc[[i]] <- rast(file)
}


##
setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/figures/')
png(filename=paste0('map_val_org_all_4_sig.png'),width=14,height=8,units='in',res=300)

# MGU
par(fig=c(0,0.5,0.66,1),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0))
plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
plot(shp,col='grey75',axes=F,add=T)
plot(rstc[[1]],add=T,col=c('red','yellow','darkorange','darkorange4'),smooth=T,
     axes=F,type='interval',breaks=c(0,0.05,0.1,0.5,1),
     plg=list(legend=c('< 0.05','0.05-0.10','0.10-0.50','0.50-1.00'),
              x="bottomleft",cex=1.1))
plot(shp,add=T)
box()


# MGD
par(fig=c(0,0.5,0.33,0.66),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0),new=T)
plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
plot(shp,col='grey75',axes=F,add=T)
plot(rstc[[2]],add=T,col=c('red','yellow','darkorange','darkorange4'),smooth=T,
     axes=F,type='interval',breaks=c(0,0.05,0.1,0.5,1))
plot(shp,add=T)
box()


# GSL
par(fig=c(0,0.5,0.00,0.33),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0),new=T)
plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
plot(shp,col='grey75',axes=F,add=T)
plot(rstc[[3]],add=T,col=c('red','yellow','darkorange','darkorange4'),smooth=T,
     axes=F,type='interval',breaks=c(0,0.05,0.1,0.5,1))
plot(shp,add=T)
box()


# EVImax
par(fig=c(0.5,1,0.66,1),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0),new=T)
plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
plot(shp,col='grey75',axes=F,add=T)
plot(rstc[[4]],add=T,col=c('red','yellow','darkorange','darkorange4'),smooth=T,
     axes=F,type='interval',breaks=c(0,0.05,0.1,0.5,1))
plot(shp,add=T)
box()


# EVIamp
par(fig=c(0.5,1,0.33,0.66),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0),new=T)
plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
plot(shp,col='grey75',axes=F,add=T)
plot(rstc[[5]],add=T,col=c('red','yellow','darkorange','darkorange4'),smooth=T,
     axes=F,type='interval',breaks=c(0,0.05,0.1,0.5,1))
plot(shp,add=T)
box()


# EVIarea
par(fig=c(0.5,1,0.00,0.33),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0),new=T)
plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
plot(shp,col='grey75',axes=F,add=T)
plot(rstc[[6]],add=T,col=c('red','yellow','darkorange','darkorange4'),smooth=T,
     axes=F,type='interval',breaks=c(0,0.05,0.1,0.5,1))
plot(shp,add=T)
box()

#
mtext('a',3, -1.2,outer=T,adj=0.003,cex=1.5,font=2)
mtext('b',3,-15.1,outer=T,adj=0.003,cex=1.5,font=2)
mtext('c',3,-28.2,outer=T,adj=0.003,cex=1.5,font=2)
mtext('d',3, -1.2,outer=T,adj=0.509,cex=1.5,font=2)
mtext('e',3,-15.1,outer=T,adj=0.509,cex=1.5,font=2)
mtext('f',3,-28.2,outer=T,adj=0.509,cex=1.5,font=2)

dev.off()
  


########################
# SD
rstc <- vector('list',6)
dstc <- vector('list',6)
for(i in 1:6){
  file <- list.files(path,pattern=glob2rx(paste0('1_merge_sd*',sprintf('%02d',vv[i]),'.tif')),full.names=T)
  rstc[[i]] <- rast(file)
  
  file <- list.files(path,pattern=glob2rx(paste0('merge_dnt_sd*',sprintf('%02d',vv[i]),'.rda')),full.names=T)
  load(file)
  dstc[[i]] <- ds
}

##
setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/figures/')
png(filename=paste0('map_val_org_all_4_sd.png'),width=14,height=8,units='in',res=300)

# MGU
mycol <- rev(brewer.pal(11,'Spectral'))
Pal   <- colorRampPalette(mycol)
mycol <- Pal(300)

par(fig=c(0,0.5,0.66,1),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0))
plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
plot(shp,col='grey75',axes=F,add=T)
r1 <- focal(rstc[[1]],3,fun='median',na.rm=T)
r1[r1 >  25] <-  25
r1[r1 <   5] <-   5
plot(r1,add=T,col=mycol,axes=F,smooth=T,
     plg = list(ext = c(-30, 120, -45.5, -42), loc = "bottom", cex=0.9,
                at = c(5,10,15,20,25),bty="n",
                labels = c('< 5',10,15,20,'> 25'),
                cex.lab=1.2))
plot(shp,add=T)
box()
par(fig=c(0.005,0.14,0.68,0.83),oma=c(0,0,1,1),mar=c(0,0,1,1),mgp=c(1.5,0.5,0),new=T)
rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col='white',border=NA)
par(fig=c(0.005,0.16,0.68,0.88),oma=c(0,0,1,1),mar=c(3,1,1,1),mgp=c(1.5,0.5,0),new=T)
plot(dstc[[1]]$x,dstc[[1]]$y,xlim=c(0,40),type='l',axes=F,lwd=2,xlab="Days",cex.lab=0.9)
axis(1,c(0,20,40),cex.axis=0.9)


# MGD
par(fig=c(0,0.5,0.33,0.66),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0),new=T)
plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
plot(shp,col='grey75',axes=F,add=T)
r1 <- focal(rstc[[2]],3,fun='median',na.rm=T)
r1[r1 >  25] <-  25
r1[r1 <   5] <-   5
plot(r1,add=T,col=mycol,axes=F,smooth=T,
     plg = list(ext = c(-30, 120, -45.5, -42), loc = "bottom", cex=0.9,
                at = c(5,10,15,20,25),bty="n",
                labels = c('< 5',10,15,20,'> 25'),
                cex.lab=1.2))
plot(shp,add=T)
box()
par(fig=c(0.005,0.14,0.34,0.49),oma=c(0,0,1,1),mar=c(0,0,1,1),mgp=c(1.5,0.5,0),new=T)
rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col='white',border=NA)
par(fig=c(0.005,0.16,0.34,0.54),oma=c(0,0,1,1),mar=c(3,1,1,1),mgp=c(1.5,0.5,0),new=T)
plot(dstc[[2]]$x,dstc[[2]]$y,xlim=c(0,40),type='l',axes=F,lwd=2,xlab="Days",cex.lab=0.9)
axis(1,c(0,20,40),cex.axis=0.9)


# GSL
par(fig=c(0,0.5,0.00,0.33),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0),new=T)
plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
plot(shp,col='grey75',axes=F,add=T)
r1 <- focal(rstc[[3]],3,fun='median',na.rm=T)
r1[r1 >  25] <-  25
r1[r1 <   5] <-   5
plot(r1,add=T,col=mycol,axes=F,smooth=T,
     plg = list(ext = c(-30, 120, -45.5, -42), loc = "bottom", cex=0.9,
                at = c(5,10,15,20,25),bty="n",
                labels = c('< 5',10,15,20,'> 25'),
                cex.lab=1.2))
plot(shp,add=T)
box()
par(fig=c(0.005,0.14,0.01,0.15),oma=c(0,0,1,1),mar=c(0,0,1,1),mgp=c(1.5,0.5,0),new=T)
rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col='white',border=NA)
par(fig=c(0.005,0.16,0.0,0.2),oma=c(0,0,1,1),mar=c(3,1,1,1),mgp=c(1.5,0.5,0),new=T)
plot(dstc[[3]]$x,dstc[[3]]$y,xlim=c(0,40),type='l',axes=F,lwd=2,xlab="Days",cex.lab=0.9)
axis(1,c(0,20,40),cex.axis=0.9)


# EVImax
par(fig=c(0.5,1,0.66,1),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0),new=T)
plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
plot(shp,col='grey75',axes=F,add=T)
r1 <- focal(rstc[[4]],3,fun='median',na.rm=T)
r1[r1 <  0.01] <-  0.01
r1[r1 >  0.05] <-  0.05
plot(r1,add=T,col=mycol,axes=F,smooth=T,
     plg = list(ext = c(-30, 120, -45.5, -42), loc = "bottom", cex=0.9,
                at = seq(0.01,0.05,0.01),bty="n",
                labels = c('< 0.01',0.02,0.03,0.04,'> 0.05'),tck=2,
                cex.lab=1.2))
plot(shp,add=T)
box()
par(fig=c(0.51,0.64,0.68,0.83),oma=c(0,0,1,1),mar=c(0,0,1,1),mgp=c(1.5,0.5,0),new=T)
rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col='white',border=NA)
par(fig=c(0.51,0.66,0.68,0.88),oma=c(0,0,1,1),mar=c(3,1,1,1),mgp=c(1.5,0.5,0),new=T)
plot(dstc[[4]]$x,dstc[[4]]$y,xlim=c(0,0.1),type='l',axes=F,lwd=2,xlab="EVI2",cex.lab=0.9)
axis(1,c(0,0.05,0.1),cex.axis=0.9)


# EVIamp
par(fig=c(0.5,1,0.33,0.66),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0),new=T)
plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
plot(shp,col='grey75',axes=F,add=T)
r1 <- focal(rstc[[5]],3,fun='median',na.rm=T)
r1[r1 < 0.01] <- 0.01
r1[r1 > 0.05] <- 0.05
plot(r1,add=T,col=mycol,axes=F,smooth=T,
     plg = list(ext = c(-30, 120, -45.5, -42), loc = "bottom", cex=0.9,
                at = seq(0.01,0.05,0.01),bty="n",
                labels = c('< 0.01',0.02,0.03,0.04,'> 0.05'),tck=2,
                cex.lab=1.2))
plot(shp,add=T)
box()
par(fig=c(0.51,0.64,0.34,0.49),oma=c(0,0,1,1),mar=c(0,0,1,1),mgp=c(1.5,0.5,0),new=T)
rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col='white',border=NA)
par(fig=c(0.51,0.66,0.34,0.54),oma=c(0,0,1,1),mar=c(3,1,1,1),mgp=c(1.5,0.5,0),new=T)
plot(dstc[[5]]$x,dstc[[5]]$y,xlim=c(0,0.1),type='l',axes=F,lwd=2,xlab="EVI2",cex.lab=0.9)
axis(1,c(0,0.05,0.1),cex.axis=0.9)


# EVIarea
par(fig=c(0.5,1,0.00,0.33),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0),new=T)
plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
plot(shp,col='grey75',axes=F,add=T)
r1 <- focal(rstc[[6]],3,fun='median',na.rm=T)
r1[r1 < 1] <- 1
r1[r1 > 6] <- 6
plot(r1,add=T,col=mycol,axes=F,smooth=T,
     plg = list(ext = c(-30, 120, -45.5, -42), loc = "bottom", cex=0.9,
                at = seq(1,6,1),bty="n",
                labels = c('< 1',2,3,4,5,'> 6'),tck=2,
                cex.lab=1.2))
plot(shp,add=T)
box()
par(fig=c(0.51,0.64,0.01,0.15),oma=c(0,0,1,1),mar=c(0,0,1,1),mgp=c(1.5,0.5,0),new=T)
rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col='white',border=NA)
par(fig=c(0.51,0.66,0.0,0.2),oma=c(0,0,1,1),mar=c(3,1,1,1),mgp=c(1.5,0.5,0),new=T)
plot(dstc[[6]]$x,dstc[[6]]$y,xlim=c(0,10),type='l',axes=F,lwd=2,xlab="EVI2",cex.lab=0.9)
axis(1,c(0,5,10),cex.axis=0.9)

#
mtext('a',3, -0.2,outer=T,adj=0.003,cex=1.5,font=2)
mtext('b',3,-14.1,outer=T,adj=0.003,cex=1.5,font=2)
mtext('c',3,-27.2,outer=T,adj=0.003,cex=1.5,font=2)
mtext('d',3, -0.2,outer=T,adj=0.518,cex=1.5,font=2)
mtext('e',3,-14.1,outer=T,adj=0.518,cex=1.5,font=2)
mtext('f',3,-27.2,outer=T,adj=0.518,cex=1.5,font=2)

dev.off()