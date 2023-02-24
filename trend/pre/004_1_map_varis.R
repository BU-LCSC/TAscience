rm(list = ls())

library(gdalUtils)
library(sp)
library(raster)

library(doMC)
library(doParallel)

library(RColorBrewer)


############################################################
# From bash code
args <- commandArgs()
print(args)

ii <- as.numeric(args[3])
# ii <- 1


# ############################################################
# #
# path <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_nor/clust_2pc'
# files <- list.files(path=paste0(path,'/vals'),pattern=glob2rx('*.rda'),full.names=T)
# 
# registerDoMC()
# datCl <-foreach(tt=1:315, .combine=c) %dopar% {
#   load(files[tt])
#   na.omit(dat[,1])
# }
# tnCl <- table(datCl)
# tnClSort <- tnCl[order(tnCl,decreasing=T)]
# 
# mycol <- c('#ccb879ff','#68ab5fff','#1c5f2cff','#dcd939ff','#7584c9ff','#ff7f00ff','#ed79e4ff','#e31a1cff','#ab6c28ff')
# mycol <- mycol[order(tnCl,decreasing=T)]
# 
# 
# ##
# setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/figures/')
# png(filename='clust_portion.png',width=4.5,height=5,units='in',res=150,bg=NA)
# 
# par(oma=c(2,2,1,1),mar=c(4,4,2,1),mgp=c(2.5,1,0))
# barplot(tnClSort/sum(tnCl)*100,col=mycol,ylim=c(0,30),
#         ylab='Portion of Coverage (%)',cex.lab=1.5,cex.axis=1.3,
#         axisnames=F)
# 
# dev.off()

############################################################
# Map for each variable
path <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_nor/res_cor'

# ii <- 1

files <- list.files(paste0(path,'/',sprintf('%02d',ii)),pattern=glob2rx('1*nor_abs*.tif'),full.names=T)
rast <- raster(files[1])
shp <- shapefile('/projectnb/modislc/users/mkmoon/TAscience/trend/data/shp/world-administrative-boundaries_edited.shp')
shp <- spTransform(shp,crs(rast))
shp <- crop(shp,extent(-15000000,17000000,-6500000,8500000))


setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/figures/')
png(filename=paste0('map_val_nor_',sprintf('%02d',ii),'.png'),width=9.2,height=4.2,units='in',res=300)

par(oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0))
plot(shp,col='grey75')
mycolRamp <- colorRampPalette(c('#640065ff','#8200bcff','#1f00ffff','#00ffebff','#52ff00ff',
                               '#ffff00ff','#ff4200ff','#ff0000ff','#c90000ff','#8d0000ff','#610000ff'))
mycol <- c(rgb(1,1,1,0),mycolRamp(300))

for(j in 1:315){
  rast <- raster(files[j])
  rast[rast <   0] <-   0
  rast[rast > 1.5] <- 1.5

  r1 <- disaggregate(rast,10,method='bilinear')
  plot(r1,add=T,legend=F,col=mycol,zlim=c(0,1.5))

  print(j)
}
plot(shp,add=T)

par(fig=c(0.37,0.845,0,0.16),oma=c(0,0,1,1),mar=c(0,0,1,1),mgp=c(1.5,0.5,0),new=T)
rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col='white',border=NA)

par(fig=c(0,1,0,1),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0),bty='n',new=T)
plot(raster(matrix(seq(0,1.5,length.out=100),10,10)),
     legend.only=T,
     col=mycol,
     zlim=c(0,1.5),
     legend.width=1.5,
     legend.shrink=0.6,
     horiz=T,lwd=1.5,
     smallplot=c(0.42,0.77,0.12,0.145),
     axis.args=list(at=c(0,0.5,1,1.5),
                    labels=c(0,0.5,1,'> 1.5'),
                    cex.axis=1.2,font=1))


dev.off()


###
# Merge image
library(png)

# example image
img1 <- readPNG('/projectnb/modislc/users/mkmoon/TAscience/trend/figures/map_val_nor_02.png')
img2 <- readPNG('/projectnb/modislc/users/mkmoon/TAscience/trend/figures/map_val_nor_06.png')
img3 <- readPNG('/projectnb/modislc/users/mkmoon/TAscience/trend/figures/map_val_nor_09.png')
img4 <- readPNG('/projectnb/modislc/users/mkmoon/TAscience/trend/figures/map_val_nor_14.png')
img5 <- readPNG('/projectnb/modislc/users/mkmoon/TAscience/trend/figures/map_val_nor_20.png')
img6 <- readPNG('/projectnb/modislc/users/mkmoon/TAscience/trend/figures/map_val_nor_23.png')

# setup plot
setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/figures/')
png(filename=paste0('map_val_nor_all.png'),width=13,height=8,units='in',res=300)

par(mar=rep(0,4)) # no margins
layout(matrix(1:6, ncol=2, byrow=TRUE))

plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
rasterImage(img1,0,0,1,1); box()
plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
rasterImage(img4,0,0,1,1); box()
plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
rasterImage(img2,0,0,1,1); box()
plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
rasterImage(img5,0,0,1,1); box()
plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
rasterImage(img3,0,0,1,1); box()
plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
rasterImage(img6,0,0,1,1); box()

mtext('a',3, -2.2,outer=T,adj=0.010,cex=1.5,font=2)
mtext('b',3,-22.5,outer=T,adj=0.010,cex=1.5,font=2)
mtext('c',3,-42.8,outer=T,adj=0.010,cex=1.5,font=2)
mtext('d',3, -2.2,outer=T,adj=0.520,cex=1.5,font=2)
mtext('e',3,-22.5,outer=T,adj=0.520,cex=1.5,font=2)
mtext('f',3,-42.8,outer=T,adj=0.520,cex=1.5,font=2)

dev.off()
