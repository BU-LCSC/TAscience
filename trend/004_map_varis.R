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

vv <- c(2,6,9,16,20,23)

print(vv[ii])
# ii <- 2

files <- list.files(paste0(path,'/',sprintf('%02d',vv[ii])),pattern=glob2rx('*org*.tif'),full.names=T)
rast <- raster(files[1])
shp <- shapefile('/projectnb/modislc/users/mkmoon/TAscience/trend/data/shp/world-administrative-boundaries_edited.shp')
shp <- spTransform(shp,crs(rast))
shp <- crop(shp,extent(-15000000,17000000,-6500000,8500000))


setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/figures/')
png(filename=paste0('map_val_org_',sprintf('%02d',vv[ii]),'.png'),width=9.2,height=4.2,units='in',res=300)

par(fig=c(0,1,0,1),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0),bty='n')
plot(shp,col='grey75')
# dev.off()
# par(fig=c(0.4,0.8,0,0.13),oma=c(1,2,1,1),mar=c(3,1,1,1),mgp=c(1.5,0.5,0),new=T)
# rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col='white',border=NA)

if(vv[ii]==2){
  mycol <- rev(brewer.pal(11,'PiYG'))
}else{
  mycol <-    (brewer.pal(11,'PiYG'))
}
Pal   <- colorRampPalette(mycol)
mycol <- Pal(300)

# foreach(j=1:10) %dopar% {
for(j in 1:315){
  rast <- raster(files[j])
  if(vv[ii]==2|vv[ii]==6|vv[ii]==9){
    rast[rast < -15] <- -15
    rast[rast >  15] <-  15
    r1 <- disaggregate(rast,10,method='bilinear')
    plot(r1,add=T,legend=F,col=mycol,zlim=c(-15,15))
  }else if(vv[ii]==16){
    rast[rast < -0.05] <- -0.05
    rast[rast >  0.05] <-  0.05
    r1 <- disaggregate(rast,10,method='bilinear')
    plot(r1,add=T,legend=F,col=mycol,zlim=c(-0.05,0.05))
  }else{
    rast[rast < -0.001] <- -0.001
    rast[rast >  0.001] <-  0.001
    r1 <- disaggregate(rast,10,method='bilinear')
    plot(r1,add=T,legend=F,col=mycol,zlim=c(-0.001,0.001))
  }
  print(j)
}
plot(shp,add=T)

par(fig=c(0.37,0.845,0,0.16),oma=c(0,0,1,1),mar=c(0,0,1,1),mgp=c(1.5,0.5,0),new=T)
rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col='white',border=NA)

par(fig=c(0,1,0,1),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0),bty='n',new=T)
# par(fig=c(0,1,0,1),oma=c(1,2,1,1),mar=c(3,1,1,1),mgp=c(1.5,0.5,0),new=T)
if(vv[ii]==2|vv[ii]==6|vv[ii]==9){
  plot(raster(matrix(seq(-15,15,length.out=100),10,10)),
       legend.only=T,
       col=mycol,
       zlim=c(-15,15),
       legend.width=1.5,
       legend.shrink=0.6,
       horiz=T,lwd=1.5,
       smallplot=c(0.4,0.8,0.12,0.145),
       axis.args=list(at=c(-15,-7.5,0,7.5,15),
                      labels=c('< -15',-7.5,0,7.5,'> 15'),
                      cex.axis=1.2,font=1))
}else if(vv[ii]==16){
  plot(raster(matrix(seq(-0.05,0.05,length.out=100),10,10)),
       legend.only=T,
       col=Pal(100),
       zlim=c(-0.05,0.05),
       legend.width=1.5,
       legend.shrink=0.6,
       horiz=T,lwd=1.5,
       smallplot=c(0.4,0.8,0.12,0.145),
       axis.args=list(at=c(-0.05,-0.025,0,0.025,0.05),
                      labels=c('< -0.05',-0.025,0,0.025,'> 0.05'),
                      cex.axis=1.3,font=1))
}else{
  plot(raster(matrix(seq(-0.001,0.001,length.out=100),10,10)),
       legend.only=T,
       col=Pal(100),
       zlim=c(-0.001,0.001),
       legend.width=1.5,
       legend.shrink=0.6,
       horiz=T,lwd=1.5,
       smallplot=c(0.4,0.8,0.12,0.145),
       axis.args=list(at=c(-0.001,-0.0005,0,0.0005,0.001),
                      labels=c('< -0.1',-0.05,0,0.05,'> 0.1'),
                      cex.axis=1.3,font=1))
}

# Density plot
path <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_nor/clust_2pc/map_coarse/map_clust'
CLfiles <- list.files(path,pattern=glob2rx('*.tif'),full.names=T)

Val <- c()
val1 <- c(); val2 <- c(); val3 <- c(); val4 <- c(); val5 <- c(); val6 <- c(); val7 <- c(); val8 <- c(); val9 <- c()
for(j in 1:315){
  val   <- values(raster(files[j]))
  CLval <- values(raster(CLfiles[j]))
  if(sum(!is.na(val))>100){
     Val <- c(Val,na.omit(val))
     
     tmp1 <- val[which(CLval==1)];    tmp2 <- val[which(CLval==2)];    tmp3 <- val[which(CLval==3)]
     tmp4 <- val[which(CLval==4)];    tmp5 <- val[which(CLval==5)];    tmp6 <- val[which(CLval==6)]
     tmp7 <- val[which(CLval==7)];    tmp8 <- val[which(CLval==8)];    tmp9 <- val[which(CLval==9)]
     
     val1 <- c(val1,na.omit(tmp1));    val2 <- c(val2,na.omit(tmp2));    val3 <- c(val3,na.omit(tmp3))
     val4 <- c(val4,na.omit(tmp4));    val5 <- c(val5,na.omit(tmp5));    val6 <- c(val6,na.omit(tmp6))
     val7 <- c(val7,na.omit(tmp7));    val8 <- c(val8,na.omit(tmp8));    val9 <- c(val9,na.omit(tmp9))
  }
  print(j)
}
den <- density(Val)

#
par(fig=c(0,0.24,0,0.42),oma=c(0,0,1,1),mar=c(0,0,1,1),mgp=c(1.5,0.5,0),new=T)
rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col='white',border=NA)
par(fig=c(0,0.28,0,0.55),oma=c(0,0,1,1),mar=c(3,1,1,1),mgp=c(1.5,0.5,0),new=T)
if(vv[ii]==2|vv[ii]==6|vv[ii]==9){
  plot(den$x,den$y,xlim=c(-20,20),type='l',axes=F,lwd=2,xlab="Days",cex.lab=1.2)
  axis(1,c(-20,-10,0,10,20))
}else if(vv[ii]==16){
  plot(den$x,den$y,xlim=c(-0.1,0.1),type='l',axes=F,lwd=2,xlab="EVI",cex.lab=1.2)
  axis(1,c(-0.1,-0.05,0,0.05,0.1))
}else{
  plot(den$x,den$y,xlim=c(-0.0011,0.0011),type='l',axes=F,lwd=2,xlab="EVI / day",cex.lab=1.2)
  axis(1,at=c(-0.002,-0.001,-0.0005,0,0.0005,0.001,0.002),labels=c(-0.2,-0.1,-0.05,0,0.05,0.1,0.2))
}
abline(v=0,lty=5)
# polygon(c(den$x[den$x>=0],0),c(den$y[den$x>=0],0),col=mycol[80],border=1)
# polygon(c(den$x[den$x<=0],0),c(den$y[den$x<=0],0),col=mycol[20],border=1)

dev.off()

# Save density values
path <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data'
save(Val,
     val1,val2,val3,val4,val5,val6,val7,val8,val9,
     file=paste0(path,'/density_',sprintf('%02d',vv[ii]),'.rda'))


###
# Merge image
library(png)

# example image
img1 <- readPNG('/projectnb/modislc/users/mkmoon/TAscience/trend/figures/map_val_org_02.png')
img2 <- readPNG('/projectnb/modislc/users/mkmoon/TAscience/trend/figures/map_val_org_06.png')
img3 <- readPNG('/projectnb/modislc/users/mkmoon/TAscience/trend/figures/map_val_org_09.png')
img4 <- readPNG('/projectnb/modislc/users/mkmoon/TAscience/trend/figures/map_val_org_16.png')
img5 <- readPNG('/projectnb/modislc/users/mkmoon/TAscience/trend/figures/map_val_org_20.png')
img6 <- readPNG('/projectnb/modislc/users/mkmoon/TAscience/trend/figures/map_val_org_23.png')

# setup plot
setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/figures/')
png(filename=paste0('map_val_org_all.png'),width=13,height=8,units='in',res=300)

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



###
# Box plots
path <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data'
files <- list.files(path,pattern=glob2rx('den*'),full.names=T)
mycol <- brewer.pal(9,'Set1')
for(i in 1:9){
  if(i==1){
    load(files[1]);    dat1 <- val1;    load(files[2]);    dat2 <- val1;    load(files[3]);    dat3 <- val1
    load(files[4]);    dat4 <- val1;    load(files[5]);    dat5 <- val1;    load(files[6]);    dat6 <- val1
  }else if(i==2){
    load(files[1]);    dat1 <- val2;    load(files[2]);    dat2 <- val2;    load(files[3]);    dat3 <- val2
    load(files[4]);    dat4 <- val2;    load(files[5]);    dat5 <- val2;    load(files[6]);    dat6 <- val2
  }else if(i==3){
    load(files[1]);    dat1 <- val3;    load(files[2]);    dat2 <- val3;    load(files[3]);    dat3 <- val3
    load(files[4]);    dat4 <- val3;    load(files[5]);    dat5 <- val3;    load(files[6]);    dat6 <- val3
  }else if(i==4){
    load(files[1]);    dat1 <- val4;    load(files[2]);    dat2 <- val4;    load(files[3]);    dat3 <- val4
    load(files[4]);    dat4 <- val4;    load(files[5]);    dat5 <- val4;    load(files[6]);    dat6 <- val4
  }else if(i==5){
    load(files[1]);    dat1 <- val5;    load(files[2]);    dat2 <- val5;    load(files[3]);    dat3 <- val5
    load(files[4]);    dat4 <- val5;    load(files[5]);    dat5 <- val5;    load(files[6]);    dat6 <- val5
  }else if(i==6){
    load(files[1]);    dat1 <- val6;    load(files[2]);    dat2 <- val6;    load(files[3]);    dat3 <- val6
    load(files[4]);    dat4 <- val6;    load(files[5]);    dat5 <- val6;    load(files[6]);    dat6 <- val6
  }else if(i==7){
    load(files[1]);    dat1 <- val7;    load(files[2]);    dat2 <- val7;    load(files[3]);    dat3 <- val7
    load(files[4]);    dat4 <- val7;    load(files[5]);    dat5 <- val7;    load(files[6]);    dat6 <- val7
  }else if(i==8){
    load(files[1]);    dat1 <- val8;    load(files[2]);    dat2 <- val8;    load(files[3]);    dat3 <- val8
    load(files[4]);    dat4 <- val8;    load(files[5]);    dat5 <- val8;    load(files[6]);    dat6 <- val8
  }else if(i==9){
    load(files[1]);    dat1 <- val9;    load(files[2]);    dat2 <- val9;    load(files[3]);    dat3 <- val9
    load(files[4]);    dat4 <- val9;    load(files[5]);    dat5 <- val9;    load(files[6]);    dat6 <- val9
  }
  
  setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/figures/')
  png(filename=paste0('vari_box_',sprintf('%02d',i),'.png'),width=8,height=4.2,units='in',res=300)
  
  par(fig=c(0,0.5,0,1),oma=c(2,2,0,0),mar=c(4,3,0.5,1),mgp=c(2.5,1,0))
  boxplot(dat1,dat2,dat3,outline=F,ylim=c(-27,27),names=c('MGU','MGD','GSL'),cex.axis=1.3,
          col=mycol[c(3,7,5)])
  abline(h=0,lty=5)
  par(fig=c(0.5,1,0,1),oma=c(2,2,0,0),mar=c(4,3,0.5,1),mgp=c(2.5,1,0),new=T)
  boxplot(dat4,dat5*100,dat6*100,outline=F,ylim=c(-0.12,0.12),names=c('EVImax','GUR','GDR'),cex.axis=1.3,
          col=mycol[c(1,2,4)])
  abline(h=0,lty=5)
  
  dev.off()
  
  print(i)
}



