rm(list = ls())

library(terra)
library(sf)

library(RColorBrewer)
library(scales)

library(mclust)


############################################################
shp <- vect('/projectnb/modislc/users/mkmoon/TAscience/trend/data/shp/IPCC-WGI-reference-regions-v4.shp')
rNaFull <- shp$Name
rNaAcym <- shp$Acronym

rChg <- vector('list',25)
for(i in 1:25){
  rChg[[i]] <- rast(paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_sig/merge/1_merge_',sprintf('%02d',i),'.tif'))
}

dat <- matrix(NA,length(rNaFull),125)
for(i in 1:length(rNaFull)){
  sSub <- subset(shp,shp$Name==rNaFull[i])
  
  for(j in 1:25){
    rSub <- crop(rChg[[j]],sSub)  
    val  <- na.omit(values(rSub))
    dat[i,(((j-1)*5+1):(j*5))] <- c(mean(val),sd(val),quantile(val)[2:4])
  }
  print(i)
}

## 
pcp    <- prcomp(dat,center = T,scale. = T)
pcpDat <- cbind(as.numeric(pcp$x[,1]),as.numeric(pcp$x[,2]),as.numeric(pcp$x[,3]),as.numeric(pcp$x[,4]),as.numeric(pcp$x[,5]),as.numeric(pcp$x[,6]))

var_explained <- pcp$sdev^2 / sum(pcp$sdev^2)
plot(round(var_explained*100,1))
sum(var_explained[1:6])
plot(pcp$rotation[,1])

##
X   <- pcpDat
BIC <- mclustBIC(X)
plot(BIC)
summary(BIC)

mod1 <- Mclust(X, x = BIC)
summary(mod1, parameters = TRUE)

prMC <- predict(mod1,pcpDat)


############################################################
shp1 <- vect('/projectnb/modislc/users/mkmoon/TAscience/trend/data/shp/world-administrative-boundaries_edited.shp')


##
setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/figures/')
png(filename=paste0('map_ipcc_clust.png'),width=14,height=6,units='in',res=300)

par(oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0))
# plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
plot(shp,col=alpha(prMC$classification,0.5),border='gray75',axes=F)
plot(shp1,add=T)

dev.off()


##
setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/figures/')
png(filename=paste0('map_ipcc_clust_eg.png'),width=14,height=6,units='in',res=300)

par(oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0))
plot(shp,border='gray75',axes=F)

mycol <- rev(brewer.pal(11,'PiYG'))
Pal   <- colorRampPalette(mycol)
mycol <- Pal(300)

plot(shp1,col='grey75',axes=F,add=T)
r1 <- focal(rChg[[2]],3,fun='median',na.rm=T)
r1[r1 < -15] <- -15
r1[r1 >  15] <-  15
plot(r1,add=T,col=mycol,axes=F,legend=F)
plot(shp,add=T,lwd=2)
plot(shp1,border='gray55',add=T)

dev.off()


############################################################
setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/figures/')
png(filename=paste0('box_by_refR.png'),width=10,height=6,units='in',res=300)

par(mfrow=c(2,3),oma=c(2,2,0,0),mar=c(4,4,2,2))

vv <- c(2,6,9,16,20,23)
for(h in 1:6){
  dat <- vector('list',3)
  for(i in 1:3){
    dd <- which(prMC$classification == i)
    
    tDat <- vector('list',length(dd))
    for(j in 1:length(dd)){
      sSub <- subset(shp,shp$Name==rNaFull[dd[j]])  
      
      rSub <- crop(rChg[[vv[h]]],sSub)  
      tDat[[j]] <- na.omit(values(rSub))
    }
    dat[[i]] <- unlist(tDat)
  }
  boxplot(dat,outline=F,cex.axis=1.5,col=alpha(c(1,2,3),0.5))
  abline(h=0)
  print(h)
}

dev.off()



############################################################
library(ppcor)

dd <- which(prMC$classification == 2)

vv <- 9
for(i in 1:15){
  sSub <- subset(shp,shp$Name==rNaFull[dd[i]])
  
  r1 <- rast(paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_sig/merge/1_merge_',sprintf('%02d',vv),'.tif'))
  vr1 <- values(crop(r1,sSub))
  for(j in 1:10){
    rt  <- rast(paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_sig/climate/1_chg_cli_dif_',sprintf('%02d',j),'.tif'))
    vr2 <- values(crop(rt,sSub))
    vr1 <- cbind(vr1,vr2)
    # vr1 <- na.omit(vr1)
  }
  if(i==1){
    mv1 <- vr1 
  }else{
    mv1 <- rbind(mv1,vr1) 
  }
  print(i)
}

cleaned_data <- na.omit(mv1)
cleaned_data <- as.data.frame(cleaned_data)
paStat <- matrix(NA,10,2)
for(i in 2:11){
  pat <- pcor.test(cleaned_data[,1],cleaned_data[,i],cleaned_data[,-c(1,i)],method="pearson")
  paStat[i-1,1] <- pat$estimate
  paStat[i-1,2] <- pat$p.value
}
print(paStat)
