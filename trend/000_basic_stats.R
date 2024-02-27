rm(list = ls())

library(terra)
library(sf)

library(RColorBrewer)


############################################################
shp <- vect('/projectnb/modislc/users/mkmoon/TAscience/trend/data/shp/world-administrative-boundaries_edited.shp')
path <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/re/rasters/merge/'

# Metric changes
i <- 2
file <- list.files(path,pattern=glob2rx(paste0('1_merge_trd*',sprintf('%02d',i),'.tif')),full.names=T)
tmp  <- rast(file)
val  <- values(tmp)
median(val,na.rm=T)
sum(!is.na(val))
sum(val<0,na.rm=T)/sum(!is.na(val))*100
sum(val>0,na.rm=T)/sum(!is.na(val))*100
sum(val==0,na.rm=T)/sum(!is.na(val))*100


############################################################
dat <- read.csv('/usr3/graduate/mkmoon/Desktop/mkmoon/TAscience/trend/data/re/parts/stat_pval_300_22yrs_cleaned.csv')

sum((dat$Trend.pval<0.05|dat$Trend.pval.1<0.05|dat$Trend.pval.2<0.05|dat$Trend.pval.3<0.05|dat$Trend.pval.4<0.05|dat$Trend.pval.5<0.05))

sum(dat$Trend.pval<0.05)

sum(dat$Trend.pval.1<0.05)
sum(dat$Trend.pval.1<0.05 & dat$mean.1<0)
sum(dat$Trend.pval.1<0.05 & dat$mean.1>0)

which(dat$mean.1==min(dat$mean.1))


sum(dat$Trend.pval.3<0.05)
sum(dat$Trend.pval.3<0.05 & dat$mean.3<0)
sum(dat$mean.3<0)
sum(dat$mean.3>0)


dat <- read.csv('/usr3/graduate/mkmoon/Desktop/mkmoon/TAscience/trend/data/re/parts/stat_pval_300_22yrs.csv')
unique(dat$X1)


############################################################
mm <- 1
rr <- 1

refReg <- vect('/projectnb/modislc/users/mkmoon/TAscience/trend/data/shp/IPCC-WGI-reference-regions-v4.shp')
rrName <- unique(refReg$Acronym)

Dat <- vector('list',6)
for(mm in 1:6){
  dat1 <- c()
  dat2 <- c()
  for(rr in 1:44){
    rName  <- rrName[rr]
    path  <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/re/att/data'
    fName <- list.files(path,pattern=glob2rx(paste0('*',rName,'_',mm,'.rda')),full.names=T)
    load(fName)
    
    dat1 <- c(dat1,dat[,1])
    dat2 <- c(dat2,ranF$predicted)
  }
  Dat[[mm]] <- cbind(dat1,dat2)
  
  print(mm)
}

library(lmodel2)
for(i in 1:6){
  reg <- lmodel2(Dat[[i]][,2]~Dat[[i]][,1])
  print(reg$rsquare)
}


############################################################
shp <- vect('/projectnb/modislc/users/mkmoon/TAscience/trend/data/shp/world-administrative-boundaries_edited.shp')
refReg <- vect('/projectnb/modislc/users/mkmoon/TAscience/trend/data/shp/IPCC-WGI-reference-regions-v4.shp')
rrName <- unique(refReg$Acronym)

##
setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/figures/')
png(filename=paste0('map_ipcc_ref.png'),width=9.5,height=4,units='in',res=300)

par(oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(0,0,0))
plot(1,xlim=c(-180,180),ylim=c(-55,83),type='n',bty='n',axes=F)
plot(shp,axes=F,add=T,col='grey75',border='grey75')
for(i in 1:44){
  plot(refReg[i],axes=NULL,add=T,lwd=1)
  if(i==6){
    text(-80,
         (ext(refReg[i])[3]+ext(refReg[i])[4])/2,
         rrName[i],font=2)
  }else if(i==29){
    text(100,
         (ext(refReg[i])[3]+ext(refReg[i])[4])/2,
         rrName[i],font=2)
  }else if(i==18){
    text((ext(refReg[i])[1]+ext(refReg[i])[2])/2,
         (ext(refReg[i])[3]+ext(refReg[i])[4])/2-4,
         rrName[i],font=2)
  }else if(i==14){
    text((ext(refReg[i])[1]+ext(refReg[i])[2])/2,
         (ext(refReg[i])[3]+ext(refReg[i])[4])/2,
         rrName[i],font=2,srt=90)
  }else if(i==27){
    text((ext(refReg[i])[1]+ext(refReg[i])[2])/2-6,
         (ext(refReg[i])[3]+ext(refReg[i])[4])/2,
         rrName[i],font=2,srt=90)
  }else if(i==28){
    text((ext(refReg[i])[1]+ext(refReg[i])[2])/2+2,
         (ext(refReg[i])[3]+ext(refReg[i])[4])/2-5,
         rrName[i],font=2)
  }else if(i==16){
    text((ext(refReg[i])[1]+ext(refReg[i])[2])/2,
         (ext(refReg[i])[3]+ext(refReg[i])[4])/2-4,
         rrName[i],font=2)
  }else if(i==43){
    text((ext(refReg[i])[1]+ext(refReg[i])[2])/2,
         (ext(refReg[i])[3]+ext(refReg[i])[4])/2+4,
         rrName[i],font=2)
  }else if(i==26){
    text((ext(refReg[i])[1]+ext(refReg[i])[2])/2,
         (ext(refReg[i])[3]+ext(refReg[i])[4])/2,
         rrName[i],font=2,srt=90)
  }else if(i==10){
    text((ext(refReg[i])[1]+ext(refReg[i])[2])/2,
         (ext(refReg[i])[3]+ext(refReg[i])[4])/2-2,
         rrName[i],font=2,srt=90)
  }else if(i==32){
    text((ext(refReg[i])[1]+ext(refReg[i])[2])/2-6,
         (ext(refReg[i])[3]+ext(refReg[i])[4])/2,
         rrName[i],font=2)
  }else if(i==22){
    text((ext(refReg[i])[1]+ext(refReg[i])[2])/2,
         (ext(refReg[i])[3]+ext(refReg[i])[4])/2+2,
         rrName[i],font=2)
  }else if(i==42){
    text((ext(refReg[i])[1]+ext(refReg[i])[2])/2+3,
         (ext(refReg[i])[3]+ext(refReg[i])[4])/2+2,
         rrName[i],font=2)
  }else if(i==7){
    text((ext(refReg[i])[1]+ext(refReg[i])[2])/2+3,
         (ext(refReg[i])[3]+ext(refReg[i])[4])/2,
         rrName[i],font=2)
  }else if(i==39){
    text((ext(refReg[i])[1]+ext(refReg[i])[2])/2-6,
         (ext(refReg[i])[3]+ext(refReg[i])[4])/2,
         rrName[i],font=2)
  }else{
    text((ext(refReg[i])[1]+ext(refReg[i])[2])/2,
         (ext(refReg[i])[3]+ext(refReg[i])[4])/2,
         rrName[i],font=2)
  }
}
dev.off()