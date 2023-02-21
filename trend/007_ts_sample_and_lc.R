rm(list = ls())

library(gdalUtils)
library(sp)
library(raster)

# library(doMC)
# library(doParallel)


############################################################
# files
path <- '/projectnb/modislc/projects/sat/data/mcd12q/q1/2021'
LCfiles <- list.files(path,pattern=glob2rx('*.hdf'),full.names=T)

path <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_nor/clust_2pc/map_clust'
CLfiles <- list.files(path,pattern=glob2rx('*.tif'),full.names=T)


##
Dat <- vector('list',315)
for(tt in 1:315){
  sds <- get_subdatasets(LCfiles[tt])
  lct <- raster(sds[1])
  clt <- raster(CLfiles[tt])
  
  datMat <- cbind(values(clt),values(lct))
  datTable <- matrix(NA,9,17)
  for(i in 1:9){
    tmp <- datMat[which(datMat[,1]==i),]
    if(length(tmp)>3){
      for(j in 1:17){
        datTable[i,j] <- length(which(tmp[,2]==j))
      }  
    }else{
      datTable[i,] <- 0
    }
  }
  
  Dat[[tt]] <- datTable
  
  print(tt)  
}

path <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data'
save(Dat,file=paste0(path,'/lct_by_cl.rda'))
  

############################################################
load('/projectnb/modislc/users/mkmoon/TAscience/trend/data/lct_by_cl.rda')

datTable <- matrix(NA,9,17)
for(i in 1:9){
  for(j in 1:315){
    if(j==1){
      tmp <- Dat[[j]][i,]  
    }else{
      tmp <- tmp + Dat[[j]][i,]
    }
  }
  datTable[i,] <- tmp
}
datLCT <- matrix(NA,9,17)
for(i in 1:9){
  tmp <- datTable[i,]
  tSum <- sum(tmp)
  tDat <- tmp/tSum*100
  datLCT[i,] <- tDat
}



#
setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/figures/')
mycol <- c('#218a21','#31cd31','#9acd31','#97fa97','#8fbb8f','#bb8f8f','#f5deb3','#dbeb9d','#e1d600','#efb766','#4682b2','#faed9b','#ff0000','#999355','#f5f5dc','#bdbdbd','#87cbff')
numLCT <- c('ENF','EBF','DNF','DBF','MF','CS','OS','WS','SA','GR','PW','CR','UR','CNV','PI','BA','WA')

for(i in 1:9){
  png(filename=paste0('lct_hist_',sprintf('%02d',i),'.png'),width=8.2,height=6,units='in',res=300,bg=NA)
  par(mfrow=c(1,1),oma=c(1,1,0,0),mar=c(4,4,1,1),mgp=c(2.5,1,0))
  barplot(datLCT[i,order(datLCT[i,],decreasing=T)[1:9]],col=mycol[order(datLCT[i,],decreasing=T)[1:9]],
          ylim=c(0,45),ylab='Portion of land cover (%)',names.arg=numLCT[order(datLCT[i,],decreasing=T)[1:9]],
          cex.axis=1.5,cex.lab=1.5,cex.names=1.5) 
  dev.off()
}

#
setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/figures/')
png(filename=paste0('lct_hist_all.png'),width=10,height=8,units='in',res=300)
par(mfrow=c(3,3),oma=c(1,1,0,0),mar=c(4,4,1,1),mgp=c(2.5,1,0))
for(i in c(2,3,7,5,1,9,4,8,6)){
  barplot(datLCT[i,order(datLCT[i,],decreasing=T)[1:7]],col=mycol[order(datLCT[i,],decreasing=T)[1:7]],
          ylim=c(0,45),ylab='Portion of land cover (%)',names.arg=numLCT[order(datLCT[i,],decreasing=T)[1:7]],
          cex.axis=1.3,cex.lab=1.3)  
}
dev.off()


