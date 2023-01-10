rm(list = ls())

library(gdalUtils)
library(sp)
library(raster)

# library(doMC)
# library(doParallel)


# ############################################################
# # From bash code
# args <- commandArgs()
# print(args)
# 
# tile <-            substr(args[3],1,6)
# cc   <- as.numeric(substr(args[3],7,7))
# # tile <- 'h12v04'; cc <- 2
# 
# print(tile)
# print(cc)
# 
# 
# ############################################################
# # NBAR files
# path <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/nbar/',tile,'/',sprintf('%02d',cc))
# files <- list.files(path,pattern=glob2rx('evi*.rda'),full.names=T)
# 
# print(files)
# 
# ###
# if(length(files)==14){
#   ## Before
#   Dat <- vector('list',7)
#   for(i in 1:7){
#     load(files[i])
# 
#     if(cc==1){    dat <- dat1
#     }else if(cc==2){    dat <- dat2
#     }else if(cc==3){    dat <- dat3
#     }else if(cc==4){    dat <- dat4
#     }else if(cc==5){    dat <- dat5
#     }else if(cc==6){    dat <- dat6
#     }else if(cc==7){    dat <- dat7
#     }else if(cc==8){    dat <- dat8
#     }else          {    dat <- dat9
#     }
# 
#     if(i==1){
#       # if(dim(dat)[1]>30000){
#       #   sam <- sample(1:dim(dat)[1],30000)
#       # }else{
#       #   sam <- sample(1:dim(dat)[1],dim(dat)[1])
#       # }
#       if(cc==6){
#         sam <- sample(1:dim(dat)[1],round(dim(dat)[1])*0.04)  
#       }else{
#         sam <- sample(1:dim(dat)[1],round(dim(dat)[1])*0.02)
#       }
#     }
#     Dat[[i]] <- dat[sam,]
# 
#     print(i)
#   }
#   print(length(sam))
# 
#   dat11 <- matrix(NA,length(sam),365)
#   for(i in 1:length(sam)){
#     t1 <- Dat[[1]][i,];  t2 <- Dat[[2]][i,];  t3 <- Dat[[3]][i,];  t4 <- Dat[[4]][i,]
#     t5 <- Dat[[5]][i,];  t6 <- Dat[[6]][i,];  t7 <- Dat[[7]][i,]
# 
#     ts      <- rbind(t1,t2,t3,t4,t5,t6,t7)
#     dat11[i,] <- apply(ts,2,median,na.rm=T)
# 
#     if(i%%100==0) print(i)
#   }
# 
# 
#   ## After
#   Dat <- vector('list',7)
#   for(i in 1:7){
#     load(files[i+7])
# 
#     if(cc==1){    dat <- dat1
#     }else if(cc==2){    dat <- dat2
#     }else if(cc==3){    dat <- dat3
#     }else if(cc==4){    dat <- dat4
#     }else if(cc==5){    dat <- dat5
#     }else if(cc==6){    dat <- dat6
#     }else if(cc==7){    dat <- dat7
#     }else if(cc==8){    dat <- dat8
#     }else          {    dat <- dat9
#     }
# 
#     Dat[[i]] <- dat[sam,]
# 
#     print(i)
#   }
# 
#   dat22 <- matrix(NA,length(sam),365)
#   for(i in 1:length(sam)){
#     t1 <- Dat[[1]][i,];  t2 <- Dat[[2]][i,];  t3 <- Dat[[3]][i,];  t4 <- Dat[[4]][i,]
#     t5 <- Dat[[5]][i,];  t6 <- Dat[[6]][i,];  t7 <- Dat[[7]][i,]
# 
#     ts      <- rbind(t1,t2,t3,t4,t5,t6,t7)
#     dat22[i,] <- apply(ts,2,median,na.rm=T)
# 
#     if(i%%100==0) print(i)
#   }
# 
#   # Save files
#   if(dim(dat11)[1]>100 & dim(dat11)[1]==dim(dat22)[1] & dim(dat11)[2]==dim(dat22)[2]){
#     outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/nbar/bfoaft/',sprintf('%02d',cc))
#     if (!dir.exists(outDir)) {dir.create(outDir)}
#     save(dat11,file=paste0(outDir,'/ts_1_',tile,'.rda'))
#     save(dat22,file=paste0(outDir,'/ts_2_',tile,'.rda'))
# 
#     print('Done!')
#   }
# }



############################################################
# From bash code
args <- commandArgs()
print(args)

cc <- as.numeric(args[3])
# cc <- 6

print(cc)


#---------------------------------------------------------------------
CheckSpike <- function(vi){

  vi_og <- vi # preserve original vector
  good  <- !is.na(vi_og)

  x_outs <- matrix(F, length(vi_og)) # create the outlier output vector
  count <- 0
  while (count < 50) {
    count <- count + 1

    eS <- vi_og[good]
    dS <- c(1:365)[good] #subset date vector

    ind1 <- 1:(length(dS)-2)  #Get indices for first, second, and third images
    ind2 <- 2:(length(dS)-1)
    ind3 <- 3:length(dS)

    dayFrac <- (dS[ind2]-dS[ind1]) / (dS[ind3]-dS[ind1])   #Calculate time fraction of date 1 to 2 compared to date 1 to 3
    fitVal <- eS[ind1] + (eS[ind3] - eS[ind1]) * dayFrac   #Calculate value at point 2 if a straight line is drawn from point 1 to 3.
    dev1 <- eS[ind2] - eS[ind1]
    dev2 <- eS[ind2] - eS[ind3]
    dev <- fitVal - eS[ind2]
    devRatio <- dev / (eS[ind3] - eS[ind1])
    dDiff <- dS[ind3] - dS[ind1]

    #look for negative spikes in vi
    eTest <- (dev > 0.1) & (abs(devRatio) > 2) & (dDiff < 40)

    ##
    check <- eTest
    check <- c(FALSE,check,FALSE)
    check[is.na(check) | is.infinite(check)] <- FALSE
    if (sum(check) == 0) {break}    #Break if no observations have been despiked
    x_outs[good] <- check   #expand to size of original x, accounting for missing values
    good[x_outs] <- FALSE              #remove the despiked values from the pixels of interest and try again
  }
  return(x_outs)
}


###
outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/nbar/bfoaft/'
for(pp in 1:2){
  path <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/nbar/bfoaft/',sprintf('%02d',cc))
  files <- list.files(path,pattern=glob2rx(paste0('ts_',pp,'*')),full.names=T)

  # print(files)

  Dat <- c()
  for(i in 1:length(files)){
    load(files[i])

    if(pp==1){  dat <- dat11
    }else{      dat <- dat22}

    for(j in 1:dim(dat)[1]){
      log <- try({
        vi <- dat[j,]

        # Remove spikes
        spikes <- CheckSpike(vi)
        vi[spikes] <- NA
        # Remove negative VI
        vi[vi < 0] <- NA
        # weight and dormant value
        vi_dorm <- quantile(vi,0.1,na.rm=T)
        wt               <- rep(1,365)
        wt[is.na(vi)]    <- 0.5
        wt[vi < vi_dorm] <- 0.5
        vi[vi < vi_dorm] <- vi_dorm

        # Spline
        spl <- smooth.spline(c(1:365)[!is.na(vi)],vi[!is.na(vi)],spar=0.55,w=wt[!is.na(vi)])
        Smooth <- predict(spl,1:365)

        dat[j,] <- Smooth$y
      })

      if(inherits(log, "try-error")){
        dat[j,] <- rep(NA,365)
      }

    }

    Dat <- rbind(Dat,dat)
    print(i)
  }

  print(dim(Dat))

  if(pp==1){
    dat1 <- Dat
    save(dat1,file=paste0(outDir,'/raw_ts_',pp,'_',cc,'.rda'))
    t1  <- apply(Dat,2,median,na.rm=T)
    t12 <- apply(Dat,2,mean,na.rm=T)
    save(t1,t12,file=paste0(outDir,'/ts_',pp,'_',cc,'.rda'))
  }else{
    dat2 <- Dat
    save(dat2,file=paste0(outDir,'/raw_ts_',pp,'_',cc,'.rda'))
    t2  <- apply(Dat,2,median,na.rm=T)
    t22 <- apply(Dat,2,mean,na.rm=T)
    save(t2,t22,file=paste0(outDir,'/ts_',pp,'_',cc,'.rda'))
  }
}


############################################################
#
path <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/nbar/bfoaft'
files <- list.files(path,pattern=glob2rx('raw_*'),full.names=T)

setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/figures/')
#
for(i in 1:9){
  load(files[i])
  load(files[i+9])

  png(filename=paste0('resi_',sprintf('%02d',i),'.png'),width=8,height=3.5,units='in',res=300)
  par(oma=c(1,1,0,0),mar=c(4,4,1,1),mgp=c(2.5,1,0))

  dat  <- dat2 - dat1
  resi <- apply(dat,2,median,na.rm=T)
  # resi <- apply(dat,2,mean,na.rm=T)
  plot(resi,ylim=c(-0.01,0.04),type='l',xlim=c(0,365),axes=F,
       xlab='Day of year',ylab='Residual',cex.lab=1.5,lwd=3)
  axis(1,at=c(1,50,100,150,200,250,300,350),cex.axis=1.3)
  axis(2,at=c(-0.02,0,0.02,0.04),cex.axis=1.3)
  box()
  abline(h=0,lty=5)

  print(i)

  dev.off()
}


par(mfrow=c(3,3),oma=c(1,1,0,0),mar=c(4,4,1,1),mgp=c(2.5,1,0))
for(i in c(2,3,7,5,1,9,4,8,6)){
  load(files[i])
  load(files[i+9])

  dat  <- dat2 - dat1
  resi <- apply(dat,2,median,na.rm=T)
  plot(resi,ylim=c(-0.015,0.045),type='l',xlim=c(0,365),axes=F,
       xlab='Day of year',ylab='Residual',cex.lab=1.5,lwd=3)
  axis(1,at=c(1,50,100,150,200,250,300,350),cex.axis=1.3)
  axis(2,at=c(-0.015,0,0.015,0.03,0.045),cex.axis=1.3)
  box()
  abline(h=0,lty=5)

  print(i)
}


############################################################
#
path <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/nbar/bfoaft'
files <- list.files(path,pattern=glob2rx('ts_*'),full.names=T)

setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/figures/')
mycol <- c('#ccb879ff','#68ab5fff','#1c5f2cff','#dcd939ff','#7584c9ff','#ff7f00ff','#ed79e4ff','#e31a1cff','#ab6c28ff')

#
for(cc in 1:9){
  png(filename=paste0('ts_',sprintf('%02d',cc),'.png'),width=8,height=6,units='in',res=300)
  par(oma=c(1,1,0,0),mar=c(4,4,1,1),mgp=c(2.5,1,0))

  load(files[cc  ])
  load(files[cc+9])

  plot(t1,type='l',xlim=c(0,365),ylim=c(0.1,0.6),axes=F,
       xlab='Day of year',ylab='EVI2',cex.lab=1.5,lwd=3)
  axis(1,at=c(1,50,100,150,200,250,300,350),cex.axis=1.3)
  axis(2,at=c(0.1,0.2,0.3,0.4,0.5,0.6),cex.axis=1.3)
  box()
  lines(t2,col=mycol[cc],lwd=3)
  legend('topleft',c(sprintf('%02d',cc),'2001-2007','2015-2021'),cex=1.5,bty='n',
         lty=c(NA,1,1),lwd=c(NA,3.5,3.5),col=c(NA,'black',mycol[cc]))

  dev.off()

  print(cc)
}



#
png(filename=paste0('ts_all.png'),width=10,height=8,units='in',res=300)

par(mfrow=c(3,3),oma=c(1,1,0,0),mar=c(4,4,1,1),mgp=c(2.5,1,0))
for(cc in c(2,3,7,5,1,9,4,8,6)){
  load(files[cc  ])
  load(files[cc+9])

  plot(t1,type='l',xlim=c(0,365),ylim=c(0.1,0.6),axes=F,
       xlab='Day of year',ylab='EVI2',cex.lab=1.5,lwd=1.5)
  axis(1,at=c(1,50,100,150,200,250,300,350),cex.axis=1.3)
  axis(2,at=c(0.1,0.2,0.3,0.4,0.5,0.6),cex.axis=1.3)
  box()
  lines(t2,col=mycol[cc],lwd=1.5)
  legend('topleft',c(sprintf('%02d',cc),'2001-2007','2015-2021'),cex=1.5,bty='n',
         lty=c(NA,1,1),lwd=c(NA,1.5,1.5),col=c(NA,'black',mycol[cc]))

  print(cc)
}

dev.off()



