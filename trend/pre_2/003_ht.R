rm(list = ls())

library(terra)
library(sf)

library(remotePARTS)

library(RColorBrewer)


# ############################################################
# # From bash code
# args <- commandArgs()
# print(args)
# 
# mm <- as.numeric(substr(args[3],1,3))
# rr <- as.numeric(substr(args[3],4,6))
# # mm <- 9; rr <- 2 # ABoVE
# # mm <- 2; rr <- 6 # New England
# 
# 
# ############################################################
# refReg <- vect('/projectnb/modislc/users/mkmoon/TAscience/trend/data/shp/IPCC-WGI-reference-regions-v4.shp')
# rrName <- unique(refReg$Acronym)
# rName  <- rrName[rr]
# 
# # Load files
# outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/parts/vals/02'
# files  <- list.files(outDir,pattern=glob2rx('coef_coords_lc*.rda'),full.names=T)
# metric <- sprintf('%02d',mm)
# outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/parts/vals/',metric)
# file2  <- list.files(outDir,pattern=glob2rx('coef_coords_lc*.rda'),full.names=T)
# 
# dat   <- c()
# for(i in 1:315){
#   load(files[i])
#   rSub <- which(e1==rName)
# 
#   if(length(rSub)==1){
#     tmp <- which(e2==(rSub-1))
#     d1 <- coords[tmp,]
#     d2 <- lct_ch[tmp,]
#     d3 <- rep(rName,length(tmp))
# 
#     if(mm==2){
#       d4 <- mStat[tmp,c(1:7)]
#     }else{
#       load(file2[i])
#       d4 <- mStat[tmp,c(1:7)]
#     }
# 
#     d0 <- cbind(d1,d2,d3,d4)
#     d0 <- na.omit(d0)
#     dat <- rbind(dat,d0)
#   }
#   print(i)
# }
# 
# #
# AR_coef <- as.numeric(dat[,7])
# lng     <- as.numeric(dat[,1])
# lat     <- as.numeric(dat[,2])
# refR    <- dat[,5]
# land    <- as.numeric(dat[,3])
# ms1     <- as.numeric(dat[,8])
# ms2     <- as.numeric(dat[,10])
# ms3     <- as.numeric(dat[,11])
# ms4     <- as.numeric(dat[,12])
# # mResi   <- matrix(as.numeric(dat[,c(13:33)]),ncol=ncol(dat[,c(13:33)]))
# 
# datF <- data.frame(AR_coef,lng,lat,refR,land,ms1,ms2,ms3,ms4)
# colnames(datF) <- c('AR_coef','lng','lat','eco','land','pval','mean','median','sd')
# 
# rm(dat,d0,d1,d2,d3,d4,mStat)
# gc(reset=T)
# 
# 
# ##
# mResi <- c()
# for(i in 1:315){
#   load(files[i])
#   rSub <- which(e1==rName)
# 
#   if(length(rSub)==1){
#     tmp <- which(e2==(rSub-1))
#     d1 <- coords[tmp,]
#     d2 <- lct_ch[tmp,]
# 
#     if(mm==2){
#       d4 <- mStat[tmp,c(8:28)]
#     }else{
#       load(file2[i])
#       d4 <- mStat[tmp,c(8:28)]
#     }
# 
#     d0 <- cbind(d1,d2,d4)
#     d0 <- na.omit(d0)
#     mResi <- rbind(mResi,d0)
#   }
#   print(i)
# }
# 
# mResi <- data.frame(mResi)
# colnames(mResi) <- c('lng','lat','land_21','land',rep('resi',21))
# 
# ## Save
# outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/parts/vals/by_eco'
# if (!dir.exists(outDir)) {dir.create(outDir)}
# outDir <- paste0(outDir,'/',metric)
# if (!dir.exists(outDir)) {dir.create(outDir)}
# 
# if(dim(datF)[1]==dim(mResi)[1]){
#   save(datF,file=paste0(outDir,'/dat_',rName,'.rda'))
#   save(mResi,file=paste0(outDir,'/resi_',rName,'.rda'))
# }
# 
# print(dim(datF))
# print(dim(mResi))


############################################################
# From bash code
args <- commandArgs()
print(args)

mm <- as.numeric(substr(args[3],1,3))
rr <- as.numeric(substr(args[3],4,6))
# mm <- 2; rr <- 2

nn <- 3000

#
refReg <- vect('/projectnb/modislc/users/mkmoon/TAscience/trend/data/shp/IPCC-WGI-reference-regions-v4.shp')
rrName <- unique(refReg$Acronym)
rName  <- rrName[rr]

print(rName)

#
metric <- sprintf('%02d',mm)
outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/pre/rasters/parts/vals/by_eco/',metric)
file1  <- list.files(outDir,pattern=glob2rx(paste0('dat_',rName,'.rda')),full.names=T)
file2  <- list.files(outDir,pattern=glob2rx(paste0('resi_',rName,'.rda')),full.names=T)
outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/pre/rasters/parts/vals/by_eco/lct/'
file3  <- list.files(outDir,pattern=glob2rx(paste0('lct_',rName,'.rda')),full.names=T)
load(file1)
load(file2)
load(file3)
lct <- unlist(lct)
lct[lct==5] <- 4
lct[lct==7] <- 6
lct[lct==9|lct==10] <- 8
nlct <- table(lct)

print(dim(datF))
print(dim(mResi))

if(dim(datF)[1]==dim(mResi)[1]){
  datF$land[datF$land==5] <- 4
  datF$land[datF$land==7] <- 6
  datF$land[datF$land==9|datF$land==10] <- 8

  # datF$AR_coef_1 <- datF$AR_coef/datF$mean
  datF$AR_coef_1 <- datF$AR_coef/(datF$mean)

  ##
  frqLand <- as.numeric(names(sort(nlct,decreasing=TRUE)[1:3]))

  mDat     <- matrix(NA,4,6)
  mDat[2:4,1] <- frqLand
  mDat[2,2] <- sum(lct==frqLand[1])/sum(nlct)*100
  mDat[3,2] <- sum(lct==frqLand[2])/sum(nlct)*100
  mDat[4,2] <- sum(lct==frqLand[3])/sum(nlct)*100

  for(j in 1:4){
    if(j==1){
      subD <- datF[,]
      subR <- mResi[,5:25]
    }else{
      subD <- datF[which(datF$land==frqLand[j-1]),]
      subR <- mResi[which(datF$land==frqLand[j-1]),5:25]
    }
    mDat[j,3] <- mean(subD$AR_coef)
    mDat[j,4] <- sd(subD$AR_coef)
    mDat[j,6] <- dim(subD)[1]

    if(dim(subD)[1]>=nn){
      for(i in 1:10){
        try({
          sam <- sample(1:dim(subD)[1],nn)
          SubD <- subD[sam,]
          SubR <- subR[sam,]
          
          coords <- cbind(SubD$lng,SubD$lat)
          corfit <- fitCor(resids = SubR, coords = coords, covar_FUN = "covar_exp",
                           start = list(range = 0.1), fit.n = nn)
          range.opt = corfit$spcor
          D       <- distm_scaled(coords)
          V.opt   <- covar_exp(D, range.opt)
          
          GLS.opt <- fitGLS(formula = AR_coef_1 ~ 1, data = SubD, V = V.opt, nugget = NA, no.F = TRUE)
          mDat[j,5] <- GLS.opt$pval_t
          },silent=T)
        if (!is.na(mDat[j,5])) {
          break
        }
      }
    }
    print(j)
  }
  mDat <- data.frame(rep(rName,4),mDat)
  colnames(mDat) <- c('RR','LCT','LCT%','mean','sd','pval','npix')

  # Save
  outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/parts/plus300/',metric)
  if (!dir.exists(outDir)) {dir.create(outDir)}
  save(mDat,file=paste0(outDir,'/spatio_',rName,'.rda'))
  save(datF,file=paste0(outDir,'/dat_',rName,'.rda'))
}


# nug.opt <- GLS.opt$nugget
# GLS.int <- fitGLS(AR_coef ~ 1, data = SubD1, V = V.opt, nugget = nug.opt, no.F = TRUE)

# pm <- sample_partitions(npix=nrow(SubD1),partsize=1000,npart=NA)
# partGLS.trd <- fitGLS_partition(formula = AR_coef ~ 1, data = SubD1,
#                                 partmat = pm, covar_FUN = "covar_exp",
#                                 covar.pars = list(range = range.opt),
#                                 nugget = NA)
# save(partGLS.trd,file =paste0(outDir,'/partGLS_trd.rda'))

# path  <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/parts/'
# files <- list.files(path,pattern=glob2rx('spa*'),full.names=T)
#
# load(files[i])
# mDat



