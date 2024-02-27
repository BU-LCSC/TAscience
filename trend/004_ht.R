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
# # mm <- 2; rr <- 2 # ABoVE
# # mm <- 2; rr <- 6 # New England
# 
# 
# ############################################################
# refReg <- vect('/projectnb/modislc/users/mkmoon/TAscience/trend/data/shp/IPCC-WGI-reference-regions-v4.shp')
# rrName <- unique(refReg$Acronym)
# rName  <- rrName[rr]
# 
# # Load files
# metric <- sprintf('%02d',mm)
# outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/re/vals/',metric)
# files  <- list.files(outDir,pattern=glob2rx('coef_coords_lc*.rda'),full.names=T)
# 
# dat   <- c()
# for(i in 1:315){
#   load(files[i])
#   rSub <- which(e1==rName)
# 
#   if(length(rSub)==1){
#     tmp <- which(e2==(rSub-1))
#     d1 <- coords[tmp,]
#     d2 <- rep(rName,length(tmp))
#     d3 <- mStat[tmp,c(1:8)]
# 
#     d0 <- cbind(d1,d2,d3)
#     d0 <- na.omit(d0)
#     dat <- rbind(dat,d0)
#   }
#   print(i)
# }
# 
# #
# AR_coef <- as.numeric(dat[,5])
# lng     <- as.numeric(dat[,1])
# lat     <- as.numeric(dat[,2])
# refR    <- dat[,3]
# land    <- as.numeric(dat[,11])
# ms1     <- as.numeric(dat[,6])
# ms2     <- as.numeric(dat[,8])
# ms3     <- as.numeric(dat[,9])
# ms4     <- as.numeric(dat[,10])
# # mResi   <- matrix(as.numeric(dat[,c(13:33)]),ncol=ncol(dat[,c(13:33)]))
# 
# datF <- data.frame(AR_coef,lng,lat,refR,land,ms1,ms2,ms3,ms4)
# colnames(datF) <- c('AR_coef','lng','lat','eco','land','pval','mean','median','sd')
# 
# rm(dat,d0,d1,d2,d3,mStat)
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
#     d2 <- mStat[tmp,c(8:29)]
# 
#     d0 <- cbind(d1,d2)
#     d0 <- na.omit(d0)
#     mResi <- rbind(mResi,d0)
#   }
#   print(i)
# }
# 
# mResi <- data.frame(mResi)
# colnames(mResi) <- c('lng','lat','land',rep('resi',21))
# 
# ## Save
# outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/re/vals/by_eco'
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
# mm <- 2; rr <- 9


##
refReg <- vect('/projectnb/modislc/users/mkmoon/TAscience/trend/data/shp/IPCC-WGI-reference-regions-v4.shp')
rrName <- unique(refReg$Acronym)
rName  <- rrName[rr]

print(rName)

#
metric <- sprintf('%02d',mm)
outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/re/vals/by_eco/',metric)
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
  datF$AR_coef_1 <- datF$AR_coef/(datF$mean+300)

  ##
  nlct    <- table(datF$land)
  frqLand <- as.numeric(names(sort(nlct,decreasing=TRUE)[1:3]))

  mDat     <- matrix(NA,4,6)
  mDat[2:4,1] <- frqLand
  mDat[2,2] <- sum(datF$land==frqLand[1])/sum(nlct)*100
  mDat[3,2] <- sum(datF$land==frqLand[2])/sum(nlct)*100
  mDat[4,2] <- sum(datF$land==frqLand[3])/sum(nlct)*100
  mDat[1,1] <- sum(nlct)

  for(j in 1){
    if(j==1){
      subD <- datF[which(datF$land==frqLand[1]|datF$land==frqLand[2]|datF$land==frqLand[3]),]
      subR <- mResi[which(datF$land==frqLand[1]|datF$land==frqLand[2]|datF$land==frqLand[3]),4:24]
    }else{
      subD <- datF[which(datF$land==frqLand[j-1]),]
      subR <- mResi[which(datF$land==frqLand[j-1]),4:24]
    }

    mDat[1,3] <- mean(subD$AR_coef)
    mDat[1,4] <- sd(subD$AR_coef)
    mDat[1,6] <- dim(subD)[1]

    if(dim(subD)[1]>=3000){
      sam <- sample(1:dim(subD)[1],3000)
      SubD <- subD[sam,]
      SubR <- subR[sam,]

      coords <- cbind(SubD$lng,SubD$lat)
      corfit <- fitCor(resids = SubR, coords = coords, covar_FUN = "covar_exp",
                       start = list(range = 0.1), fit.n = 500)
      range.opt = corfit$spcor
      D       <- distm_scaled(coords)
      V.opt   <- covar_exp(D, range.opt)
      GLS.opt <- fitGLS(formula = AR_coef_1 ~ 1 + land, data = SubD, V = V.opt, nugget = NA, no.F = TRUE)
      nug.opt <- GLS.opt$nugget

      pm <- sample_partitions(npix = nrow(subD), partsize = 3000, npart = NA)
      dim(pm)

      if(j==1){
        if(dim(pm)[2]==1){
          mDat[1,5] <- GLS.opt$pval_t[1]
          mDat[1,2] <- GLS.opt$pval_t[2]
        }else{
          partGLS <- fitGLS_partition(formula = AR_coef_1 ~ 1 + land, data = subD,
                                      partmat = pm, covar_FUN = "covar_exp",
                                      covar.pars = list(range = range.opt),
                                      nugget = nug.opt, ncores = 8)
          mDat[1,5] <- partGLS$overall$t.test[7]
          mDat[1,2] <- partGLS$overall$t.test[8]
        }
      }else{
        partGLS <- fitGLS_partition(formula = AR_coef_1 ~ 1, data = subD,
                                    partmat = pm, covar_FUN = "covar_exp",
                                    covar.pars = list(range = range.opt),
                                    nugget = nug.opt, ncores = 8)
        mDat[j,5] <- partGLS$overall$t.test[7]
      }
    }
    print(j)
  }
  mDat <- data.frame(rep(rName,4),mDat)
  colnames(mDat) <- c('RR','LCT','LCT%','mean','sd','pval','npix')

  # Save
  outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/re/parts/',metric)
  if (!dir.exists(outDir)) {dir.create(outDir)}
  # save(mDat,partGLS,file=paste0(outDir,'/spatio_',rName,'.rda'))
  save(mDat,file=paste0(outDir,'/spatio_',rName,'.rda'))
  save(datF,file=paste0(outDir,'/dat_',rName,'.rda'))
}


