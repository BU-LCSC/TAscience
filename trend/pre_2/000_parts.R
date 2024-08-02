rm(list = ls())

library(terra)
library(sf)

library(Rcpp)
library(RcppRoll)

library(zyp)
library(Kendall)

library(remotePARTS)


############################################################
# From bash code
args <- commandArgs()
print(args)

tt <- as.numeric(substr(args[3],1,3))
mm <- as.numeric(substr(args[3],4,6))
cc <- as.numeric(substr(args[3],7,9))
# tt <- 69; mm <- 1; cc <- 1


############################################################
# Get tile list
mcd12q2_path <- '/projectnb/modislc/projects/sat/data/mcd12q/q2/c61'
file <- list.files(path=paste0(mcd12q2_path,'/2001'),full.names=T)
tile_list <- substr(file,74,79)

mcd12q1_path <- '/projectnb/modislc/projects/sat/data/mcd12q/q1'


## panel grid
sds <- unlist(gdal_subdatasets(file[tt]))
imgBase <- rast(sds[1])

cxx <- 20
cyy <- 20
panel_grid <- matrix(1:length(imgBase),dim(imgBase)[1]/cxx,dim(imgBase)[2]/cyy)
panel_grid <- rast(panel_grid,crs=crs(imgBase),extent=ext(imgBase))


############################################################
outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/parts/',tile_list[tt])
valFile <- paste0(outDir,'/vals.rda')

if(file.exists(valFile)){
  load(valFile)
}else{
  # Load metrics
  dat1 <- matrix(NA,(120*120),21)
  dat2 <- matrix(NA,(120*120),21)
  years <- 2001:2021
  for(yy in 1:21){
    path <- paste0(mcd12q2_path,'/',years[yy])
    sstr <- paste0('*',tile_list[tt],'*')
    file <- list.files(path=path,pattern=glob2rx(sstr),full.names=T)
    sds  <- unlist(gdal_subdatasets(file))
    
    rQA <- rast(sds[12],lyrs=1)
    doy_offset <- as.integer(as.Date(paste(years[yy], "-1-1", sep="")) - as.Date("1970-1-1"))
    
    # DOY metrics
    if(mm==1)      {r1 <- rast(sds[ 3],lyrs=1) - doy_offset               # MidGreenup
    }else if(mm==2){r1 <- rast(sds[ 7],lyrs=1) - doy_offset               # MidGreendown
    }else if(mm==3){r1 <- rast(sds[ 7],lyrs=1) -  rast(sds[ 3],lyrs=1)    # GSL
    # EVI metrics
    }else if(mm==4){r1 <- rast(sds[ 9],lyrs=1) +  rast(sds[10],lyrs=1)}   # EVI_Max
    
    # Only best or good
    r1[rQA!=0 & rQA!=1] <- NA
    
    # LCT
    path <- paste0(mcd12q1_path,'/',years[yy],'.01.01')
    sstr <- paste0('*',tile_list[tt],'*')
    file <- list.files(path=path,pattern=glob2rx(sstr),full.names=T)
    sds  <- unlist(gdal_subdatasets(file))
    r2  <- rast(sds[1])
    
    #
    datP    <- matrix(NA,(ncol(panel_grid)*nrow(panel_grid)),1)
    datL    <- matrix(NA,(ncol(panel_grid)*nrow(panel_grid)),1);  ii <- 1
    for(i in 1:dim(panel_grid)[1]){
      for(j in 1:dim(panel_grid)[2]){
        # Phenometric
        tmp1 <- unlist(r1[(cxx*(i-1)+1):(cxx*i),(cyy*(j-1)+1):(cyy*j)])
        if(sum(!is.na(tmp1))>=8){
          datP[ii] <- median(tmp1,na.rm=T)
        }
        # LCT
        tmp2 <- unlist(r2[(cxx*(i-1)+1):(cxx*i),(cyy*(j-1)+1):(cyy*j)])
        tmp3 <- data.frame(table(tmp2))
        if(max(tmp3$Freq)>199){
          datL[ii] <- as.numeric(as.character(tmp3$tmp2[which(tmp3$Freq==max(tmp3$Freq))]))
        }
        ii <- ii + 1
      }
    }
    dat1[,yy] <- datP
    dat2[,yy] <- datL
    
    print(years[yy])
  }
  
  if (!dir.exists(outDir)) {dir.create(outDir)}
  save(dat1,file=paste0(outDir,'/vals.rda'))
}


############################################################
cCase <- complete.cases(dat1)
pCase <- c(1:(120*120))[cCase]

##
valFile <- paste0(outDir,'/trends.rda')

if(file.exists(valFile)){
  load(valFile)
}else{
  ts <- 1:21
  mStat <- matrix(NA,(120*120),13)
  mResi <- matrix(NA,(120*120),21)
  for(i in pCase){
    ty <- dat1[i,]
    if(sum(!is.na(ty))==21){
      m1 <- summary(lm(ty ~ ts))
      m2 <- zyp.sen(ty ~ ts)
      m2p <- MannKendall(ty)
      m3 <- fitAR(ty ~ ts)
        
      mStat[i,] <- c(mean(ty),median(ty),sd(ty),(median(ty[15:21])-median(ty[1:7])),
                     m1$coefficients[c(1,2,8)],
                     m2$coefficients,m2p$sl[1],
                     m3$coefficients,m3$pval[2])
      mResi[i,] <- m3$residuals
    }
    if(i%%1000==0) print(i)
  }
  save(mStat,mResi,file=paste0(outDir,'/trends.rda'))
}



############################################################
# library(RColorBrewer)
# mycol <- rev(brewer.pal(11,'Spectral'))
# Pal   <- colorRampPalette(mycol)
# mycol <- Pal(300)
# 
# r0 <- setValues(panel_grid,mStat[,1])
r1 <- setValues(panel_grid,mStat[,3])
# r2 <- setValues(panel_grid,mStat[,4])
# r3 <- setValues(panel_grid,mStat[,6])
# r4 <- setValues(panel_grid,mStat[,9])
# r5 <- setValues(panel_grid,mStat[,12])
# 
# r6 <- setValues(panel_grid,mStat[,7])
# r7 <- setValues(panel_grid,mStat[,10])
# r8 <- setValues(panel_grid,mStat[,13])
# 
# ##
# setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/figures/')
# png(filename=paste0('comp_parts.png'),width=11,height=7,units='in',res=300)
# 
# par(mfrow=c(3,3),oma=c(1,1,0,0))
# plot(r0,main='Mean MGU',cex.main=1.5,smooth=T,colNA='gray65')
# plot(r1,range=c(0,10),main='SD',cex.main=1.5,smooth=T,colNA='gray65')
# plot(r2/14,range=c(-0.8,0.8),main='Diff. (days/yr)',cex.main=1.5,col=mycol,smooth=T,colNA='gray65')
# plot(r3,range=c(-0.8,0.8),main='LS (days/yr)',cex.main=1.5,col=mycol,smooth=T,colNA='gray65')
# plot(r4,range=c(-0.8,0.8),main='ThS (days/yr)',cex.main=1.5,col=mycol,smooth=T,colNA='gray65')
# plot(r5,range=c(-0.8,0.8),main='PARTS (days/yr)',cex.main=1.5,col=mycol,smooth=T,colNA='gray65')
# hist(r3,breaks=seq(-5,5,0.01),xlim=c(-0.5,0.5),ylim=c(0,500),main='Hist. LS',cex.main=1.5)
# abline(v=0,lwd=2)
# abline(v=mean(values(r3,na.rm=T)),lwd=2,col='red')
# hist(r4,breaks=seq(-5,5,0.01),xlim=c(-0.5,0.5),ylim=c(0,500),main='Hist. ThS',cex.main=1.5)
# abline(v=0,lwd=2)
# abline(v=mean(values(r4,na.rm=T)),lwd=2,col='red')
# hist(r5,breaks=seq(-5,5,0.01),xlim=c(-0.5,0.5),ylim=c(0,500),main='Hist. PARTS',cex.main=1.5)
# abline(v=0,lwd=2)
# abline(v=mean(values(r5,na.rm=T)),lwd=2,col='red')
# 
# dev.off()
# 
# ##
# setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/figures/')
# png(filename=paste0('comp_parts_1.png'),width=11,height=7,units='in',res=300)
# 
# par(mfrow=c(3,3),oma=c(1,1,0,0))
# plot(r3,range=c(-0.8,0.8),main='LS (days/yr)',cex.main=1.5,col=mycol,smooth=T,colNA='gray65')
# plot(r4,range=c(-0.8,0.8),main='ThS (days/yr)',cex.main=1.5,col=mycol,smooth=T,colNA='gray65')
# plot(r5,range=c(-0.8,0.8),main='PARTS (days/yr)',cex.main=1.5,col=mycol,smooth=T,colNA='gray65')
# plot(r6,main='LS p-val',cex.main=1.5,smooth=T,colNA='gray65')
# plot(r7,main='ThS p-val',cex.main=1.5,smooth=T,colNA='gray65')
# plot(r8,main='PARTS p-val',cex.main=1.5,smooth=T,colNA='gray65')
# hist(r6,breaks=seq(0,1,0.01),xlim=c(0,1),ylim=c(0,500),main='Hist. LS',cex.main=1.5)
# abline(v=mean(values(r6,na.rm=T)),lwd=2,col='red')
# hist(r7,breaks=seq(0,1,0.01),xlim=c(0,1),ylim=c(0,500),main='Hist. ThS',cex.main=1.5)
# abline(v=mean(values(r7,na.rm=T)),lwd=2,col='red')
# hist(r8,breaks=seq(0,1,0.01),xlim=c(0,1),ylim=c(0,500),main='Hist. PARTS',cex.main=1.5)
# abline(v=mean(values(r8,na.rm=T)),lwd=2,col='red')
# 
# dev.off()


##
coords  <- crds(r1,na.rm=F)
shp <- vect(coords,crs=crs(r1))
shp <- project(shp,'EPSG:4326')
coords <- crds(shp)

ARfit <- fitAR_map(dat1[cCase,],coords[cCase,])
AR_coef <- ARfit$coefficients[,"t"]/rowMeans(dat1[cCase,])

#
ecoR <- vect('/projectnb/modislc/users/mkmoon/NEphenology/ecoregions/na_cec_eco_l2/NA_CEC_Eco_Level2.shp')
ecoR <- project(ecoR,r1)
ecoR <- crop(ecoR,r1)
s1   <- rasterize(ecoR,r1,'NA_L1CODE')
s1[s1==0] <- NA
valEco <- values(s1)
datF <- data.frame(AR_coef,ARfit$coords,valEco[cCase,])
colnames(datF) <- c('AR_coef','lng','lat','eco')

corfit <- fitCor(resids = mResi, coords = coords, covar_FUN = "covar_exp",
                 start = list(range = 0.1), fit.n = dim(datF)[1])
range.opt = corfit$spcor

# D <- distm_scaled(coords[cCase,])
# V.opt <- covar_exp(D, range.opt)
# GLS.opt <- fitGLS(formula = AR_coef ~ 1, data = datF, V = V.opt, nugget = NA, no.F = T)
# save(GLS.opt,file =paste0(outDir,'/GLS_opt.rda'))
# load(paste0(outDir,'/GLS_opt.rda'))
# nug.opt = GLS.opt$nugget



pm <- sample_partitions(npix=nrow(datF),partsize=2000,npart=NA)
if(cc==1){
  partGLS.trd <- fitGLS_partition(formula = AR_coef ~ 1, data = datF,
                                  partmat = pm, covar_FUN = "covar_exp",
                                  covar.pars = list(range = range.opt),
                                  nugget = NA)
  save(partGLS.trd,file =paste0(outDir,'/partGLS_trd.rda'))
}else if(cc==2){
  partGLS.eco <- fitGLS_partition(formula = AR_coef ~ 1 + eco, data = datF,
                                  partmat = pm, covar_FUN = "covar_exp",
                                  covar.pars = list(range = range.opt),
                                  nugget = NA)
  save(partGLS.eco,file =paste0(outDir,'/partGLS_eco.rda'))
}

# load(paste0(outDir,'/partGLS_trd.rda'))
# data(partGLS.trd)
# partGLS.trd$overall$pv



# if(cc==1){
  # GLS.trd <- fitGLS(AR_coef ~ 1, data = datF, V = V.opt, nugget = nug.opt, no.F = T)
#   save(GLS.trd,file =paste0(outDir,'/partGLS_trd_1.rda'))
# }else if(cc==2){
  # GLS.eco <- fitGLS(AR_coef ~ 0 + eco, data = datF, V = V.opt, nugget = nug.opt, no.F = F)
#   save(GLS.eco,file =paste0(outDir,'/partGLS_eco_1.rda'))
# }
#   
# load('/projectnb/modislc/users/mkmoon/TAscience/trend/data/parts/h12v04/partGLS_trd_1.rda')
# GLS.trd$nugget
# load('/projectnb/modislc/users/mkmoon/TAscience/trend/data/parts/h12v04/partGLS_eco_1.rda')
# GLS.eco$nugget

 