rm(list = ls())


library(sp)
library(raster)
library(terra)
library(sf)


############################################################
# From bash code
args <- commandArgs()
print(args)

tt <- as.numeric(substr(args[3],1,3))
mm <- as.numeric(substr(args[3],4,5))
# tt <- 51; mm <- 4; year <- 2012


############################################################
# Get tile list
mcd12q2_path <- '/projectnb/modislc/projects/sat/data/mcd12q/q2/c61'
file <- list.files(path=paste0(mcd12q2_path,'/2001'),full.names=T)
tile_list <- substr(file,74,79)



############################################################
# Load metrics
dat1 <- matrix(NA,2400*2400,21)
for(year in 2001:2021){
  path <- paste0(mcd12q2_path,'/',year)
  sstr <- paste0('*',tile_list[tt],'*')
  file <- list.files(path=path,pattern=glob2rx(sstr),full.names=T)
  sds  <- gdal_subdatasets(file)
  
  doy_offset <- as.integer(as.Date(paste(year, "-1-1", sep="")) - as.Date("1970-1-1"))
  
  # DOY metrics
  if(mm==1)       {dat <- values(raster(sds[[ 2]][1])) - doy_offset               # Greenup
  }else if(mm== 2){dat <- values(raster(sds[[ 3]][1])) - doy_offset               # MidGreenup
  }else if(mm== 3){dat <- values(raster(sds[[ 4]][1])) - doy_offset               # Peak
  }else if(mm== 4){dat <- values(raster(sds[[ 5]][1])) - doy_offset               # Maturity
  }else if(mm== 5){dat <- values(raster(sds[[ 6]][1])) - doy_offset               # Senescence
  }else if(mm== 6){dat <- values(raster(sds[[ 7]][1])) - doy_offset               # MidGreendown
  }else if(mm== 7){dat <- values(raster(sds[[ 8]][1])) - doy_offset               # Dormancy
  }else if(mm== 8){dat <- values(raster(sds[[ 8]][1])) -  values(raster(sds[[ 2]][1])) # gsl_long
  }else if(mm== 9){dat <- values(raster(sds[[ 7]][1])) -  values(raster(sds[[ 3]][1])) # gsl
  }else if(mm==10){dat <- values(raster(sds[[ 6]][1])) -  values(raster(sds[[ 5]][1])) # gsl_peak
  }else if(mm==11){dat <- values(raster(sds[[ 5]][1])) -  values(raster(sds[[ 2]][1])) # greenup_period
  }else if(mm==12){dat <- values(raster(sds[[ 8]][1])) -  values(raster(sds[[ 6]][1])) # greendown_period
  # EVI metrics
  }else if(mm==13){dat <- values(raster(sds[[ 9]][1]))                                 # EVI_Minimum
  }else if(mm==14){dat <- values(raster(sds[[10]][1]))                                 # EVI_Amplitude
  }else if(mm==15){dat <- values(raster(sds[[11]][1]))                                 # EVI_Area
  }else if(mm==16){dat <- values(raster(sds[[ 9]][1])) +  values(raster(sds[[10]][1]))      # EVI_Max
  }else if(mm==17){dat <- values(raster(sds[[ 9]][1])) +  values(raster(sds[[10]][1]))*0.15 # EVI_at 15%
  }else if(mm==18){dat <- values(raster(sds[[ 9]][1])) +  values(raster(sds[[10]][1]))*0.50 # EVI_at 50%
  }else if(mm==19){dat <- values(raster(sds[[ 9]][1])) +  values(raster(sds[[10]][1]))*0.90 # EVI_at 90%
  # Rates
  }else if(mm==20){
    eviamp <- values(raster(sds[[10]][1]))*0.90 - values(raster(sds[[10]][1]))*0.15
    dat <- eviamp / (values(raster(sds[[ 5]][1])) -  values(raster(sds[[ 2]][1]))) # rate_greenup_long
  }else if(mm==21){
    eviamp <- values(raster(sds[[10]][1]))*0.50 - values(raster(sds[[10]][1]))*0.15
    dat <- eviamp / (values(raster(sds[[ 3]][1])) -  values(raster(sds[[ 2]][1]))) # rate_greenup_early
  }else if(mm==22){
    eviamp <- values(raster(sds[[10]][1]))*0.90 - values(raster(sds[[10]][1]))*0.50
    dat <- eviamp / (values(raster(sds[[ 5]][1])) -  values(raster(sds[[ 3]][1]))) # rate_greenup_late
  }else if(mm==23){
    eviamp <- values(raster(sds[[10]][1]))*0.90 - values(raster(sds[[10]][1]))*0.15
    dat <- eviamp / (values(raster(sds[[ 8]][1])) -  values(raster(sds[[ 6]][1]))) # rate_greendown_long
  }else if(mm==24){
    eviamp <- values(raster(sds[[10]][1]))*0.90 - values(raster(sds[[10]][1]))*0.50
    dat <- eviamp / (values(raster(sds[[ 7]][1])) -  values(raster(sds[[ 6]][1]))) # rate_greendown_early
  }else           {
    eviamp <- values(raster(sds[[10]][1]))*0.50 - values(raster(sds[[10]][1]))*0.15
    dat <- eviamp / (values(raster(sds[[ 8]][1])) -  values(raster(sds[[ 7]][1]))) # rate_greendown_late  
  }
  
  # phe1 <- raster(sds[ 2])
  # phe2 <- raster(sds[ 8])
  # phe3 <- raster(sds[ 8]) - raster(sds[ 2])
  # 
  # filt <- which(values(phe1)<0|values(phe2)>365|values(phe3)>300)
  # dat[filt] <- NA
 
  dat1[,(year-2000)] <- dat
  print(year)
}



############################################################
## Calculate change
# Normalize & filtering
dat2 <- matrix(NA,(2400*2400),21)
for(i in 1:(2400*2400)){
  temp <- dat1[i,]
  
  if(sum(is.na(temp[c(1:7)]))<4 & sum(is.na(temp[c(15:21)]))<4 & sd(temp,na.rm=T)<=90){
    dat1[i,] <- temp
    dat2[i,] <- scale(temp)
  }else{
    dat1[i,] <- NA
    dat2[i,] <- NA
  }
  
  if(i%%1000000==0) print(i)
}

# Calc early and late mean
# dat1Org <- apply(dat1[,c(1:7)],1,mean,na.rm=T)
# dat2Org <- apply(dat1[,c(15:21)],1,mean,na.rm=T)
# dat1Nor <- apply(dat2[,c(1:7)],1,mean,na.rm=T)
# dat2Nor <- apply(dat2[,c(15:21)],1,mean,na.rm=T)

dat1OrgMedi <- apply(dat1[,c(1:7)],1,median,na.rm=T)
dat2OrgMedi <- apply(dat1[,c(15:21)],1,median,na.rm=T)
dat1NorMedi <- apply(dat2[,c(1:7)],1,median,na.rm=T)
dat2NorMedi <- apply(dat2[,c(15:21)],1,median,na.rm=T)


############################################################  
# save
outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/metrics/filt_21yrs_nor/',tile_list[tt])
if (!dir.exists(outDir)) {dir.create(outDir)}  
metric <- sprintf('%02d',mm)
save(#dat1,dat2,
     # dat1Org,dat2Org,dat1Nor,dat2Nor,
     dat1OrgMedi,dat2OrgMedi,dat1NorMedi,dat2NorMedi,
     file=paste0(outDir,'/metrics_',metric,'.rda'))

  
  

