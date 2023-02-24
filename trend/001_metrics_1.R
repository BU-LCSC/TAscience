rm(list = ls())

library(terra)
library(sf)


############################################################
# From bash code
args <- commandArgs()
print(args)

tt <- as.numeric(substr(args[3],1,3))
mm <- as.numeric(substr(args[3],4,5))
# tt <- 69; mm <- 4; year <- 2012


############################################################
# Get tile list
mcd12q2_path <- '/projectnb/modislc/projects/sat/data/mcd12q/q2/c61'
file <- list.files(path=paste0(mcd12q2_path,'/2001'),full.names=T)
tile_list <- substr(file,74,79)

# # Northern hemisphere tiles
# file <- file[which(as.numeric(substr(tile_list,5,6))<6)]
# tile_list <- tile_list[which(as.numeric(substr(tile_list,5,6))<6)]


############################################################
# Load metrics
dat1 <- matrix(NA,2400*2400,21)
for(year in 2001:2021){
  path <- paste0(mcd12q2_path,'/',year)
  sstr <- paste0('*',tile_list[tt],'*')
  file <- list.files(path=path,pattern=glob2rx(sstr),full.names=T)
  sds  <- unlist(gdal_subdatasets(file))
  
  valQA <- as.numeric(values(rast(sds[12],lyrs=1)))
  doy_offset <- as.integer(as.Date(paste(year, "-1-1", sep="")) - as.Date("1970-1-1"))
  
  # DOY metrics
  if(mm==1)       {dat <- values(rast(sds[ 2],lyrs=1),mat=F) - doy_offset               # Greenup
  }else if(mm== 2){dat <- values(rast(sds[ 3],lyrs=1),mat=F) - doy_offset               # MidGreenup
  }else if(mm== 3){dat <- values(rast(sds[ 4],lyrs=1),mat=F) - doy_offset               # Peak
  }else if(mm== 4){dat <- values(rast(sds[ 5],lyrs=1),mat=F) - doy_offset               # Maturity
  }else if(mm== 5){dat <- values(rast(sds[ 6],lyrs=1),mat=F) - doy_offset               # Senescence
  }else if(mm== 6){dat <- values(rast(sds[ 7],lyrs=1),mat=F) - doy_offset               # MidGreendown
  }else if(mm== 7){dat <- values(rast(sds[ 8],lyrs=1),mat=F) - doy_offset               # Dormancy
  }else if(mm== 8){dat <- values(rast(sds[ 8],lyrs=1),mat=F) -  values(rast(sds[ 2],lyrs=1),mat=F) # gsl_long
  }else if(mm== 9){dat <- values(rast(sds[ 7],lyrs=1),mat=F) -  values(rast(sds[ 3],lyrs=1),mat=F) # gsl
  }else if(mm==10){dat <- values(rast(sds[ 6],lyrs=1),mat=F) -  values(rast(sds[ 5],lyrs=1),mat=F) # gsl_peak
  }else if(mm==11){dat <- values(rast(sds[ 5],lyrs=1),mat=F) -  values(rast(sds[ 2],lyrs=1),mat=F) # greenup_period
  }else if(mm==12){dat <- values(rast(sds[ 8],lyrs=1),mat=F) -  values(rast(sds[ 6],lyrs=1),mat=F) # greendown_period
  # EVI metrics
  }else if(mm==13){dat <- values(rast(sds[ 9],lyrs=1),mat=F)                                 # EVI_Minimum
  }else if(mm==14){dat <- values(rast(sds[10],lyrs=1),mat=F)                                 # EVI_Amplitude
  }else if(mm==15){dat <- values(rast(sds[11],lyrs=1),mat=F)                                 # EVI_Area
  }else if(mm==16){dat <- values(rast(sds[ 9],lyrs=1),mat=F) +  values(rast(sds[10],lyrs=1),mat=F)      # EVI_Max
  }else if(mm==17){dat <- values(rast(sds[ 9],lyrs=1),mat=F) +  values(rast(sds[10],lyrs=1),mat=F)*0.15 # EVI_at 15%
  }else if(mm==18){dat <- values(rast(sds[ 9],lyrs=1),mat=F) +  values(rast(sds[10],lyrs=1),mat=F)*0.50 # EVI_at 50%
  }else if(mm==19){dat <- values(rast(sds[ 9],lyrs=1),mat=F) +  values(rast(sds[10],lyrs=1),mat=F)*0.90 # EVI_at 90%
  # Rates
  }else if(mm==20){
    eviamp <- values(rast(sds[10],lyrs=1),mat=F)*0.90 - values(rast(sds[10],lyrs=1),mat=F)*0.15
    dat <- eviamp / (values(rast(sds[ 5],lyrs=1),mat=F) -  values(rast(sds[ 2],lyrs=1),mat=F)) # rate_greenup_long
  }else if(mm==21){
    eviamp <- values(rast(sds[10],lyrs=1),mat=F)*0.50 - values(rast(sds[10],lyrs=1),mat=F)*0.15
    dat <- eviamp / (values(rast(sds[ 3],lyrs=1),mat=F) -  values(rast(sds[ 2],lyrs=1),mat=F)) # rate_greenup_early
  }else if(mm==22){
    eviamp <- values(rast(sds[10],lyrs=1),mat=F)*0.90 - values(rast(sds[10],lyrs=1),mat=F)*0.50
    dat <- eviamp / (values(rast(sds[ 5],lyrs=1),mat=F) -  values(rast(sds[ 3],lyrs=1),mat=F)) # rate_greenup_late
  }else if(mm==23){
    eviamp <- values(rast(sds[10],lyrs=1),mat=F)*0.90 - values(rast(sds[10],lyrs=1),mat=F)*0.15
    dat <- eviamp / (values(rast(sds[ 8],lyrs=1),mat=F) -  values(rast(sds[ 6],lyrs=1),mat=F)) # rate_greendown_long
  }else if(mm==24){
    eviamp <- values(rast(sds[10],lyrs=1),mat=F)*0.90 - values(rast(sds[10],lyrs=1),mat=F)*0.50
    dat <- eviamp / (values(rast(sds[ 7],lyrs=1),mat=F) -  values(rast(sds[ 6],lyrs=1),mat=F)) # rate_greendown_early
  }else           {
    eviamp <- values(rast(sds[10],lyrs=1),mat=F)*0.50 - values(rast(sds[10],lyrs=1),mat=F)*0.15
    dat <- eviamp / (values(rast(sds[ 8],lyrs=1),mat=F) -  values(rast(sds[ 7],lyrs=1),mat=F)) # rate_greendown_late  
  }
  
  # phe1 <- rast(sds[ 2],lyrs=1),mat=F)
  # phe2 <- rast(sds[ 8],lyrs=1),mat=F)
  # phe3 <- rast(sds[ 8],lyrs=1),mat=F) - rast(sds[ 2],lyrs=1),mat=F)
  # 
  # filt <- which(values(phe1)<0|values(phe2)>365|values(phe3)>300)
  # dat[filt] <- NA
 
  dat[which(valQA!=0 & valQA!=1)] <- NA # Only best or good
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

  
  

