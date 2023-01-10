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
# tt <- 22; mm <- 4


############################################################
# Get tile list
mcd12q2_path <- '/projectnb/modislc/projects/sat/data/mcd12q/q2/c61'
file <- list.files(path=paste0(mcd12q2_path,'/2001'),full.names=T)
tile_list <- substr(file,74,79)

# Northern hemisphere tiles
tile_list <- tile_list[which(as.numeric(substr(tile_list,5,6))<6)]

# # North America tiles
# tile_list <- c('h10v02', 'h10v03', 'h10v04', 'h10v05', 'h10v06', 'h10v07', 'h10v08', 
#                'h11v02', 'h11v03', 'h11v04', 'h11v05', 'h11v06', 'h11v07', 
#                'h12v01', 'h12v02', 'h12v03', 'h12v04', 'h12v05', 'h12v07', 
#                'h13v01', 'h13v02', 'h13v03', 'h13v04', 
#                'h14v01', 'h14v02', 'h14v03', 'h14v04', 
#                'h15v01', 'h15v02', 'h15v03', 'h16v00', 'h16v01', 'h16v02', 
#                'h17v00', 'h17v01', 'h17v02', 'h28v03', 'h29v03', 
#                'h06v03', 'h07v03', 'h07v05', 'h07v06', 'h07v07', 
#                'h08v03', 'h08v04', 'h08v05', 'h08v06', 'h08v07', 
#                'h09v02', 'h09v03', 'h09v04', 'h09v05', 'h09v06', 'h09v07', 'h09v08')

############################################################
## Get MCD12Q1
mat_lct <- matrix(NA,(2400*2400),21)
for(i in 1:21){
  mcd12q1_path <- paste('/projectnb/modislc/projects/sat/data/mcd12q/q1/',(i+2000),'.01.01',sep='')
  file <- list.files(path=mcd12q1_path,pattern=glob2rx(paste0('*',tile_list[tt],'*')),full.names=T)
  sds <- unlist(gdal_subdatasets(file))
  lct <- raster(sds[1])
  mat_lct[,i] <- values(lct)
  
  print(i)
}
lct_val <- mat_lct[,21]

lct_ch <- matrix(NA,(2400*2400),1)
for(i in 1:(2400*2400)){
  temp <- mat_lct[i,]
  if(length(unique(temp))==1){
    lct_ch[i] <- 1
  }else{
    lct_ch[i] <- 0
  }
}
lct_ch <- setValues(lct,lct_ch)
rm(mat_lct)



############################################################
# Load metrics
dat1 <- matrix(NA,2400*2400,21)
for(year in 2001:2021){
  path <- paste0(mcd12q2_path,'/',year)
  sstr <- paste0('*',tile_list[tt],'*')
  file <- list.files(path=path,pattern=glob2rx(sstr),full.names=T)
  sds  <- unlist(gdal_subdatasets(file))
  
  doy_offset <- as.integer(as.Date(paste(year, "-1-1", sep="")) - as.Date("1970-1-1"))
  
  # DOY metrics
  if(mm==1)       {dat <- values(raster(sds[ 2])) - doy_offset               # Greenup
  }else if(mm== 2){dat <- values(raster(sds[ 3])) - doy_offset               # MidGreenup
  }else if(mm== 3){dat <- values(raster(sds[ 4])) - doy_offset               # Peak
  }else if(mm== 4){dat <- values(raster(sds[ 5])) - doy_offset               # Maturity
  }else if(mm== 5){dat <- values(raster(sds[ 6])) - doy_offset               # Senescence
  }else if(mm== 6){dat <- values(raster(sds[ 7])) - doy_offset               # MidGreendown
  }else if(mm== 7){dat <- values(raster(sds[ 8])) - doy_offset               # Dormancy
  }else if(mm== 8){dat <- values(raster(sds[ 8])) -  values(raster(sds[ 2])) # gsl_long
  }else if(mm== 9){dat <- values(raster(sds[ 7])) -  values(raster(sds[ 3])) # gsl
  }else if(mm==10){dat <- values(raster(sds[ 6])) -  values(raster(sds[ 5])) # gsl_peak
  }else if(mm==11){dat <- values(raster(sds[ 5])) -  values(raster(sds[ 2])) # greenup_period
  }else if(mm==12){dat <- values(raster(sds[ 8])) -  values(raster(sds[ 6])) # greendown_period
  # EVI metrics
  }else if(mm==13){dat <- values(raster(sds[ 9]))                                 # EVI_Minimum
  }else if(mm==14){dat <- values(raster(sds[10]))                                 # EVI_Amplitude
  }else if(mm==15){dat <- values(raster(sds[11]))                                 # EVI_Area
  }else if(mm==16){dat <- values(raster(sds[ 9])) +  values(raster(sds[10]))      # EVI_Max
  }else if(mm==17){dat <- values(raster(sds[ 9])) +  values(raster(sds[10]))*0.15 # EVI_at 15%
  }else if(mm==18){dat <- values(raster(sds[ 9])) +  values(raster(sds[10]))*0.50 # EVI_at 50%
  }else if(mm==19){dat <- values(raster(sds[ 9])) +  values(raster(sds[10]))*0.90 # EVI_at 90%
  # Rates
  }else if(mm==20){
    eviamp <- values(raster(sds[10]))*0.90 - values(raster(sds[10]))*0.15
    dat <- eviamp / (values(raster(sds[ 5])) -  values(raster(sds[ 2]))) # rate_greenup_long
  }else if(mm==21){
    eviamp <- values(raster(sds[10]))*0.50 - values(raster(sds[10]))*0.15
    dat <- eviamp / (values(raster(sds[ 3])) -  values(raster(sds[ 2]))) # rate_greenup_early
  }else if(mm==22){
    eviamp <- values(raster(sds[10]))*0.90 - values(raster(sds[10]))*0.50
    dat <- eviamp / (values(raster(sds[ 5])) -  values(raster(sds[ 3]))) # rate_greenup_late
  }else if(mm==23){
    eviamp <- values(raster(sds[10]))*0.90 - values(raster(sds[10]))*0.15
    dat <- eviamp / (values(raster(sds[ 8])) -  values(raster(sds[ 6]))) # rate_greendown_long
  }else if(mm==24){
    eviamp <- values(raster(sds[10]))*0.90 - values(raster(sds[10]))*0.50
    dat <- eviamp / (values(raster(sds[ 7])) -  values(raster(sds[ 6]))) # rate_greendown_early
  }else           {
    eviamp <- values(raster(sds[10]))*0.50 - values(raster(sds[10]))*0.15
    dat <- eviamp / (values(raster(sds[ 8])) -  values(raster(sds[ 7]))) # rate_greendown_late  
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
     # & lct_ch[i]==1 
     # & lct_val[i]!=13 & lct_val[i]!=15 & lct_val[i]!=16 & lct_val[i]!=17){
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
outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/metrics/nh/',tile_list[tt])
if (!dir.exists(outDir)) {dir.create(outDir)}  
metric <- sprintf('%02d',mm)
save(#dat1,dat2,
     # dat1Org,dat2Org,dat1Nor,dat2Nor,
     dat1OrgMedi,dat2OrgMedi,dat1NorMedi,dat2NorMedi,
     lct_ch,
     lct_val,
     file=paste0(outDir,'/metrics_',metric,'.rda'))

  
  

