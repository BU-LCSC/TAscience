rm(list = ls())

library(gdalUtils)
library(sp)
library(raster)


############################################################
# From bash code
args <- commandArgs()
print(args)

tt <- as.numeric(substr(args[3],1,3))
mm <- as.numeric(substr(args[3],4,5))
# tt <- 69; mm <- 2


############################################################
# Get tile list
mcd12q2_path <- '/projectnb/modislc/projects/sat/data/mcd12q/q2/c61'
file <- list.files(path=paste0(mcd12q2_path,'/2001'),full.names=T)
tile_list <- substr(file,74,79)


## panel grid
sds <- get_subdatasets(file[tt])
imgBase <- raster(sds[1])

cxx <- 20
cyy <- 20
panel_grid <- matrix(1:length(imgBase),dim(imgBase)[1]/cxx,dim(imgBase)[2]/cyy)
panel_grid <- raster(panel_grid,crs=imgBase@crs,
                     xmn=imgBase@extent@xmin,xmx=imgBase@extent@xmax,
                     ymn=imgBase@extent@ymin,ymx=imgBase@extent@ymax)


############################################################
# Load metrics
dat1 <- matrix(NA,length(panel_grid),20)
for(year in 2001:2020){
  path <- paste0(mcd12q2_path,'/',year)
  sstr <- paste0('*',tile_list[tt],'*')
  file <- list.files(path=path,pattern=glob2rx(sstr),full.names=T)
  sds  <- get_subdatasets(file)
  
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
  }else if(mm==20){dat <- values(raster(sds[10]))/ (values(raster(sds[ 5])) -  values(raster(sds[ 2]))) # rate_greenup_long
  }else if(mm==21){dat <- values(raster(sds[10]))/ (values(raster(sds[ 3])) -  values(raster(sds[ 2]))) # rate_greenup_early
  }else if(mm==22){dat <- values(raster(sds[10]))/ (values(raster(sds[ 5])) -  values(raster(sds[ 3]))) # rate_greenup_late
  }else if(mm==23){dat <- values(raster(sds[10]))/ (values(raster(sds[ 8])) -  values(raster(sds[ 6]))) # rate_greendown_long
  }else if(mm==24){dat <- values(raster(sds[10]))/ (values(raster(sds[ 7])) -  values(raster(sds[ 6]))) # rate_greendown_early
  }else           {dat <- values(raster(sds[10]))/ (values(raster(sds[ 8])) -  values(raster(sds[ 7]))) # rate_greendown_late  
  }
  
  #
  datRas <- setValues(imgBase,dat)
  datMat <- as.matrix(datRas)
  for(i in 1:dim(panel_grid)[1]){
    for(j in 1:dim(panel_grid)[2]){
      temp <- datMat[(cxx*(i-1)+1):(cxx*i),(cyy*(j-1)+1):(cyy*j)]
      if(sum(is.na(temp))>360){
        dat1[(j+(dim(panel_grid)[1]*(i-1))),(year-2000)] <- NA
      }else{
        dat1[(j+(dim(panel_grid)[1]*(i-1))),(year-2000)] <- median(temp,na.rm=T)
      }
    }
  }
  
  print(year)
}

############################################################
## Calculate change
# Normalize & filtering
dat2 <- matrix(NA,length(panel_grid),20)
for(i in 1:length(panel_grid)){
  temp <- dat1[i,]
  
  if(sum(is.na(temp[c(1:7)]))<4 & sum(is.na(temp[c(14:20)]))<4 & sd(temp,na.rm=T)<=60){
    dat2[i,] <- scale(temp)  
  }else{
    dat1[i,] <- NA
    dat2[i,] <- NA
  }
  
  if(i%%1000==0) print(i)
}

# Calc early and late mean
dat1Org <- apply(dat1[,c(1:7)],1,mean,na.rm=T)
dat2Org <- apply(dat1[,c(14:20)],1,mean,na.rm=T)
dat1Nor <- apply(dat2[,c(1:7)],1,mean,na.rm=T)
dat2Nor <- apply(dat2[,c(14:20)],1,mean,na.rm=T)

dat1OrgMedi <- apply(dat1[,c(1:7)],1,median,na.rm=T)
dat2OrgMedi <- apply(dat1[,c(14:20)],1,median,na.rm=T)
dat1NorMedi <- apply(dat2[,c(1:7)],1,median,na.rm=T)
dat2NorMedi <- apply(dat2[,c(14:20)],1,median,na.rm=T)


############################################################  
# save
outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/metrics/filt_01_10by10_10pc/',tile_list[tt])
if (!dir.exists(outDir)) {dir.create(outDir)}  
metric <- sprintf('%02d',mm)
save(dat1,dat2,panel_grid,
     dat1Org,dat2Org,dat1Nor,dat2Nor,
     dat1OrgMedi,dat2OrgMedi,dat1NorMedi,dat2NorMedi,
     file=paste0(outDir,'/metrics_',metric,'.rda'))

  
  

