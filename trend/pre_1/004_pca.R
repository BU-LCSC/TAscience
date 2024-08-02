rm(list = ls())

library(terra)
library(sf)


############################################################
# From bash code
args <- commandArgs()
print(args)

tt <- as.numeric(args[3])
# tt <- 22


############################################################
# PC - metrics
path  <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_sig/merge/'
files <- list.files(path,pattern=glob2rx('1_me*'),full.names=T)

dat <- matrix(NA,(1800*3600),25)
for(i in 1:25){
  r1 <- rast(files[i])
  dat[,i] <- values(r1)
  
  print(i)
}
dat1 <- na.omit(dat)

pcp     <- prcomp(dat1,center = T,scale. = T)
var_explained <- pcp$sdev^2 / sum(pcp$sdev^2)
plot(round(var_explained*100,1))
sum(var_explained[1:6])
plot(pcp$rotation[,1])

dat2 <- predict(pcp,dat)    


outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_sig/merge/'

for(i in 1:25){
  rP <- setValues(r1,dat2[,i])
  writeRaster(rP,filename=paste0(outDir,'/pc_',sprintf('%02d',i),'.tif'),overwrite=TRUE)
}


############################################################
# PC - climate
path  <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_sig/climate/'
files <- list.files(path,pattern=glob2rx('*dif*'),full.names=T)

dat <- matrix(NA,(360*720),10)
for(i in 1:10){
  r1 <- rast(files[i])
  dat[,i] <- values(r1)
  
  print(i)
}
dat1 <- na.omit(dat)

pcp     <- prcomp(dat1,center = T,scale. = T)
var_explained <- pcp$sdev^2 / sum(pcp$sdev^2)
plot(round(var_explained*100,1))
sum(var_explained[1:5])
plot(pcp$rotation[,1])

dat2 <- predict(pcp,dat)    


outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_sig/merge/'
for(i in 1:10){
  rP <- setValues(r1,dat2[,i])
  writeRaster(rP,filename=paste0(outDir,'/pc_cli_',sprintf('%02d',i),'.tif'),overwrite=TRUE)
}
