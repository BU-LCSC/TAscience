rm(list = ls())

library(gdalUtils)
library(sp)
library(raster)

# library(doMC)
# library(doParallel)


############################################################
# From bash code
args <- commandArgs()
print(args)

tile <-            substr(args[3], 1, 6)
yy   <- as.numeric(substr(args[3], 7,10))
# tile <- 'h18v08'; yy <- 2004

print(tile)
print(yy)


############################################################
# Cluster
path <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_nor/clust_2pc/map_clust'
clst <- raster(paste0(path,'/map_clust_',tile,'.tif'))
cval <- values(clst)
c1   <- which(cval==1);c2   <- which(cval==2);c3   <- which(cval==3)
c4   <- which(cval==4);c5   <- which(cval==5);c6   <- which(cval==6)
c7   <- which(cval==7);c8   <- which(cval==8);c9   <- which(cval==9)

# NBAR files 
path <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/nbar/e4ftl01.cr.usgs.gov/MOTA/MCD43A4.061'
files <- list.files(path,pattern=glob2rx(paste0('MCD43A4.A',yy,'*.',tile,'*.hdf')),full.names=T,recursive=T)

dd <- as.numeric(substr(files,120,122))
print(dd)

dat1 <- matrix(NA,length(c1),365);dat2 <- matrix(NA,length(c2),365);dat3 <- matrix(NA,length(c3),365)
dat4 <- matrix(NA,length(c4),365);dat5 <- matrix(NA,length(c5),365);dat6 <- matrix(NA,length(c6),365)
dat7 <- matrix(NA,length(c7),365);dat8 <- matrix(NA,length(c8),365);dat9 <- matrix(NA,length(c9),365)

options(warn=-1)
# registerDoMC()

# foreach(i=1:length(files)) %dopar% {
for(i in 1:length(files)){
  log <- try({
    
      sds <- get_subdatasets(files[i])
    
      q1  <- raster(sds[1])
      q2  <- raster(sds[2])
      red <- raster(sds[8])
      nir <- raster(sds[9])
      evi2 <- 2.5*(nir - red) / (nir + 2.4*red + 1)
      evi2[q1!=0|q2!=0] <- NA
      
      dat1[,dd[i]] <- values(evi2)[c1]
      dat2[,dd[i]] <- values(evi2)[c2]
      dat3[,dd[i]] <- values(evi2)[c3]
      dat4[,dd[i]] <- values(evi2)[c4]
      dat5[,dd[i]] <- values(evi2)[c5]
      dat6[,dd[i]] <- values(evi2)[c6]
      dat7[,dd[i]] <- values(evi2)[c7]
      dat8[,dd[i]] <- values(evi2)[c8]
      dat9[,dd[i]] <- values(evi2)[c9]
      
      print(dd[i])
    
    })
  
  if(inherits(log, "try-error")){print(paste('Error in ',dd[i]))} 
  
}


outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/nbar/',tile)
if (!dir.exists(outDir)) {dir.create(outDir)}

outDir1 <- paste0(outDir,'/01'); if (!dir.exists(outDir1)) {dir.create(outDir1)}
save(dat1,file=paste0(outDir1,'/evi2_',yy,'.rda'))
outDir2 <- paste0(outDir,'/02'); if (!dir.exists(outDir2)) {dir.create(outDir2)}
save(dat2,file=paste0(outDir2,'/evi2_',yy,'.rda'))
outDir3 <- paste0(outDir,'/03'); if (!dir.exists(outDir3)) {dir.create(outDir3)}
save(dat3,file=paste0(outDir3,'/evi2_',yy,'.rda'))
outDir4 <- paste0(outDir,'/04'); if (!dir.exists(outDir4)) {dir.create(outDir4)}
save(dat4,file=paste0(outDir4,'/evi2_',yy,'.rda'))
outDir5 <- paste0(outDir,'/05'); if (!dir.exists(outDir5)) {dir.create(outDir5)}
save(dat5,file=paste0(outDir5,'/evi2_',yy,'.rda'))
outDir6 <- paste0(outDir,'/06'); if (!dir.exists(outDir6)) {dir.create(outDir6)}
save(dat6,file=paste0(outDir6,'/evi2_',yy,'.rda'))
outDir7 <- paste0(outDir,'/07'); if (!dir.exists(outDir7)) {dir.create(outDir7)}
save(dat7,file=paste0(outDir7,'/evi2_',yy,'.rda'))
outDir8 <- paste0(outDir,'/08'); if (!dir.exists(outDir8)) {dir.create(outDir8)}
save(dat8,file=paste0(outDir8,'/evi2_',yy,'.rda'))
outDir9 <- paste0(outDir,'/09'); if (!dir.exists(outDir9)) {dir.create(outDir9)}
save(dat9,file=paste0(outDir9,'/evi2_',yy,'.rda'))


