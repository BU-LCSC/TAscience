library(raster)
library(rgdal)
library(gdalUtils)

library(rjson)
library(geojsonR)

library(doMC)
library(doParallel)

library(RcppRoll)

###############################
args <- commandArgs()
print(args)

tile <- substr(args[3],1,6)
year <- as.numeric(substr(args[3],7,10))
cc   <- as.numeric(substr(args[3],11,13))
# tile <- 'h10v03'; year <- 2003; cc <- 50


###############################
params <- fromJSON(file='/usr3/graduate/mkmoon/GitHub/TAscience/mcd12q2/Applying_MSLSP/Q2_Parameters.json')
source(params$setup$rFunctions)


########################################
ckDir <- paste0(params$setup$workDir,tile,'/chunks/',year)
print(ckDir)

ckNum <- sprintf('%03d',cc)
file <- list.files(path=ckDir,pattern=glob2rx(paste0('*',ckNum,'.rda')),full.names=T)

load(file)


##########################################
numPix <- dim(band1)[1]
phenYr <- year
# dates <- c(seq(as.Date(0,origin=paste0((phenYr-1),'-1-1')),as.Date(364,origin=paste0((phenYr-1),'-1-1')),by='day'),
#            seq(as.Date(0,origin=paste0((phenYr-0),'-1-1')),as.Date(364,origin=paste0((phenYr-0),'-1-1')),by='day'),
#            seq(as.Date(0,origin=paste0((phenYr+1),'-1-1')),as.Date(364,origin=paste0((phenYr+1),'-1-1')),by='day'))
dates <- as.Date(dates,origin='1970-1-1')

numLyrs <- 24
pheno_mat <- matrix(NA,numPix,numLyrs)


for (i in 1:numPix){
  red     <- band1[i,]
  nir     <- band2[i,]
  green   <- band2[i,]
  swir    <- band2[i,]
  snowPix <- bsnow[i,];
  snowPix[is.na(snowPix)] <- 0

  pheno_mat[i,] <- DoPhenologyMODIS(red,nir,green,swir,snowPix,dates,phenYr,params,numLyrs)

  if(i%%10000==0) print(i)
}


# Save
ckPheDir  <- paste0(params$setup$workDir,tile,'/chunk_phe'); if (!dir.exists(ckPheDir)) {dir.create(ckPheDir)}
ckPheDirY <- paste0(ckPheDir,'/',year); if (!dir.exists(ckPheDirY)) {dir.create(ckPheDirY)}

save(pheno_mat,file=paste0(ckPheDirY,'/chunk_phe_',ckNum,'.rda'))





########################################
files <- list.files(path=ckPheDirY,pattern=glob2rx('*.rda'),full.names=T)
print(length(files))

productTable <- read.csv(params$setup$productTable,header=T,stringsAsFactors = F)

if(length(files==200)){
  
  # base image
  imgDir <- paste0(params$setup$dataDir)
  q1 <- list.files(path=paste0(imgDir,'mcd12q/q1/2020.01.01'),pattern=glob2rx(paste0('*',tile,'*.hdf')),recursive=T,full.names=T)
  imgBase <- raster(get_subdatasets(q1[1])[[1]])
  
  numPix <- length(imgBase)
  numChunks <- params$setup$numChunks
  chunk <- numPix%/%numChunks
  
  # Save
  pheDir <- paste0(params$setup$workDir,tile,'/metrics/')
  if (!dir.exists(pheDir)) {dir.create(pheDir)}
  
  pheDirYear <- paste0(pheDir,year)
  if (!dir.exists(pheDirYear)) {dir.create(pheDirYear)}
  
  
  l01 <- matrix(NA,numPix,1);l02 <- matrix(NA,numPix,1);l03 <- matrix(NA,numPix,1);l04 <- matrix(NA,numPix,1)
  l05 <- matrix(NA,numPix,1);l06 <- matrix(NA,numPix,1);l07 <- matrix(NA,numPix,1);l08 <- matrix(NA,numPix,1)
  l09 <- matrix(NA,numPix,1);l10 <- matrix(NA,numPix,1);l11 <- matrix(NA,numPix,1);l12 <- matrix(NA,numPix,1)
  l13 <- matrix(NA,numPix,1);l14 <- matrix(NA,numPix,1);l15 <- matrix(NA,numPix,1);l16 <- matrix(NA,numPix,1)
  l17 <- matrix(NA,numPix,1);l18 <- matrix(NA,numPix,1);l19 <- matrix(NA,numPix,1);l20 <- matrix(NA,numPix,1)
  l21 <- matrix(NA,numPix,1);l22 <- matrix(NA,numPix,1);l23 <- matrix(NA,numPix,1);l24 <- matrix(NA,numPix,1)
  
  for(i in 1:numChunks){
    cc <- sprintf('%03d',i)
    cfile <- paste0(ckPheDirY,'/chunk_phe_',cc,'.rda') 
    log <- try(load(cfile),silent=F)
    if (inherits(log, 'try-error')) next 
    
    if(i==numChunks){chunks <- c((chunk*(i-1)+1):numPix)
    }else{chunks <- c((chunk*(i-1)+1):(chunk*i))}
    
    chunkStart <- chunks[1];  chunkEnd <- chunks[length(chunks)]
    
    l01[chunkStart:chunkEnd,] <- pheno_mat[, (1)];l02[chunkStart:chunkEnd,] <- pheno_mat[, (2)];l03[chunkStart:chunkEnd,] <- pheno_mat[, (3)]
    l04[chunkStart:chunkEnd,] <- pheno_mat[, (4)];l05[chunkStart:chunkEnd,] <- pheno_mat[, (5)];l06[chunkStart:chunkEnd,] <- pheno_mat[, (6)]
    l07[chunkStart:chunkEnd,] <- pheno_mat[, (7)];l08[chunkStart:chunkEnd,] <- pheno_mat[, (8)];l09[chunkStart:chunkEnd,] <- pheno_mat[, (9)]
    l10[chunkStart:chunkEnd,] <- pheno_mat[,(10)];l11[chunkStart:chunkEnd,] <- pheno_mat[,(11)];l12[chunkStart:chunkEnd,] <- pheno_mat[,(12)]
    l13[chunkStart:chunkEnd,] <- pheno_mat[,(13)];l14[chunkStart:chunkEnd,] <- pheno_mat[,(14)];l15[chunkStart:chunkEnd,] <- pheno_mat[,(15)]
    l16[chunkStart:chunkEnd,] <- pheno_mat[,(16)];l17[chunkStart:chunkEnd,] <- pheno_mat[,(17)];l18[chunkStart:chunkEnd,] <- pheno_mat[,(18)]
    l19[chunkStart:chunkEnd,] <- pheno_mat[,(19)];l20[chunkStart:chunkEnd,] <- pheno_mat[,(20)];l21[chunkStart:chunkEnd,] <- pheno_mat[,(21)]
    l22[chunkStart:chunkEnd,] <- pheno_mat[,(22)];l23[chunkStart:chunkEnd,] <- pheno_mat[,(23)];l24[chunkStart:chunkEnd,] <- pheno_mat[,(24)]
    
    print(i)
  }
  
  r01 <- setValues(imgBase,l01); r02 <- setValues(imgBase,l02); r03 <- setValues(imgBase,l03); r04 <- setValues(imgBase,l04)
  r05 <- setValues(imgBase,l05); r06 <- setValues(imgBase,l06); r07 <- setValues(imgBase,l07); r08 <- setValues(imgBase,l08)
  r09 <- setValues(imgBase,l09); r10 <- setValues(imgBase,l10); r11 <- setValues(imgBase,l11); r12 <- setValues(imgBase,l12)
  r13 <- setValues(imgBase,l13); r14 <- setValues(imgBase,l14); r15 <- setValues(imgBase,l15); r16 <- setValues(imgBase,l16)
  r17 <- setValues(imgBase,l17); r18 <- setValues(imgBase,l18); r19 <- setValues(imgBase,l19); r20 <- setValues(imgBase,l20)
  r21 <- setValues(imgBase,l21); r22 <- setValues(imgBase,l22); r23 <- setValues(imgBase,l23); r24 <- setValues(imgBase,l24)
  
  
  # Save
  writeRaster(r01,filename=paste0(pheDirYear,'/01_',year,'_',productTable$short_name[ 1],'.tif'), format="GTiff", overwrite=TRUE)
  writeRaster(r02,filename=paste0(pheDirYear,'/02_',year,'_',productTable$short_name[ 2],'.tif'), format="GTiff", overwrite=TRUE)
  writeRaster(r03,filename=paste0(pheDirYear,'/03_',year,'_',productTable$short_name[ 3],'.tif'), format="GTiff", overwrite=TRUE)
  writeRaster(r04,filename=paste0(pheDirYear,'/04_',year,'_',productTable$short_name[ 4],'.tif'), format="GTiff", overwrite=TRUE)
  writeRaster(r05,filename=paste0(pheDirYear,'/05_',year,'_',productTable$short_name[ 5],'.tif'), format="GTiff", overwrite=TRUE)
  writeRaster(r06,filename=paste0(pheDirYear,'/06_',year,'_',productTable$short_name[ 6],'.tif'), format="GTiff", overwrite=TRUE)
  writeRaster(r07,filename=paste0(pheDirYear,'/07_',year,'_',productTable$short_name[ 7],'.tif'), format="GTiff", overwrite=TRUE)
  writeRaster(r08,filename=paste0(pheDirYear,'/08_',year,'_',productTable$short_name[ 8],'.tif'), format="GTiff", overwrite=TRUE)
  writeRaster(r09,filename=paste0(pheDirYear,'/09_',year,'_',productTable$short_name[ 9],'.tif'), format="GTiff", overwrite=TRUE)
  writeRaster(r10,filename=paste0(pheDirYear,'/10_',year,'_',productTable$short_name[10],'.tif'), format="GTiff", overwrite=TRUE)
  writeRaster(r11,filename=paste0(pheDirYear,'/11_',year,'_',productTable$short_name[11],'.tif'), format="GTiff", overwrite=TRUE)
  writeRaster(r12,filename=paste0(pheDirYear,'/12_',year,'_',productTable$short_name[12],'.tif'), format="GTiff", overwrite=TRUE)
  writeRaster(r13,filename=paste0(pheDirYear,'/13_',year,'_',productTable$short_name[13],'.tif'), format="GTiff", overwrite=TRUE)
  writeRaster(r14,filename=paste0(pheDirYear,'/14_',year,'_',productTable$short_name[14],'.tif'), format="GTiff", overwrite=TRUE)
  writeRaster(r15,filename=paste0(pheDirYear,'/15_',year,'_',productTable$short_name[15],'.tif'), format="GTiff", overwrite=TRUE)
  writeRaster(r16,filename=paste0(pheDirYear,'/16_',year,'_',productTable$short_name[16],'.tif'), format="GTiff", overwrite=TRUE)
  writeRaster(r17,filename=paste0(pheDirYear,'/17_',year,'_',productTable$short_name[17],'.tif'), format="GTiff", overwrite=TRUE)
  writeRaster(r18,filename=paste0(pheDirYear,'/18_',year,'_',productTable$short_name[18],'.tif'), format="GTiff", overwrite=TRUE)
  writeRaster(r19,filename=paste0(pheDirYear,'/19_',year,'_',productTable$short_name[19],'.tif'), format="GTiff", overwrite=TRUE)
  writeRaster(r20,filename=paste0(pheDirYear,'/20_',year,'_',productTable$short_name[20],'.tif'), format="GTiff", overwrite=TRUE)
  writeRaster(r21,filename=paste0(pheDirYear,'/21_',year,'_',productTable$short_name[21],'.tif'), format="GTiff", overwrite=TRUE)
  writeRaster(r22,filename=paste0(pheDirYear,'/22_',year,'_',productTable$short_name[22],'.tif'), format="GTiff", overwrite=TRUE)
  writeRaster(r23,filename=paste0(pheDirYear,'/23_',year,'_',productTable$short_name[23],'.tif'), format="GTiff", overwrite=TRUE)
  writeRaster(r24,filename=paste0(pheDirYear,'/24_',year,'_',productTable$short_name[24],'.tif'), format="GTiff", overwrite=TRUE)
  
}





