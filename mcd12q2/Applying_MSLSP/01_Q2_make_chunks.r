library(raster)
library(rgdal)
library(gdalUtils)

library(rjson)
library(geojsonR)

library(doMC)
library(doParallel)

########################################
args <- commandArgs()
print(args)

tile <- substr(args[3],1,6)
year <- as.numeric(substr(args[3],7,10))
# tile <- 'h10v03'; year <- 2016

print(tile)
print(year)

########################################
params <- fromJSON(file='/usr3/graduate/mkmoon/GitHub/TAscience/mcd12q2/Applying_MSLSP/Q2_Parameters.json')
source(params$setup$rFunctions)

outDir <- paste0(params$setup$workDir,tile)
if (!dir.exists(outDir)) {dir.create(outDir)}


########################################
imgDir <- paste0(params$setup$dataDir)
a41 <- list.files(path=paste0(imgDir,'/mcd43a4_link/',(year-1)),pattern=glob2rx(paste0('*',tile,'*.hdf')),recursive=T,full.names=T)
a42 <- list.files(path=paste0(imgDir,'/mcd43a4_link/',(year-0)),pattern=glob2rx(paste0('*',tile,'*.hdf')),recursive=T,full.names=T)
a43 <- list.files(path=paste0(imgDir,'/mcd43a4_link/',(year+1)),pattern=glob2rx(paste0('*',tile,'*.hdf')),recursive=T,full.names=T)
a4files  <- c(a41,a42,a43)

# base image
imgBase <- raster(get_subdatasets(a4files[1])[[1]])

numCk <- params$setup$numChunks
chunk <- length(imgBase)%/%numCk

# Output directory
ckDir <- paste0(outDir,'/chunks'); if (!dir.exists(ckDir)) {dir.create(ckDir)}
ckDirY <- paste0(ckDir,'/',year); if (!dir.exists(ckDirY)) {dir.create(ckDirY)}

# Directory for temporal outputs
ckDirTemp <- paste0(ckDirY,'/temp/')
if (!dir.exists(ckDirTemp)) {dir.create(ckDirTemp)}



########################################
# For each image corresponded to each date,
# divede images into chunks, and save them as temporal files
registerDoMC(params$setup$numCores)

foreach(i=1:length(a4files)) %dopar%{
  a4file <- a4files[i]
  yd <- as.numeric(substr(unlist(strsplit(a4file,'/'))[11],10,16))
  a2file <- list.files(path=paste0(imgDir,'mcd43a2_link/',substr(yd,1,4),'/',substr(yd,5,7)),pattern=glob2rx(paste0('MCD43A2.A',yd,'.',tile,'*.hdf')),full.names=T)
  
  sds4 <- get_subdatasets(a4file)
  sds2 <- get_subdatasets(a2file)
  
  band1 <- values(raster(sds4[8]))
  band2 <- values(raster(sds4[9])) 
  band4 <- values(raster(sds4[11])) 
  band6 <- values(raster(sds4[13])) 
  bsnow <- values(raster(sds2[1])) 
  
  foreach(cc=1:numCk) %dopar%{
    ckNum <- sprintf('%03d',cc)
    dirTemp <- paste0(ckDirTemp,ckNum)
    if (!dir.exists(dirTemp)) {dir.create(dirTemp)}
    
    if(cc==numCk){
      chunks <- c((chunk*(cc-1)+1):length(imgBase))
    }else{
      chunks <- c((chunk*(cc-1)+1):(chunk*cc))
    }
    b1 <- band1[chunks]
    b2 <- band2[chunks]
    b4 <- band4[chunks]
    b6 <- band6[chunks]
    bs <- bsnow[chunks]
    
    save(b1,b2,b4,b6,bs,yd,file=paste0(dirTemp,'/',yd,'.rda'))
  }
}


########################################
# Load files for each chunk, merge then, and save 
foreach(cc=1:numCk) %dopar%{
  ckNum <- sprintf('%03d',cc)
  dirTemp <- paste0(ckDirTemp,ckNum)
  files <- list.files(dirTemp,full.names=T)
  
  if(cc==numCk){
    chunks <- c((chunk*(cc-1)+1):length(imgBase))
  }else{
    chunks <- c((chunk*(cc-1)+1):(chunk*cc))
  }
  
  if(length(files)==(365*3)){
    band1 <- matrix(NA,length(chunks),length(files))
    band2 <- matrix(NA,length(chunks),length(files))
    band4 <- matrix(NA,length(chunks),length(files))
    band6 <- matrix(NA,length(chunks),length(files))
    bsnow <- matrix(NA,length(chunks),length(files))
    for(i in 1:length(files)){
      load(files[i])
      
      band1[,i] <- b1
      band2[,i] <- b2
      band4[,i] <- b4
      band6[,i] <- b6
      bsnow[,i] <- bs
    }
    # Save
    save(band1,band2,band4,band6,bsnow,
         file=paste0(ckDirY,'/chunk_',ckNum,'.rda'))
  }
}
  
  
########################################
## Remove temporary files
system(paste0('rm -r ',ckDirTemp))



