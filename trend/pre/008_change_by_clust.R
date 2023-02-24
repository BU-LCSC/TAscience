rm(list = ls())

library(terra)
library(sf)
library(sp)
library(raster)

library(RColorBrewer)

# library(doMC)
# library(doParallel)

############################################################
# From bash code
args <- commandArgs()
print(args)

tt <- as.numeric(substr(args[3],1,3))
# tt <- 1


############################################################
# Get tile list
mcd12q2_path <- '/projectnb/modislc/projects/sat/data/mcd12q/q2/c61'
file <- list.files(path=paste0(mcd12q2_path,'/2001'),full.names=T)
tile_list <- substr(file,74,79)

tile <- tile_list[tt]

print(tile)


## files
path <- '/projectnb/modislc/projects/sat/data/mcd12q/q1/2021.01.01'
LCfile <- list.files(path,pattern=glob2rx(paste0('*',tile,'*.hdf')),full.names=T)
path <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_nor/clust_2pc/map_coarse/map_clust'
CLfile <- list.files(path,pattern=glob2rx(paste0('*',tile,'.tif')),full.names=T)
path <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_nor/vals'
chfile <- list.files(path,pattern=glob2rx(paste0('*',tile,'_nor.rda')),full.names=T)

print(LCfile)
print(CLfile)
print(chfile)

##
sds <- unlist(gdal_subdatasets(LCfile))
lct <- raster(sds[1])
clt <- raster(CLfile)
load(chfile)

outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_nor/chg_val_by_clust'
if (!dir.exists(outDir)) {dir.create(outDir)}

# registerDoMC()
# dat <- foreach(vv=1:25, .combine=rbind) %dopar% {

for(vv in 1:25){

  outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_nor/chg_val_by_clust/',sprintf('%02d',vv))
  if (!dir.exists(outDir)) {dir.create(outDir)}
  
  chval <- setValues(lct,datNor[,vv])
  
  # Reproject and save summation of change
  pr2 <- projectExtent(lct,crs(lct))
  res(pr2) <- 10000
  robin_crs = CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
  pr3 <- projectExtent(lct,robin_crs)
  res(pr3) <- 10000
  
  r1 <- resample(chval,pr2)
  rast1 <- projectRaster(r1,pr3)
  
  Dat <- vector('list',9)
  for(i in 1:9){
    if(length(which(values(clt)==i))>2){
      Dat[[i]] <- rast1[clt==i]  
    }else{
      Dat[[i]] <- NA 
    }
  }
  save(Dat,file=paste0(outDir,'/chg_val_',tile,'.rda'))
  
  print(vv)
}



############################################################
datTable <- vector('list',9)
for(cc in 1:9){
  datT <- matrix(NA,500000,25)
  for(vv in 1:25){
    path <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_nor/chg_val_by_clust/',sprintf('%02d',vv))
    files <- list.files(path,pattern=glob2rx('*.rda'),full.names=T)
    
    dat <- vector('list',315)
    for(i in 1:315){
      load(files[i])
      dat[[i]] <- Dat[[cc]]
    }
    tmp <- na.omit(unlist(dat))
    datT[1:length(tmp),vv] <- tmp
  }
  datTable[[cc]] <- datT
  
  print(cc)
}

save(datTable,file=('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_nor/val_chg_by_clust.rda'))

#
setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/figures/')
png(filename=paste0('chg_val_box.png'),width=10,height=8,units='in',res=300)
mycol <- brewer.pal(9,'Set1')
par(mfrow=c(3,3),oma=c(1,1,0,0),mar=c(4,4,1,1),mgp=c(2,0.8,0))
for(i in c(2,3,7,5,1,9,4,8,6)){
  boxplot(datTable[[i]][,c(2,6,9,14,16,20,23)],outline=F,ylim=c(-2,2),
          # names=c('MGU','Peak','MGD','GSL','EA','EM','GUR','GDR'),
          ylab='Normalized change',cex.lab=1.5,cex.axis=1.2)
          # col=c(mycol[3],mycol[3],mycol[3],mycol[3],mycol[7],mycol[7],mycol[7],mycol[7]))
  abline(h=0)
}
dev.off()


