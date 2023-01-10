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
# tt <- 22


############################################################
# Get tile list
mcd12q2_path <- '/projectnb/modislc/projects/sat/data/mcd12q/q2/c61'
file <- list.files(path=paste0(mcd12q2_path,'/2001'),full.names=T)
tile_list <- substr(file,74,79)

imgBase <- raster(unlist(gdal_subdatasets(file[tt]))[10])

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

slct <- which(values(lct)!=4 & values(lct)!=5 & values(lct)!=8) # non-Forests



############################################################
path <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/metrics/filt_21yrs_nor/',tile_list[tt])
file <- list.files(path=path,pattern=glob2rx('*'),full.names=T)

#
datOrg <- matrix(NA,2400*2400,25)
datNor <- matrix(NA,2400*2400,25)

for(i in 1:25){                           
  load(file[i])                            
  
  datOrg[,i] <- dat2OrgMedi - dat1OrgMedi
  datNor[,i] <- dat2NorMedi - dat1NorMedi
  
  print(i)
}

## Only include pixels having all 25 variables 
# and Forested pixels
tmp1   <- data.frame(1:(2400*2400),datNor)
caseCP <- tmp1[complete.cases(tmp1),1]
caseNC <- c(1:(2400*2400))
caseNC <- caseNC[-caseCP]

datOrg[caseNC,] <- NA
datNor[caseNC,] <- NA
datOrg[slct,] <- NA
datNor[slct,] <- NA

valRow <- cbind(rep(tt,sum(!is.na(datOrg[,1]))),which(!is.na(datOrg[,1]))) # for save

#
datNorAb <- abs(datNor)
datNorAbSum  <- apply(datNorAb,1,sum,na.rm=T)
mapChgNor3  <- setValues(imgBase,datNorAbSum)



############################################################
# save original values
outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/nh/vals'
if (!dir.exists(outDir)) {dir.create(outDir)}
save(datOrg  ,file=paste0(outDir,'/vals_',tile_list[tt],'.rda'))
save(datNor  ,file=paste0(outDir,'/vals_',tile_list[tt],'_nor.rda'))
save(datNorAb,file=paste0(outDir,'/vals_',tile_list[tt],'_nor_ab.rda'))
save(valRow  ,file=paste0(outDir,'/vals_',tile_list[tt],'_validrow.rda'))


# Reproject and save summation of change
pr2 <- projectExtent(imgBase,crs(imgBase))
res(pr2) <- 10000
robin_crs = CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
pr3 <- projectExtent(imgBase,robin_crs)
res(pr3) <- 10000

outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/nh/chg_nor_sum_forest'
if (!dir.exists(outDir)) {dir.create(outDir)}
r1 <- resample(mapChgNor3,pr2)
writeRaster(r1,filename=paste0(outDir,'/chg_nor_sum_',tile_list[tt],'.tif'), format="GTiff", overwrite=TRUE)
rast <- projectRaster(r1,pr3)
writeRaster(rast,filename=paste0(outDir,'/1_chg_nor_sum_',tile_list[tt],'.tif'), format="GTiff", overwrite=TRUE)


# Reproject and save changes in individual variables
for(i in 1:25){
  vv <- sprintf('%02d',i)
  #
  outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/nh/res_org/',vv)
  if (!dir.exists(outDir)) {dir.create(outDir)}
  mapChgNor1 <- setValues(imgBase,datNorAb[,i])
  writeRaster(mapChgNor1,filename=paste0(outDir,'/chg_nor_abs_',tile_list[tt],'.tif'), format="GTiff", overwrite=TRUE)

  #
  outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/nh/res_cor/',vv)
  if (!dir.exists(outDir)) {dir.create(outDir)}
  mapChgNor1[is.na(mapChgNor1)] <- 0
  r1 <- resample(mapChgNor1,pr2)
  rast1 <- projectRaster(r1,pr3)
  writeRaster(rast1,filename=paste0(outDir,'/1_chg_nor_abs_',tile_list[tt],'.tif'), format="GTiff", overwrite=TRUE)
  
  print(i)
}



