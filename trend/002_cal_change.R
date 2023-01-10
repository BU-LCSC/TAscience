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

# slct <- which(values(lct)==4|values(lct)==5) # Forests



############################################################
path <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/metrics/filt_21yrs_nor/',tile_list[tt])
file <- list.files(path=path,pattern=glob2rx('*'),full.names=T)

#
datOrg <- matrix(NA,2400*2400,25)
datNor <- matrix(NA,2400*2400,25)
# datOrg <- matrix(NA,120*120,6)
# datNor <- matrix(NA,120*120,6)

for(i in 1:25){                           # all
# for(i in 1:6){
#   vv <- c(2,6,9,13,14,15)                 # key
# for(i in 1:12){                           # phe  
# for(i in 1:7){                            # evi
#   vv <- 13:19
  # i <- 15                                 # evi_area 
  # i <- 2                                  # mid_greenup 
  # i <- 14                                 # evi_amp
  
  # load(file[vv[i]])                            
  load(file[i])                            
  
  datOrg[,i] <- dat2OrgMedi - dat1OrgMedi
  datNor[,i] <- dat2NorMedi - dat1NorMedi
  # datOrg[slct,i] <- dat2OrgMedi[slct] - dat1OrgMedi[slct]
  # datNor[slct,i] <- dat2NorMedi[slct] - dat1NorMedi[slct]
  
  print(i)
}

## Only include pixels having all 25 variables 
tmp1   <- data.frame(1:(2400*2400),datNor)
caseCP <- tmp1[complete.cases(tmp1),1]
valRow <- cbind(rep(tt,length(caseCP)),caseCP) # for save
caseNC <- c(1:(2400*2400))
caseNC <- caseNC[-caseCP]

datOrg[caseNC,] <- NA
datNor[caseNC,] <- NA


# #
# datOrgSD <- apply(datOrg,1,sd,na.rm=T)
# datNorSD <- apply(datNor,1,sd,na.rm=T)
# 
# datOrgAb <- abs(datOrg)
datNorAb <- abs(datNor)
# datOrgAbMean <- apply(datOrgAb,1,mean,na.rm=T)
# datOrgAbSum  <- apply(datOrgAb,1,sum,na.rm=T)
# datNorAbMean <- apply(datNorAb,1,mean,na.rm=T)
# datNorAbMedi <- apply(datNorAb,1,median,na.rm=T)
datNorAbSum  <- apply(datNorAb,1,sum,na.rm=T)
# 
# ## Set to map
# # imgBase <- panel_grid
# 
# mapChgOrg1 <- setValues(imgBase,datOrgSD)
# mapChgOrg2 <- setValues(imgBase,datOrgAbMean)
# mapChgOrg3 <- setValues(imgBase,datOrgAbSum)
# mapChgNor1 <- setValues(imgBase,datNorSD)
# mapChgNor2  <- setValues(imgBase,datNorAbMean)
# mapChgNor20 <- setValues(imgBase,datNorAbMedi)
mapChgNor3  <- setValues(imgBase,datNorAbSum)



# ############################################################
# # Reproject
# pr3 <- projectExtent(imgBase,crs(imgBase))
# res(pr3) <- 10000
# r1 <- resample(mapChgOrg1,pr3)
# r2 <- resample(mapChgOrg2,pr3)
# r3 <- resample(mapChgOrg3,pr3)
# r4 <- resample(mapChgNor1,pr3)
# r5 <- resample(mapChgNor2,pr3)
# r6 <- resample(mapChgNor3,pr3)
# r7 <- resample(mapChgOrg0,pr3)
# 
# geog_crs = CRS("+proj=longlat +datum=WGS84")
# pr3 <- projectExtent(imgBase,geog_crs)
# res(pr3) <- 0.1   
# rast1 <- projectRaster(r1,pr3)  
# rast2 <- projectRaster(r2,pr3)  
# rast3 <- projectRaster(r3,pr3)  
# rast4 <- projectRaster(r4,pr3)  
# rast5 <- projectRaster(r5,pr3)  
# rast6 <- projectRaster(r6,pr3)  
# # rast7 <- projectRaster(r7,pr3)  


############################################################
# Save
# ptVari <- c('01_mgu','02_mgd','03_gsl','04_eviMin','05_eviAmp','06_eviAre')
# outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_01_10by10/lct_all/geog_key_each/',ptVari[ii])
# if (!dir.exists(outDir)) {dir.create(outDir)}  


# outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_nor/vals'
# if (!dir.exists(outDir)) {dir.create(outDir)}
# save(datOrg  ,file=paste0(outDir,'/vals_',tile_list[tt],'.rda'))
# save(datNor  ,file=paste0(outDir,'/vals_',tile_list[tt],'_nor.rda'))
# save(datNorAb,file=paste0(outDir,'/vals_',tile_list[tt],'_nor_ab.rda'))
# save(valRow  ,file=paste0(outDir,'/vals_',tile_list[tt],'_validrow.rda'))

# Reproject
pr2 <- projectExtent(imgBase,crs(imgBase))
res(pr2) <- 10000
robin_crs = CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
pr3 <- projectExtent(imgBase,robin_crs)
res(pr3) <- 10000

outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_nor/chg_nor_sum'
if (!dir.exists(outDir)) {dir.create(outDir)}
r1 <- resample(mapChgNor3,pr2)
writeRaster(r1,filename=paste0(outDir,'/chg_nor_sum_',tile_list[tt],'.tif'), format="GTiff", overwrite=TRUE)
rast <- projectRaster(r1,pr3)
writeRaster(rast,filename=paste0(outDir,'/1_chg_nor_sum_',tile_list[tt],'.tif'), format="GTiff", overwrite=TRUE)

# outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_nor/chg_nor_mean'
# if (!dir.exists(outDir)) {dir.create(outDir)}
# r1 <- resample(mapChgNor2,pr2)
# writeRaster(r1,filename=paste0(outDir,'/chg_nor_mean_',tile_list[tt],'.tif'), format="GTiff", overwrite=TRUE)  
# rast <- projectRaster(r1,pr3)
# writeRaster(rast,filename=paste0(outDir,'/1_chg_nor_mean_',tile_list[tt],'.tif'), format="GTiff", overwrite=TRUE)

# outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_nor/chg_nor_medi'
# if (!dir.exists(outDir)) {dir.create(outDir)}
# r1 <- resample(mapChgNor20,pr2)
# writeRaster(r1,filename=paste0(outDir,'/chg_nor_medi_',tile_list[tt],'.tif'), format="GTiff", overwrite=TRUE)  
# rast <- projectRaster(r1,pr3)
# writeRaster(rast,filename=paste0(outDir,'/1_chg_nor_medi_',tile_list[tt],'.tif'), format="GTiff", overwrite=TRUE)



for(i in 1:25){
  vv <- sprintf('%02d',i)
  #
  outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_nor/res_org/',vv)
  if (!dir.exists(outDir)) {dir.create(outDir)}
  mapChgOrg0 <- setValues(imgBase,datOrg[,i])
  # writeRaster(mapChgOrg0,filename=paste0(outDir,'/chg_org_',tile_list[tt],'.tif'), format="GTiff", overwrite=TRUE)
  mapChgNor0 <- setValues(imgBase,datNor[,i])
  # writeRaster(mapChgNor0,filename=paste0(outDir,'/chg_nor_',tile_list[tt],'.tif'), format="GTiff", overwrite=TRUE)
  mapChgNor1 <- setValues(imgBase,datNorAb[,i])
  writeRaster(mapChgNor1,filename=paste0(outDir,'/chg_nor_abs_',tile_list[tt],'.tif'), format="GTiff", overwrite=TRUE)

  #
  outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_21yrs_nor/res_cor/',vv)
  if (!dir.exists(outDir)) {dir.create(outDir)}
  r1 <- resample(mapChgOrg0,pr2)
  rast1 <- projectRaster(r1,pr3)
  # writeRaster(rast1,filename=paste0(outDir,'/1_chg_org_',tile_list[tt],'.tif'), format="GTiff", overwrite=TRUE)
  r1 <- resample(mapChgNor0,pr2)
  rast1 <- projectRaster(r1,pr3)
  # writeRaster(rast1,filename=paste0(outDir,'/1_chg_nor_',tile_list[tt],'.tif'), format="GTiff", overwrite=TRUE)
  mapChgNor1[is.na(mapChgNor1)] <- 0
  r1 <- resample(mapChgNor1,pr2)
  rast1 <- projectRaster(r1,pr3)
  writeRaster(rast1,filename=paste0(outDir,'/1_chg_nor_abs_',tile_list[tt],'.tif'), format="GTiff", overwrite=TRUE)
  
  print(i)
}



# outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_01_10by10/lct_all/geog_key_10pc/01')
# if (!dir.exists(outDir)) {dir.create(outDir)}  
# writeRaster(rast1,filename=paste0(outDir,'/01_chg_org_sd_',tile_list[tt],'.tif'), format="GTiff", overwrite=TRUE)
# outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_01_10by10/lct_all/geog_key_10pc/02')
# if (!dir.exists(outDir)) {dir.create(outDir)}  
# writeRaster(rast2,filename=paste0(outDir,'/02_chg_org_mn_',tile_list[tt],'.tif'), format="GTiff", overwrite=TRUE)
# outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_01_10by10/lct_all/geog_key_10pc/03')
# if (!dir.exists(outDir)) {dir.create(outDir)}  
# writeRaster(rast3,filename=paste0(outDir,'/03_chg_org_sm_',tile_list[tt],'.tif'), format="GTiff", overwrite=TRUE)
# outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_01_10by10/lct_all/geog_key_10pc/04')
# if (!dir.exists(outDir)) {dir.create(outDir)}  
# writeRaster(rast4,filename=paste0(outDir,'/04_chg_nor_sd_',tile_list[tt],'.tif'), format="GTiff", overwrite=TRUE)
# outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_01_10by10/lct_all/geog_key_10pc/05')
# if (!dir.exists(outDir)) {dir.create(outDir)}  
# writeRaster(rast5,filename=paste0(outDir,'/05_chg_nor_mn_',tile_list[tt],'.tif'), format="GTiff", overwrite=TRUE)
# outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_01_10by10/lct_all/geog_key_10pc/06')
# if (!dir.exists(outDir)) {dir.create(outDir)}  
# writeRaster(rast6,filename=paste0(outDir,'/06_chg_nor_sm_',tile_list[tt],'.tif'), format="GTiff", overwrite=TRUE)
# outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/filt_01_10by10/lct_all/geog_key/07')
# if (!dir.exists(outDir)) {dir.create(outDir)}  
# writeRaster(rast7,filename=paste0(outDir,'/06_chg_org_og_',tile_list[tt],'.tif'), format="GTiff", overwrite=TRUE)





# ############################################################  
# # save
# # outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/',tile_list[tt])
# # if (!dir.exists(outDir)) {dir.create(outDir)}  
# 
# outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/sinu/01'
# writeRaster(mapChgOrg1,filename=paste0(outDir,'/01_chg_org_sd_',tile_list[tt],'.tif'), format="GTiff", overwrite=TRUE)
# outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/sinu/02'
# writeRaster(mapChgOrg2,filename=paste0(outDir,'/02_chg_org_mn_',tile_list[tt],'.tif'), format="GTiff", overwrite=TRUE)
# outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/sinu/03'
# writeRaster(mapChgOrg3,filename=paste0(outDir,'/03_chg_org_sm_',tile_list[tt],'.tif'), format="GTiff", overwrite=TRUE)
# outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/sinu/04'
# writeRaster(mapChgNor1,filename=paste0(outDir,'/04_chg_nor_sd_',tile_list[tt],'.tif'), format="GTiff", overwrite=TRUE)
# outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/sinu/05'
# writeRaster(mapChgNor2,filename=paste0(outDir,'/05_chg_nor_mn_',tile_list[tt],'.tif'), format="GTiff", overwrite=TRUE)
# outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/sinu/06'
# writeRaster(mapChgNor3,filename=paste0(outDir,'/06_chg_nor_sm_',tile_list[tt],'.tif'), format="GTiff", overwrite=TRUE)
