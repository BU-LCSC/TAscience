library(easypackages)
libraries('raster','sp', 'sf', 'terra', 'ncdf4','dplyr','plyr')

args <- commandArgs()
print(args)

tile <- substr(args[3],1,6)
year <- as.numeric(substr(args[3],7,10))

#change
j <- year - 2000

mat_lct <- matrix(NA,(2400*2400),1)

list_of_lct <- list.files(path='/projectnb/modislc/projects/sat/data/mcd12q/q1a/',
                          recursive = TRUE,
                          pattern = tile,
                          full.names = TRUE)

sds <- unlist(gdal_subdatasets(list_of_lct[j]))
lct <- raster(sds[1])
mat_lct[,1] <- values(lct)

list_of_dyl <- list.files(path='/projectnb/modislc/users/twgreen/MODISProject/DayMet/DayL/',
                          recursive = TRUE,
                          pattern = 'day',
                          full.names = TRUE)


list_of_swi <- list.files(path='/projectnb/modislc/users/twgreen/MODISProject/DayMet/SW_in/',
                          recursive = TRUE,
                          pattern = 'srad',
                          full.names = TRUE)

tif <- raster(unlist(gdal_subdatasets(list_of_swi[j])[4]), 1)
extent <- projectExtent(lct, tif@crs)
croptif <- crop(tif, extent@extent)

dal_values <- matrix(NA, length(croptif), 365)
for(i in 1:365){
  tif <- raster(unlist(gdal_subdatasets(list_of_dyl[j])[4]), i)
  croptif <- crop(tif, extent@extent)
  dal_values[,i] <- values(croptif)
  print(i)
}
dal_values <- dal_values / 86400

swi_values <- matrix(NA, length(croptif), 365)
for(i in 1:365){
  tif <- raster(unlist(gdal_subdatasets(list_of_swi[j])[4]), i)
  croptif <- crop(tif, extent@extent)
  swi_values[,i] <- values(croptif)
  print(i)
}
swn_values <- swi_values * dal_values
rowSWN <- rowMeans(swn_values)
rastswn <- setValues(croptif, rowSWN)
# plot(rastswn)
rm(dal_values)
rm(swi_values)
rm(swn_values)

folder = '/projectnb/modislc/users/twgreen/MODISProject/RData/spatial/Rast_v2/SWin/'
foldercheck <- paste0(folder,tile)
if(!dir.exists(foldercheck)){dir.create(foldercheck)}

saveRDS(rowSWN, file = paste0(folder,tile,'/s',j,'.rds'))
saveRDS(rastswn, file = paste0(folder,tile,'/r',j,'.rds'))

