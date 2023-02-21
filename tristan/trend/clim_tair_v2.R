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


list_of_tma <- list.files(path='/projectnb/modislc/users/twgreen/MODISProject/DayMet/TMax/',
                          recursive = TRUE,
                          pattern = 'tmax',
                          full.names = TRUE)

list_of_tmi <- list.files(path='/projectnb/modislc/users/twgreen/MODISProject/DayMet/TMin/',
                          recursive = TRUE,
                          pattern = 'tmin',
                          full.names = TRUE)

tif <- raster(unlist(gdal_subdatasets(list_of_tma[j])[4]), 1)
extent <- projectExtent(lct, tif@crs)
croptif <- crop(tif, extent@extent)

tma_values <- matrix(NA, length(croptif), 365)
for(i in 1:365){
  tif <- raster(unlist(gdal_subdatasets(list_of_tma[j])[4]), i)
  croptif <- crop(tif, extent@extent)
  tma_values[,i] <- values(croptif)
  print(i)
}

tmi_values <- matrix(NA, length(croptif), 365)
for(i in 1:365){
  tif <- raster(unlist(gdal_subdatasets(list_of_tmi[j])[4]), i)
  croptif <- crop(tif, extent@extent)
  tmi_values[,i] <- values(croptif)
  print(i)
}
cot_values <- (tma_values + tmi_values) / 2
rowTMC <- rowMeans(cot_values)
rasttmc <- setValues(croptif, rowTMC)
# plot(rasttmc)
rm(tma_values)
rm(tmi_values)
rm(cot_values)

folder = '/projectnb/modislc/users/twgreen/MODISProject/RData/spatial/Rast_v2/Tair/'
foldercheck <- paste0(folder,tile)
if(!dir.exists(foldercheck)){dir.create(foldercheck)}

saveRDS(rowTMC, file = paste0(folder,tile,'/s',j,'.rds'))
saveRDS(rasttmc, file = paste0(folder,tile,'/r',j,'.rds'))

