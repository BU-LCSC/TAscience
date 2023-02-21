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

list_of_vpd <- list.files(path='/projectnb/modislc/users/twgreen/MODISProject/DayMet/VPD/',
                          recursive = TRUE,
                          pattern = 'vp',
                          full.names = TRUE)

tif <- raster(unlist(gdal_subdatasets(list_of_vpd[j])[4]), 1)
extent <- projectExtent(lct, tif@crs)
croptif <- crop(tif, extent@extent)
#rowTMC <- readRDS('/projectnb/modislc/users/twgreen/MODISProject/RData/spatial/Rast_v2/Tair/s12.rds')

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
rm(tma_values)
rm(tmi_values)

# vps <- 6.11 * 10^((7.5*rowTMC)/(237.3+rowTMC))
# vpsa <- 610.78 * 10^((7.5*rowTMC) / (rowTMC+237.3)) / 1000
# vpsb <- 610.94 * 2.71828^((17.625*rowTMC) / (rowTMC + 243.04)) /1000
vpsc <- 0.61078 * 2.71828^((17.269*cot_values)/(cot_values + 237.8))
vpd_values <- matrix(NA, length(croptif), 365)
for(i in 1:365){
  tif <- raster(unlist(gdal_subdatasets(list_of_vpd[j])[4]), i)
  croptif <- crop(tif, extent@extent)
  vpd_values[,i] <- values(croptif)
  print(i)
}
vpd_values <- vpd_values / 1000
#rowVPD <- rowMeans(vpd_values)
#vpf <- vpsc - rowVPD
vpf <- vpsc - vpd_values
rowVPD <- rowMeans(vpf)
rastvpf <- setValues(croptif, rowVPD)
# plot(rastvpf)
rm(vpd_values)


folder = '/projectnb/modislc/users/twgreen/MODISProject/RData/spatial/Rast_v2/VPD_v2/'
foldercheck <- paste0(folder,tile)
if(!dir.exists(foldercheck)){dir.create(foldercheck)}

saveRDS(rowVPD, file = paste0(folder,tile,'/s',j,'.rds'))
saveRDS(rastvpf, file = paste0(folder,tile,'/r',j,'.rds'))
