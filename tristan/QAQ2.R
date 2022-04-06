library(gdalUtils)
library(raster)
library(rgdal)
library(RColorBrewer)
library(zyp)
library(Kendall)
library(scales)
library(agricolae)
library(profvis)
library(ncdf4)
library(doMC)


UnpackDetailedQA <- function(x){
  bits <- as.integer(intToBits(x))
  quals <- sapply(seq(1, 16, by=2), function(i) sum(bits[i:(i+1)] * 2^c(0, 1)))[1:7]
  return(quals)
}

UnpackDetailedQAMidGuP <- function(x){
  bits <- as.integer(intToBits(x))
  quals <- sapply(seq(1, 16, by=2), function(i) sum(bits[i:(i+1)] * 2^c(0, 1)))[2]
  return(quals)
}

list_of_files <- list.files(path='/projectnb/modislc/projects/sat/data/mcd12q/q2/',
                            recursive = TRUE,
                            pattern = 'h10v03',
                            full.names = TRUE)

tile = 'h10v03'
year = c(2019, 2020, 2021)
mcd12a4_path <- paste('/projectnb/modislc/projects/sat/data/e4ftl01.cr.usgs.gov/MOTA/MCD43A4.006')
search_str <- paste('*43A4.A',year,'*.',tile,'.006*',sep='')
files <- list.files(path=mcd12a4_path, pattern=glob2rx(search_str),full.names=T,include.dirs=F,recursive=T)

test <- raster(get_subdatasets(files[1])[[1]])


getQAvaluespoint <- function(tile,lat,lon){
  mcd12a2_path <- paste('/projectnb/modislc/projects/sat/data/e4ftl01.cr.usgs.gov/MOTA/MCD43A2.006') #Path for A4 directory
  search_str <- paste('*43A2.A',2015,'*.',tile,'.006*',sep='') #Search for files with specific year and tile
  files <- list.files(path=mcd12a2_path, pattern=glob2rx(search_str),full.names=T,include.dirs=F,recursive=T) #List those files
  
  #Make selection of lon lat
  points <- cbind(lon,lat)
  v <- SpatialPoints(points, proj4string = CRS("+proj=longlat +datum=WGS84")) #Put points in reference lat/lon
  y <- spTransform(v, CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs")) #Put in modis projection
  
  #Grab sample modis raster
  modisrast <- raster(get_subdatasets(files[1])[1])
  
  #Make blank raster in order to grab lat/lon coords in sinu proj
  numPix <- length(modisrast)
  imgNum <- setValues(test, 1:numPix)
  z <- extract(imgNum, y)
  
  qualQApoint <- matrix(NA, 19, 7)
  registerDoMC()
  for (i in 1:19){
    vv <- get_subdatasets(list_of_files[i])
    rr <- raster(vv[13])
    
    qualQApoint[i,] <- UnpackDetailedQA(rr[z])
    print(i)
  }
  qualQAdfpoint <- as.data.frame(qualQApoint)
  colnames(qualQAdfpoint) <- c("Greenup", "MidGreenup", "Maturity", "Peak", "Senescence", "MidGreendown", "Dormancy")
  
  return(qualQAdfpoint)
}
getQAvaluespointMidGup <- function(tile,lat,lon){
  mcd12a2_path <- paste('/projectnb/modislc/projects/sat/data/e4ftl01.cr.usgs.gov/MOTA/MCD43A2.006') #Path for A4 directory
  search_str <- paste('*43A2.A',2015,'*.',tile,'.006*',sep='') #Search for files with specific year and tile
  files <- list.files(path=mcd12a2_path, pattern=glob2rx(search_str),full.names=T,include.dirs=F,recursive=T) #List those files
  
  #Make selection of lon lat
  points <- cbind(lon,lat)
  v <- SpatialPoints(points, proj4string = CRS("+proj=longlat +datum=WGS84")) #Put points in reference lat/lon
  y <- spTransform(v, CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs")) #Put in modis projection
  
  #Grab sample modis raster
  modisrast <- raster(get_subdatasets(files[1])[1])
  
  #Make blank raster in order to grab lat/lon coords in sinu proj
  numPix <- length(modisrast)
  imgNum <- setValues(test, 1:numPix)
  z <- extract(imgNum, y)
  
  qualQApoint <- matrix(NA, 19, 1)
  registerDoMC()
  for (i in 1:19){
    vv <- get_subdatasets(list_of_files[i])
    rr <- raster(vv[13])
    
    qualQApoint[i,] <- UnpackDetailedQAMidGuP(rr[z])
    print(i)
  }
  qualQAdfpoint <- as.data.frame(qualQApoint)
  colnames(qualQAdfpoint) <- c("MidGreenup")
  
  return(qualQAdfpoint)
}
get50guppoint <- function(tile, year, lat, lon){
  #Grab initial Paths to MODIS A4 directory product
  mcd12a4_path <- paste('/projectnb/modislc/projects/sat/data/e4ftl01.cr.usgs.gov/MOTA/MCD43A4.006') #Path for A4 directory
  search_str <- paste('*43A4.A',year,'*.',tile,'.006*',sep='') #Search for files with specific year and tile
  files <- list.files(path=mcd12a4_path, pattern=glob2rx(search_str),full.names=T,include.dirs=F,recursive=T) #List those files
  
  #Make selection of lon lat
  points <- cbind(lon,lat)
  v <- SpatialPoints(points, proj4string = CRS("+proj=longlat +datum=WGS84")) #Put points in reference lat/lon
  y <- spTransform(v, CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs")) #Put in modis projection
  
  #Grab sample modis raster
  modisrast <- raster(get_subdatasets(files[1])[1])
  
  #Make blank raster in order to grab lat/lon coords in sinu proj
  numPix <- length(modisrast)
  imgNum <- setValues(test, 1:numPix)
  z <- extract(imgNum, y)
  
  #Have to load in values for 50GUP for tile
  gupdates <- mat_values50up[z,]
  
  gupdf <- as.data.frame(gupdates)
  
  year <- c('2001','2002','2003','2004','2005','2006','2007','2008','2009','2010','2011','2012','2013','2014','2015','2016','2017','2018','2019')
  
  gupdf$year <- year
  
  return(gupdf)
}

#testb <- getQAvaluespoint(tile,51.54944,-126.6731)
#testc <- getQAvaluespointMidGup(tile, 51.54944,-126.6731)
QAtsp1 <- getQAvaluespointMidGup(tile, 51.54944,-126.6731)
QAtsp2 <- getQAvaluespointMidGup(tile, 53.5983,-122.15645)
QAtsp3 <- getQAvaluespointMidGup(tile, 53.65296,-122.41896)
QAtsp4 <- getQAvaluespointMidGup(tile, 52.34711,-121.64402)
QAtsp5 <- getQAvaluespointMidGup(tile, 55.02515,-128.21469)
QAtsp6 <- getQAvaluespointMidGup(tile, 54.21601,-122.63162)
QAtsp7 <- getQAvaluespointMidGup(tile, 59.39848,-154.78781)
QAtsp8 <- getQAvaluespointMidGup(tile, 56.74000, -129.32505)
QAtsp9 <- getQAvaluespointMidGup(tile, 50.56473, -114.04442)
QAtsp10 <- getQAvaluespointMidGup(tile, 52.48991, -120.97775)


gupp1 <- get50guppoint(tile, 2019, 51.54944,-126.6731)
gupp2 <- get50guppoint(tile, 2019,53.5983,-122.15645)
gupp3 <- get50guppoint(tile, 2019, 53.65296,-122.41896)
gupp4 <- get50guppoint(tile, 2019, 52.34711,-121.64402)
gupp5 <- get50guppoint(tile, 2019, 55.02515,-128.21469)
gupp6 <- get50guppoint(tile, 2019, 54.21601,-122.63162)
gupp7 <- get50guppoint(tile, 2019, 59.39848,-154.78781)
gupp8 <- get50guppoint(tile, 2019, 56.74000, -129.32505)
gupp9 <- get50guppoint(tile, 2019, 50.56473, -114.04442)
gupp10 <- get50guppoint(tile, 2019, 52.48991, -120.97775)


qualQApoint <- matrix(NA, (2400*2400), 7)
vv <- get_subdatasets(list_of_files[i])
rr <- raster(vv[13])
registerDoMC()
for (i in 1:(2400*2400)){
  qualQApoint[i,] <- UnpackDetailedQA(rr[i])
  print(i)
}






