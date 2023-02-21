library(easypackages)
libraries('rgdal','raster','sp', 'terra', 'sf', 'ncdf4','dplyr','plyr', 'data.table')

getMODIStileFromLonLat <- function(Lon,Lat){
  
  # MODIS tiles shapefile
  MODIStiles <- readOGR('/usr3/graduate/twgreen/CSVS_SHPS/MODISGrid/modis_sinusoidal_grid_world.shp')
  proj4string(MODIStiles) <- CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")
  
  # Creae point shapefile
  pCoor   <- data.frame(1,Lon,Lat)
  lonlat  <- pCoor[,c(2,3)]
  pShp    <- SpatialPointsDataFrame(coords = lonlat,
                                    data = pCoor,
                                    proj4string = CRS("+proj=longlat +datum=WGS84"))
  pShp    <- spTransform(pShp,crs(MODIStiles))
  
  # Grab MODIS tile including the point location
  #MODIStile <- intersect(pShp,MODIStiles)
  MODIStile <- MODIStiles[pShp, ]
  h <- sprintf('%02d',as.numeric(as.character(MODIStile$h)))
  v <- sprintf('%02d',as.numeric(as.character(MODIStile$v)))
  tile <- paste0('h',h,'v',v)
  
  return(tile)
}

files <- list.files(path='/projectnb/modislc/users/twgreen/MODISProject/Am_fluxnet/dbfmf/', 
                     recursive = TRUE,
                     pattern = 'FULLSET_YY',
                     full.names = TRUE)
met <- read.csv('/projectnb/modislc/users/twgreen/MODISProject/Am_fluxnet/dbfmf/latlonsite.csv')
lsmod <- vector('list', length(files))
for(k in 1:length(files)){
  tryCatch( {
  sitepath <- files[k]
  sitedf <- read.csv(sitepath, na.strings = c('-9999'))    
  sitenm <- substr(sitepath, 65, 74)
  Year <- as.numeric(substr(sitedf$TIMESTAMP, 1, 4))
  inddf <- data.frame(sitenm, Year)
  filtdf <- filter(inddf, Year > 2000)
  stYear <- filtdf$Year[1]
  edYear <- filtdf$Year[unique(length(filtdf$Year))]
  sitech <- met[k,]
  tile <- getMODIStileFromLonLat(sitech$Long,sitech$Lat)
  list_of_lct <- list.files(path='/projectnb/modislc/projects/sat/data/mcd12q/q1a/',
                            recursive = TRUE,
                            pattern = tile,
                            full.names = TRUE)
  list_of_phe <- list.files(path='/projectnb/modislc/projects/sat/data/mcd12q/q2/',
                            recursive = TRUE,
                            pattern = tile,
                            full.names = TRUE)
  points <- cbind(sitech$Long,sitech$Lat)
  vz <- SpatialPoints(points, proj4string = CRS("+proj=longlat +datum=WGS84")) #tell its in lat long crs
  yz <- spTransform(vz, CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs")) #convert to sinusodal for MODIS
  numPix <- length(lct)
  imgNum <- setValues(lct, 1:numPix)
  z33 <- raster::extract(imgNum, yz, buffer=850)
  
  mat_lct <- matrix(NA,(2400*2400),length(unique(filtdf$Year)))
  mat_gsl <- matrix(NA,(2400*2400),length(unique(filtdf$Year)))
  mat_gup <- matrix(NA,(2400*2400),length(unique(filtdf$Year)))
  mat_gdw <- matrix(NA,(2400*2400),length(unique(filtdf$Year)))
  mat_15u <- matrix(NA,(2400*2400),length(unique(filtdf$Year)))
  mat_15d <- matrix(NA,(2400*2400),length(unique(filtdf$Year)))
  mat_peak <- matrix(NA,(2400*2400),length(unique(filtdf$Year)))
  mat_eviamp <- matrix(NA,(2400*2400),length(unique(filtdf$Year)))
  mat_evimax <- matrix(NA,(2400*2400),length(unique(filtdf$Year)))
  mat_evimin <- matrix(NA,(2400*2400),length(unique(filtdf$Year)))
  mat_eviarea <- matrix(NA,(2400*2400),length(unique(filtdf$Year)))
  
  for(i in stYear:edYear){
  j <- i - 2000
  s <- i + 1 - stYear
  sds <- unlist(gdal_subdatasets(list_of_lct[j]))
  lct <- raster(sds[1])
  mat_lct[,s] <- values(lct)
  
  doy_offset <- as.integer(as.Date(paste((i-1),'-12-31',sep='')) - as.Date("1970-1-1"))
  
  sds <- unlist(gdal_subdatasets(list_of_phe[j+19]))
  te <- raster(sds[3])
  values(te)[values(te)>30000] <- NA
  te <- te - doy_offset
  mat_gup[,s] <- values(te)
  
  te <- raster(sds[2])
  values(te)[values(te)>30000] <- NA
  te <- te - doy_offset
  mat_15u[,s] <- values(te)
  
  te <- raster(sds[8])
  values(te)[values(te)>30000] <- NA
  te <- te - doy_offset
  mat_15d[,s] <- values(te)
  
  te <- raster(sds[4])
  values(te)[values(te)>30000] <- NA
  te <- te - doy_offset
  mat_peak[,s] <- values(te)
  
  te <- raster(sds[7])
  values(te)[values(te)>30000] <- NA
  te <- te - doy_offset
  mat_gdw[,s] <- values(te)
  
  te <- raster(sds[9])
  mat_evimin[,s] <- values(te)
  
  te <- raster(sds[10])
  mat_eviamp[,s] <- values(te)
  
  te <- raster(sds[11])
  mat_eviarea[,s] <- values(te)
  
  mat_gsl <- mat_gdw - mat_gup
  mat_evimax <- mat_eviamp - mat_evimin
  print(i)
  }
  
  LCTa <- matrix(NA, length(unique(filtdf$Year)), length(z33[[1]]))
  GUP <- matrix(NA, length(unique(filtdf$Year)), length(z33[[1]]))
  GDW <- matrix(NA, length(unique(filtdf$Year)), length(z33[[1]]))
  gsl <- matrix(NA, length(unique(filtdf$Year)), length(z33[[1]]))
  GUP15 <- matrix(NA, length(unique(filtdf$Year)), length(z33[[1]]))
  GDW15 <- matrix(NA, length(unique(filtdf$Year)), length(z33[[1]]))
  EVIAmp <- matrix(NA, length(unique(filtdf$Year)), length(z33[[1]]))
  EVIMax <- matrix(NA, length(unique(filtdf$Year)), length(z33[[1]]))
  EVIAre <- matrix(NA, length(unique(filtdf$Year)), length(z33[[1]]))
  Peak <- matrix(NA, length(unique(filtdf$Year)), length(z33[[1]]))
  
  for (f in 1:length(z33[[1]])){
    LCTa[,f] <- mat_lct[z33[[1]][f],]
    GUP[,f] <- mat_gup[z33[[1]][f],]
    GDW[,f] <- mat_gdw[z33[[1]][f],]
    gsl[,f] <- mat_gsl[z33[[1]][f],]
    GUP15[,f] <- mat_15u[z33[[1]][f],]
    GDW15[,f] <- mat_15d[z33[[1]][f],]
    EVIAmp[,f] <- mat_eviamp[z33[[1]][f],]
    EVIMax[,f] <- mat_evimax[z33[[1]][f],]
    EVIAre[,f] <- mat_eviarea[z33[[1]][f],]
    Peak[,f] <- mat_peak[z33[[1]][f],]
  }
  
  LCTa[LCTa != 4 & LCTa != 5] <- NA
  GUP[is.na(LCTa)] <- NA
  GDW[is.na(LCTa)] <- NA
  gsl[is.na(LCTa)] <- NA
  GUP15[is.na(LCTa)] <- NA
  GDW15[is.na(LCTa)] <- NA
  EVIAmp[is.na(LCTa)] <- NA
  EVIMax[is.na(LCTa)] <- NA
  EVIAre[is.na(LCTa)] <- NA
  Peak[is.na(LCTa)] <- NA
  
  GUPdf <- as.data.frame(GUP)
  GUPdf <- apply(GUPdf, 1, median, na.rm=T)
  GDWdf <- as.data.frame(GDW)
  GDWdf <- apply(GDWdf, 1, median, na.rm=T)
  GSLdf <- as.data.frame(gsl)
  GSLdf <- apply(GSLdf, 1, median, na.rm=T)
  GUP15df <- as.data.frame(GUP15)
  GUP15df <- apply(GUP15df, 1, median, na.rm=T)
  GDW15df <- as.data.frame(GDW15)
  GDW15df <- apply(GDW15df, 1, median, na.rm=T)
  EVIAmpdf <- as.data.frame(EVIAmp)
  EVIAmpdf <- apply(EVIAmpdf, 1, median, na.rm=T)
  EVIMaxdf <- as.data.frame(EVIMax)
  EVIMaxdf <- apply(EVIMaxdf, 1, median, na.rm=T)
  EVIAredf <- as.data.frame(EVIAre)
  EVIAredf <- apply(EVIAredf, 1, median, na.rm=T)
  Peakdf <- as.data.frame(Peak)
  Peakdf <- apply(Peakdf, 1, median, na.rm=T)
  
  moddf <- data.frame(sitenm, unique(filtdf$Year),GUPdf,GDWdf,GSLdf,GUP15df,GDW15df,
                      EVIAmpdf,EVIMaxdf,EVIAredf,Peakdf)
  
  lsmod[[k]] <- moddf
  }, error=function(e){cat("ERROR :", conditionMessage(e), "\n")})
}

lsmoddf <- ldply(lsmod, data.frame)
lsmoddf[lsmoddf == 'NaN'] <- NA

saveRDS(lsmoddf, '/projectnb/modislc/users/twgreen/MODISProject/RData/Ameriflux_Site/DBF_MF/sitemodis.rds')
