library(easypackages)
libraries('raster','sp', 'terra', 'sf', 'ncdf4')

args <- commandArgs()
print(args)

tile <- substr(args[3],1,6)
year <- as.numeric(substr(args[3],7,10))
j <- year - 2000

list_of_lct <- list.files(path='/projectnb/modislc/projects/sat/data/mcd12q/q1a/',
                          recursive = TRUE,
                          pattern = tile,
                          full.names = TRUE)

list_of_phe <- list.files(path='/projectnb/modislc/projects/sat/data/mcd12q/q2/',
                          recursive = TRUE,
                          pattern = tile,
                          full.names = TRUE)

sds <- unlist(gdal_subdatasets(list_of_lct[j]))
lct <- raster(sds[1])
halfx <- (lct@extent@xmin + lct@extent@xmax) /2
halfy <- (lct@extent@ymin + lct@extent@ymax) /2
halfx1 <- (lct@extent@xmin + halfx) /2
halfx12 <- (halfx1 + halfx) /2
halfx11 <- (lct@extent@xmin + halfx1) /2
halfx2 <- (lct@extent@xmax + halfx) /2
halfx22 <- (lct@extent@xmax + halfx2) /2
halfx23 <- (halfx2 + halfx)

extenthalf <- c(lct@extent@xmin, halfx11, halfy, lct@extent@ymax)
extenthalfa <- c(halfx11, halfx1, halfy, lct@extent@ymax)
extenthalfb <- c(halfx1,halfx12, halfy, lct@extent@ymax)
extenthalfc <- c(halfx12,halfx, halfy, lct@extent@ymax)

extenthalfd <- c(halfx,halfx23, halfy, lct@extent@ymax)
extenthalfe <- c(halfx23,halfx2, halfy, lct@extent@ymax)
extenthalff <- c(halfx2,halfx22, halfy, lct@extent@ymax)
extenthalfg <- c(halfx22,lct@extent@xmax, halfy, lct@extent@ymax)

extenthalfh <- c(lct@extent@xmin, halfx11, halfy, lct@extent@ymax)
extenthalfi <- c(halfx11, halfx1, halfy, lct@extent@ymax)
extenthalfj <- c(halfx1,halfx12, halfy, lct@extent@ymax)
extenthalfk <- c(halfx12,halfx, halfy, lct@extent@ymax)

extenthalfl <- c(halfx,halfx23, lct@extent@ymin, halfy)
extenthalfm <- c(halfx23,halfx2, lct@extent@ymin, halfy)
extenthalfn <- c(halfx2,halfx22, lct@extent@ymin, halfy)
extenthalfo <- c(halfx22,lct@extent@xmax, lct@extent@ymin, halfy)

lct <- crop(lct, extenthalf)


mat_lct <- matrix(NA,length(lct),1)
mat_gsl <- matrix(NA,length(lct),1)
mat_gup <- matrix(NA,length(lct),1)
mat_gdw <- matrix(NA,length(lct),1)
mat_15u <- matrix(NA,length(lct),1)
mat_15d <- matrix(NA,length(lct),1)
mat_peak <- matrix(NA,length(lct),1)
mat_eviamp <- matrix(NA,length(lct),1)
mat_evimax <- matrix(NA,length(lct),1)
mat_evimin <- matrix(NA,length(lct),1)
mat_eviarea <- matrix(NA,length(lct),1)


mat_lct[,1] <- values(lct)

mat_lct[mat_lct == 5] <- 4
mat_lct[mat_lct == 17] <- NA

doy_offset <- as.integer(as.Date(paste((year-1),'-12-31',sep='')) - as.Date("1970-1-1"))

sds <- unlist(gdal_subdatasets(list_of_phe[j+19]))
te <- raster(sds[3])
te <- crop(te, lct@extent)
values(te)[values(te)>30000] <- NA
te <- te - doy_offset
mat_gup[,1] <- values(te)

te <- raster(sds[2])
te <- crop(te, lct@extent)
values(te)[values(te)>30000] <- NA
te <- te - doy_offset
mat_15u[,1] <- values(te)

te <- raster(sds[8])
te <- crop(te, lct@extent)
values(te)[values(te)>30000] <- NA
te <- te - doy_offset
mat_15d[,1] <- values(te)

te <- raster(sds[4])
te <- crop(te, lct@extent)
values(te)[values(te)>30000] <- NA
te <- te - doy_offset
mat_peak[,1] <- values(te)

te <- raster(sds[7])
te <- crop(te, lct@extent)
values(te)[values(te)>30000] <- NA
te <- te - doy_offset
mat_gdw[,1] <- values(te)

te <- raster(sds[9])
te <- crop(te, lct@extent)
mat_evimin[,1] <- values(te)

te <- raster(sds[10])
te <- crop(te, lct@extent)
mat_eviamp[,1] <- values(te)

te <- raster(sds[11])
te <- crop(te, lct@extent)
mat_eviarea[,1] <- values(te)

mat_gsl <- mat_gdw - mat_gup
mat_evimax <- mat_eviamp - mat_evimin

foldtar <- '/projectnb/modislc/users/twgreen/MODISProject/RData/spatial/Rast_v2/Tair/'
foldswn <- '/projectnb/modislc/users/twgreen/MODISProject/RData/spatial/Rast_v2/SWin/'
foldvpf <- '/projectnb/modislc/users/twgreen/MODISProject/RData/spatial/Rast_v2/VPD_v2/'
foldcta <- '/projectnb/modislc/users/twgreen/MODISProject/RData/spatial/Rast_v2/Clim_Tair/'
rasttmc <- readRDS(file = paste0(foldtar,tile,'/r',j,'.rds'))
rastswn <- readRDS(file = paste0(foldswn,tile,'/r',j,'.rds'))
rastvpf <- readRDS(file = paste0(foldvpf,tile,'/r',j,'.rds'))
rastcta <- readRDS(file = paste0(foldcta,tile,'/rast.rds'))

protmc <- projectRaster(rasttmc, crs = crs(lct), res=res(lct))
proswn <- projectRaster(rastswn, crs = crs(lct), res=res(lct))
provpf <- projectRaster(rastvpf, crs = crs(lct), res=res(lct))
procta <- projectRaster(rastcta, crs = crs(lct), res=res(lct))

crotmc <- crop(protmc, lct@extent)
croswn <- crop(proswn, lct@extent)
crovpf <- crop(provpf, lct@extent)
crocta <- crop(procta, lct@extent)

mat_tmc <- matrix(NA,length(lct),1)
mat_swn <- matrix(NA,length(lct),1)
mat_vpf <- matrix(NA,length(lct),1)
mat_cta <- matrix(NA,length(lct),1)

mat_tmc[,1] <- values(crotmc)
mat_swn[,1] <- values(croswn)
mat_vpf[,1] <- values(crovpf)
mat_cta[,1] <- values(crocta)






