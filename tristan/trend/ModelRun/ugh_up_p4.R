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

lct <- crop(lct, extenthalfc)

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

mat_lct[mat_lct == 17] <- NA

doy_offset <- as.integer(as.Date(paste((year-1),'-12-31',sep='')) - as.Date("1970-1-1"))


spts <- rasterToPoints(lct, spatial = TRUE)
llprj <-  "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
llpts <- spTransform(spts, CRS(llprj))
x <- as.data.frame(llpts)

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

mat_gsla <- matrix(NA,length(lct),1)
mat_gupa <- matrix(NA,length(lct),1)
mat_gdwa <- matrix(NA,length(lct),1)
mat_peaka <- matrix(NA,length(lct),1)
mat_eviampa <- matrix(NA,length(lct),1)
mat_eviareaa <- matrix(NA,length(lct),1)
mat_evimaxa <- matrix(NA,length(lct),1)
mat_15ua <- matrix(NA,length(lct),1)
mat_15da <- matrix(NA,length(lct),1)
mat_tmca <- matrix(NA,length(lct),1)
mat_swna <- matrix(NA,length(lct),1)
mat_vpfa <- matrix(NA,length(lct),1)
mat_ctaa <- matrix(NA,length(lct),1)

for(i in 1:length(lct)){
  Lon <- x$x[i]
  Lat <- x$y[i]
  points <- cbind(Lon,Lat)
  vz <- SpatialPoints(points, proj4string = CRS("+proj=longlat +datum=WGS84")) #tell its in lat long crs
  yz <- spTransform(vz, CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs")) #convert to sinusodal for MODIS

  numPix <- length(lct)
  imgNum <- setValues(lct, 1:numPix)
  z33 <- raster::extract(imgNum, yz, buffer=850)
  
  LCTa <- matrix(NA, 1, length(z33[[1]]))
  GUP <- matrix(NA, 1, length(z33[[1]]))
  GDW <- matrix(NA, 1, length(z33[[1]]))
  gsl <- matrix(NA, 1, length(z33[[1]]))
  GUP15 <- matrix(NA, 1, length(z33[[1]]))
  GDW15 <- matrix(NA, 1, length(z33[[1]]))
  EVIAmp <- matrix(NA, 1, length(z33[[1]]))
  EVIMax <- matrix(NA, 1, length(z33[[1]]))
  EVIAre <- matrix(NA, 1, length(z33[[1]]))
  Peak <- matrix(NA, 1, length(z33[[1]]))
  TMC <- matrix(NA, 1, length(z33[[1]]))
  SWN <- matrix(NA, 1, length(z33[[1]]))
  VPF <- matrix(NA, 1, length(z33[[1]]))
  CTA <- matrix(NA, 1, length(z33[[1]]))
  
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
    TMC[,f] <- mat_tmc[z33[[1]][f],]
    SWN[,f] <- mat_swn[z33[[1]][f],]
    VPF[,f] <- mat_vpf[z33[[1]][f],]
    CTA[,f] <- mat_cta[z33[[1]][f],]
  }
  LCTa[LCTa != 4 & LCTa != 5] <- NA
  LCTdf <- as.data.frame(LCTa)
  LCTdf[!complete.cases(LCTdf),] <- NA
  LCTa <- as.matrix(LCTdf)
  GUP[is.na(LCTa)] <- NA
  GDW[is.na(LCTa)] <- NA
  gsl[is.na(LCTa)] <- NA
  GUP15[is.na(LCTa)] <- NA
  GDW15[is.na(LCTa)] <- NA
  EVIAmp[is.na(LCTa)] <- NA
  EVIMax[is.na(LCTa)] <- NA
  EVIAre[is.na(LCTa)] <- NA
  Peak[is.na(LCTa)] <- NA
  TMC[is.na(LCTa)] <- NA
  SWN[is.na(LCTa)] <- NA
  VPF[is.na(LCTa)] <- NA
  CTA[is.na(LCTa)] <- NA

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
  TMCdf <- as.data.frame(TMC)
  TMCdf <- apply(TMCdf, 1, median, na.rm=T)
  SWNdf <- as.data.frame(SWN)
  SWNdf <- apply(SWNdf, 1, median, na.rm=T)
  VPFdf <- as.data.frame(VPF)
  VPFdf <- apply(VPFdf, 1, median, na.rm=T)
  CTAdf <- as.data.frame(CTA)
  CTAdf <- apply(CTAdf, 1, median, na.rm=T)
  
  mat_gsla[i,] <- GSLdf
  mat_gupa[i,] <- GUPdf
  mat_gdwa[i,] <- GDWdf
  mat_peaka[i,] <- Peakdf
  mat_eviampa[i,] <- EVIAmpdf
  mat_eviareaa[i,] <- EVIAredf
  mat_evimaxa[i,] <- EVIMaxdf
  mat_15ua[i,] <- GUP15df
  mat_15da[i,] <- GDW15df
  mat_tmca[i,] <- TMCdf
  mat_swna[i,] <- SWNdf
  mat_vpfa[i,] <- VPFdf
  mat_ctaa[i,] <- CTAdf
}

mgsl <- setValues(lct, mat_gsla)
mgdw <- setValues(lct, mat_gdwa)
mgup <- setValues(lct, mat_gupa)
mpeak <- setValues(lct, mat_peaka)
mamp <- setValues(lct, mat_eviampa)
mare <- setValues(lct, mat_eviareaa)
mmax <- setValues(lct, mat_evimaxa)
m15u <- setValues(lct, mat_15ua)
m15d <- setValues(lct, mat_15da)
mtmc <- setValues(lct, mat_tmca)
mswn <- setValues(lct, mat_swna)
mvpf <- setValues(lct, mat_vpfa)
mcta <- setValues(lct, mat_ctaa)

# mgsl[lct != 4 & lct !=5] <- NA
# mgdw[lct != 4 & lct !=5] <- NA
# mgup[lct != 4 & lct !=5] <- NA
# m15u[lct != 4 & lct !=5] <- NA
# m15d[lct != 4 & lct !=5] <- NA
# mamp[lct != 4 & lct !=5] <- NA
# mmax[lct != 4 & lct !=5] <- NA
# mare[lct != 4 & lct !=5] <- NA
# mpeak[lct != 4 & lct !=5] <- NA
# mtmc[lct != 4 & lct !=5] <- NA
# mswn[lct != 4 & lct !=5] <- NA
# mvpf[lct != 4 & lct !=5] <- NA
# mcta[lct != 4 & lct !=5] <- NA

vgsl <- values(mgsl)
vgdw <- values(mgdw)
vgup <- values(mgup)
v15u <- values(m15u)
v15d <- values(m15d)
vamp <- values(mamp)
vmax <- values(mmax)
vare <- values(mare)
vpea <- values(mpeak)
vtmc <- values(mtmc)
vswn <- values(mswn)
vvpf <- values(mvpf) / 10
vcta <- values(mcta)

GPP_Maxp <- -15.474768792 + 0.161127943*vgsl + -0.147198172*vgdw +  0.222294136*vgup + 0.045022296*v15d +
  -13.168773737*vamp + 3.502067691*vmax + 0.418712775*vtmc + -0.006301636*vswn + 0.507218459*vcta

GPP_GSLp <- 484.5903814 + -1.3964782*vgup + -0.7224514*vgdw + 0.2807481*v15u + 0.3152687*v15d + 0.1156355*vpea +
  0.6122973*vare + -12.4563523*vamp + 20.6419023*vvpf + -5.3599188*vcta

GPPp <- -1332.425825+ 2.339649*GPP_GSLp + 140.619439*GPP_Maxp + 24.508376*vvpf + 3.624591*vswn

rGPP_maxp <- setValues(lct, GPP_Maxp)
rGPP_GSLp <- setValues(lct, GPP_GSLp)
rGPPp <- setValues(lct, GPPp)

GPPfold <- '/projectnb/modislc/users/twgreen/MODISProject/RData/spatial/Rast_v4/GPP_vars/'
foldercheck <- paste0(GPPfold,tile,'/',year)
if(!dir.exists(foldercheck)){dir.create(foldercheck)}

saveRDS(rGPP_maxp, file = paste0(foldercheck,'/GPPmax_',year,'c4.rds'))
saveRDS(rGPP_GSLp, file = paste0(foldercheck,'/GPPgsl_',year,'c4.rds'))
saveRDS(rGPPp, file = paste0(foldercheck,'/GPP_',year,'c4.rds'))

