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
mat_lct[mat_lct != 4] <- NA
lct <- setValues(lct, mat_lct)

doy_offset <- as.integer(as.Date(paste((year-1),'-12-31',sep='')) - as.Date("1970-1-1"))

sds <- unlist(gdal_subdatasets(list_of_phe[j+19]))
te <- raster(sds[3])
values(te)[values(te)>30000] <- NA
te <- te - doy_offset
te[is.na(lct)] <- NA
mat_gup[,1] <- values(te)

te <- raster(sds[2])
values(te)[values(te)>30000] <- NA
te <- te - doy_offset
te[is.na(lct)] <- NA
mat_15u[,1] <- values(te)

te <- raster(sds[8])
values(te)[values(te)>30000] <- NA
te <- te - doy_offset
te[is.na(lct)] <- NA
mat_15d[,1] <- values(te)

te <- raster(sds[4])
values(te)[values(te)>30000] <- NA
te <- te - doy_offset
te[is.na(lct)] <- NA
mat_peak[,1] <- values(te)

te <- raster(sds[7])
values(te)[values(te)>30000] <- NA
te <- te - doy_offset
te[is.na(lct)] <- NA
mat_gdw[,1] <- values(te)

te <- raster(sds[9])
te[is.na(lct)] <- NA
mat_evimin[,1] <- values(te)

te <- raster(sds[10])
te[is.na(lct)] <- NA
mat_eviamp[,1] <- values(te)

te <- raster(sds[11])
te[is.na(lct)] <- NA
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
crotmc[is.na(lct)] <- NA
croswn <- crop(proswn, lct@extent)
croswn[is.na(lct)] <- NA
crovpf <- crop(provpf, lct@extent)
crovpf[is.na(lct)] <- NA
crocta <- crop(procta, lct@extent)
crocta[is.na(lct)] <- NA



mat_tmc <- matrix(NA,length(lct),1)
mat_swn <- matrix(NA,length(lct),1)
mat_vpf <- matrix(NA,length(lct),1)
mat_cta <- matrix(NA,length(lct),1)

mat_tmc[,1] <- values(crotmc)
mat_swn[,1] <- values(croswn)
mat_vpf[,1] <- values(crovpf)
mat_cta[,1] <- values(crocta)


mgsl <- setValues(lct, mat_gsl)
mgdw <- setValues(lct, mat_gdw)
mgup <- setValues(lct, mat_gup)
mpeak <- setValues(lct, mat_peak)
mamp <- setValues(lct, mat_eviamp)
mare <- setValues(lct, mat_eviarea)
mmax <- setValues(lct, mat_evimax)
m15u <- setValues(lct, mat_15u)
m15d <- setValues(lct, mat_15d)
mtmc <- setValues(lct, mat_tmc)
mswn <- setValues(lct, mat_swn)
mvpf <- setValues(lct, mat_vpf)
mcta <- setValues(lct, mat_cta)

mgsl <- focal(mgsl, w=matrix(1/9,nrow=3,ncol=3), na.rm=FALSE)
mgdw <- focal(mgdw, w=matrix(1/9,nrow=3,ncol=3), na.rm=FALSE)
mgup <- focal(mgup, w=matrix(1/9,nrow=3,ncol=3), na.rm=FALSE)
mpeak <- focal(mpeak, w=matrix(1/9,nrow=3,ncol=3), na.rm=FALSE)
mamp <- focal(mamp, w=matrix(1/9,nrow=3,ncol=3), na.rm=FALSE)
mare <- focal(mare, w=matrix(1/9,nrow=3,ncol=3), na.rm=FALSE)
mmax <- focal(mmax, w=matrix(1/9,nrow=3,ncol=3), na.rm=FALSE)
m15u <- focal(m15u, w=matrix(1/9,nrow=3,ncol=3), na.rm=FALSE)
m15d <- focal(m15d, w=matrix(1/9,nrow=3,ncol=3), na.rm=FALSE)
mtmc <- focal(mtmc, w=matrix(1/9,nrow=3,ncol=3), na.rm=FALSE)
mswn <- focal(mswn, w=matrix(1/9,nrow=3,ncol=3), na.rm=FALSE)
mvpf <- focal(mvpf, w=matrix(1/9,nrow=3,ncol=3), na.rm=FALSE)
mcta <- focal(mcta, w=matrix(1/9,nrow=3,ncol=3), na.rm=FALSE)

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

GPP_Maxp <- -20.74698266 + 0.01662890*vgsl + 0.06188045*vgdw + -0.02858825*vgup + 0.03288359*v15d + 
  -1.20966256*vamp + 22.46353897*vmax + 0.25620916*vtmc + 0.03931133*vswn + -0.71145653*vcta

GPP_GSLp <- 425.3030135 + 0.7040366*vgup + -0.4701750*vgdw + -3.0143801*v15u + -0.3171406*v15d + 1.5455551*vpea + 
  -4.5787664*vare + 229.3262931*vamp + -67.0288799*vvpf + 2.3586123*vcta 

GPPp <- -1423.17190 + 2.83578*GPP_GSLp + 142.82101*GPP_Maxp + -248.08300*vvpf + 3.95701*vswn

rGPP_maxp <- setValues(lct, GPP_Maxp)
rGPP_GSLp <- setValues(lct, GPP_GSLp)
rGPPp <- setValues(lct, GPPp)

plot(rGPP_maxp)
plot(rGPP_GSLp)
plot(rGPPp)

GPPfold <- '/projectnb/modislc/users/twgreen/MODISProject/RData/spatial/Rast_v4/GPP_vars4/'
foldercheck <- paste0(GPPfold,tile)
if(!dir.exists(foldercheck)){dir.create(foldercheck)}

saveRDS(rGPP_maxp, file = paste0(foldercheck,'/GPPmax_',year,'.rds'))
saveRDS(rGPP_GSLp, file = paste0(foldercheck,'/GPPgsl_',year,'.rds'))
saveRDS(rGPPp, file = paste0(foldercheck,'/GPP_',year,'.rds'))



