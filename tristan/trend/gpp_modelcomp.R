library(easypackages)
libraries('raster','sf','terra')

zgpp_path <- '/projectnb/modislc/users/twgreen/MODISProject/ZhangGPPproduct/CMG_0.05_monthly/'
fileszgpp <- list.files(path = zgpp_path, recursive = T, full.names=T)

ra2001 <- brick(fileszgpp[2])

ra2001.sum <- sum(ra2001)

plot(ra2001.sum)

Max1 <- readRDS('/projectnb/modislc/users/twgreen/MODISProject/RData/spatial/Rast_v4/GPP_vars3/h12v04/GPP_2001.rds')
Max2 <- readRDS('/projectnb/modislc/users/twgreen/MODISProject/RData/spatial/Rast_v4/GPP_vars4/h12v04/GPP_2001.rds')

pro.ra2001 <- projectRaster(ra2001.sum, crs = crs(Max1))
cro.ra2001 <- crop(pro.ra2001, Max1@extent)
cro.ra2001 <- projectRaster(cro.ra2001, crs=crs(Max1), res=res(Max1))
cro.ra2001 <- crop(cro.ra2001, Max1@extent)
val.ra2001 <- values(cro.ra2001)
val.gpp001 <- values(Max1)
val.gpp002 <- values(Max2)

dfcomp <- data.frame(val.gpp001, val.gpp002, val.ra2001)
dfcomp <- na.omit(dfcomp)


pdf('/projectnb/modislc/users/twgreen/MODISProject/Figures/Spatial/amerifluxmodelcomp.pdf')
ggplot(dfcomp, aes(x=val.gpp001,y=val.ra2001)) + geom_point() + stat_poly_line() +
  stat_poly_eq()+xlab('Ameriflux Model') + ylab('VPM GPP')+ theme_classic()

ggplot(dfcomp, aes(x=val.gpp002,y=val.ra2001)) + geom_point() + stat_poly_line() +
  stat_poly_eq()+xlab('Old FluxNet Model') + ylab('VPM GPP') + theme_classic()
dev.off()