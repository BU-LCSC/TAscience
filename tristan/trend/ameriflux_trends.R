library(easypackages)
libraries('raster','sp', 'terra', 'sf', 'ncdf4', 'zyp','dplyr','plyr')

usstates <- shapefile('/usr3/graduate/twgreen/CSVS_SHPS/USstates/cb_2018_us_state_5m.shp')
canadianpro <- shapefile('/usr3/graduate/twgreen/CSVS_SHPS/CanadianProvinces/lpr_000b16a_e.shp')
usstates <- spTransform(usstates, Max1@crs)
canadianpro <- spTransform(canadianpro, Max1@crs)
nest <- c('VT','NY','NH','ME','PA', 'MA', 'CT', 'NJ', 'RI')
nest1 <- c('Qc','Ont.')
usfilt <- usstates[as.character(usstates$STUSPS) %in% nest,]
canfilt <- canadianpro[as.character(canadianpro$PRFABBR) %in% nest1,]

plot(usfilt)
plot(canfilt, add=T)
Max1 <- readRDS('/projectnb/modislc/users/twgreen/MODISProject/RData/spatial/Rast_v4/GPP_vars3/h12v04/2001/GPP_2001.rds')
# fMax1 <- crop(Max1, usfilt)

Max2 <- readRDS('/projectnb/modislc/users/twgreen/MODISProject/RData/spatial/Rast_v4/GPP_vars3/h12v04/2002/GPP_2002.rds')
# fMax2 <- crop(Max2, usfilt)

Max3 <- readRDS('/projectnb/modislc/users/twgreen/MODISProject/RData/spatial/Rast_v4/GPP_vars3/h12v04/2003/GPP_2003.rds')
# fMax3 <- crop(Max3, usfilt)

Max4 <- readRDS('/projectnb/modislc/users/twgreen/MODISProject/RData/spatial/Rast_v4/GPP_vars3/h12v04/2004/GPP_2004.rds')
# fMax4 <- crop(Max4, usfilt)

Max5 <- readRDS('/projectnb/modislc/users/twgreen/MODISProject/RData/spatial/Rast_v4/GPP_vars3/h12v04/2005/GPP_2005.rds')
# fMax5 <- crop(Max5, usfilt)

Max6 <- readRDS('/projectnb/modislc/users/twgreen/MODISProject/RData/spatial/Rast_v4/GPP_vars3/h12v04/2006/GPP_2006.rds')
# fMax6 <- crop(Max6, usfilt)

Max7 <- readRDS('/projectnb/modislc/users/twgreen/MODISProject/RData/spatial/Rast_v4/GPP_vars3/h12v04/2007/GPP_2007.rds')
# fMax7 <- crop(Max7, usfilt)

Max8 <- readRDS('/projectnb/modislc/users/twgreen/MODISProject/RData/spatial/Rast_v4/GPP_vars3/h12v04/2008/GPP_2008.rds')
# fMax8 <- crop(Max8, usfilt)

Max9 <- readRDS('/projectnb/modislc/users/twgreen/MODISProject/RData/spatial/Rast_v4/GPP_vars3/h12v04/2009/GPP_2009.rds')
# fMax9 <- crop(Max9, usfilt)

Max10 <- readRDS('/projectnb/modislc/users/twgreen/MODISProject/RData/spatial/Rast_v4/GPP_vars3/h12v04/2010/GPP_2010.rds')
# fMax10 <- crop(Max10, usfilt)

Max11 <- readRDS('/projectnb/modislc/users/twgreen/MODISProject/RData/spatial/Rast_v4/GPP_vars3/h12v04/2011/GPP_2011.rds')
# fMax11 <- crop(Max11, usfilt)

Max12 <- readRDS('/projectnb/modislc/users/twgreen/MODISProject/RData/spatial/Rast_v4/GPP_vars3/h12v04/2012/GPP_2012.rds')
# fMax12 <- crop(Max12, usfilt)

Max13 <- readRDS('/projectnb/modislc/users/twgreen/MODISProject/RData/spatial/Rast_v4/GPP_vars3/h12v04/2013/GPP_2013.rds')
# fMax13 <- crop(Max13, usfilt)

Max14 <- readRDS('/projectnb/modislc/users/twgreen/MODISProject/RData/spatial/Rast_v4/GPP_vars3/h12v04/2014/GPP_2014.rds')
# fMax14 <- crop(Max14, usfilt)

Max15 <- readRDS('/projectnb/modislc/users/twgreen/MODISProject/RData/spatial/Rast_v4/GPP_vars3/h12v04/2015/GPP_2015.rds')
# fMax15 <- crop(Max15, usfilt)

Max16 <- readRDS('/projectnb/modislc/users/twgreen/MODISProject/RData/spatial/Rast_v4/GPP_vars3/h12v04/2016/GPP_2016.rds')
# fMax16 <- crop(Max16, usfilt)

Max17 <- readRDS('/projectnb/modislc/users/twgreen/MODISProject/RData/spatial/Rast_v4/GPP_vars3/h12v04/2017/GPP_2017.rds')
# fMax17 <- crop(Max17, usfilt)

Max18 <- readRDS('/projectnb/modislc/users/twgreen/MODISProject/RData/spatial/Rast_v4/GPP_vars3/h12v04/2018/GPP_2018.rds')
# fMax18 <- crop(Max18, usfilt)

Max19 <- readRDS('/projectnb/modislc/users/twgreen/MODISProject/RData/spatial/Rast_v4/GPP_vars3/h12v04/2019/GPP_2019.rds')
# fMax19 <- crop(Max19, usfilt)

Max20 <- readRDS('/projectnb/modislc/users/twgreen/MODISProject/RData/spatial/Rast_v4/GPP_vars3/h12v04/2020/GPP_2020.rds')
# fMax20 <- crop(Max20, usfilt)

Max21 <- readRDS('/projectnb/modislc/users/twgreen/MODISProject/RData/spatial/Rast_v4/GPP_vars3/h12v04/2021/GPP_2021.rds')
# fMax21 <- crop(Max21, usfilt)



m1 <- values(Max1)
m2 <- values(Max2)
m3 <- values(Max3)
m4 <- values(Max4) 
m5 <- values(Max5)
m6 <- values(Max6)
m7 <- values(Max7)
m8 <- values(Max8) 
m9 <- values(Max9)
m10 <- values(Max10)
m11 <- values(Max11)
m12 <- values(Max12) 
m13 <- values(Max13)
m14 <- values(Max14)
m15 <- values(Max15)
m16 <- values(Max16)
m17 <- values(Max17)
m18 <- values(Max18)
m19 <- values(Max19)
m20 <- values(Max20)
m21 <- values(Max21)

combm <- matrix(NA, length(m1), 21)
combm[,1] <- m1
combm[,2] <- m2
combm[,3] <- m3
combm[,4] <- m4
combm[,5] <- m5
combm[,6] <- m6
combm[,7] <- m7
combm[,8] <- m8
combm[,9] <- m9
combm[,10] <- m10
combm[,11] <- m11
combm[,12] <- m12
combm[,13] <- m13
combm[,14] <- m14
combm[,15] <- m15
combm[,16] <- m16
combm[,17] <- m17
combm[,18] <- m18
combm[,19] <- m19
combm[,20] <- m20
combm[,21] <- m21

delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}

df <- as.data.frame(combm)
df$nu <- 1:length(df$V1)
dfa <- delete.na(df, 3)
dfb <- full_join(df, dfa, by=c('nu'))
df <- data.frame(dfb$V1.y, dfb$V2.y, dfb$V3.y, dfb$V4.y, dfb$V5.y, dfb$V6.y, dfb$V7.y, dfb$V8.y,
                 dfb$V9.y, dfb$V10.y, dfb$V11.y, dfb$V12.y, dfb$V13.y, dfb$V14.y, dfb$V15.y, dfb$V16.y,
                 dfb$V17.y, dfb$V18.y, dfb$V19.y, dfb$V20.y, dfb$V21.y)
combs <- as.matrix(df)

thel_slop <- matrix(NA,length(m1),1)
p_value <- matrix(NA,length(m1),1)
comb2 <- matrix(NA, length(m1),2)
x <- 2001:2021
for(i in 1:length(thel_slop)){
  u <- combs[i,]
  z <- zyp.sen(u~x)
  k <- MannKendall(u)
  thel_slop[i,] <- z$coefficients[2]
  p_value[i,] <- k$sl
  print(i)
}
comb2[,1] <- thel_slop
comb2[,2] <- p_value

comb2a <- as.data.frame(comb2)
comb2a$V1[comb2a$V2 >= .05] <- NA
comb2b <- na.omit(comb2a)
comb2m <- as.matrix(comb2a)

thel_tot <- setValues(Max1, comb2[,1])
thel_rast <- setValues(Max1, comb2m[,1])
rastpoint <- rasterToPoints(thel_rast)
rasdf <- data.frame(rastpoint)
points <- cbind(rasdf$x, rasdf$y)
vz <- SpatialPoints(points, proj4string = Max1@crs) #tell its in lat long crs
yz <- spTransform(vz, Max1@crs) #convert to daymet projection

plot(thel_tot, col=Pal(100))
plot(usfilt, col='grey60', add=T)
plot(canfilt, col='grey60',add=T)
plot(thel_tot, col=Pal(100), add=T)
plot(vz, cex = 0.0005, add=T)

plot(usfilt, col='grey60')
plot(canfilt, col='grey60',add=T)
plot(thel_rast, col=Pal(100), add=T)
hist(thel_rast, breaks=20, xlab='Annual GPP Trend (gC m-2 y-1)', main='Thiel-sen Slope Over NE (NAT=2, p<0.05)')

hist(thel_max, xlab='GPP Max Trend (gC m-2)', main='Thiel-sen Slope Over NE')
hist(thel_gsl, xlab='GPP GSL Trend (DoY)', main='Thiel-sen Slope Over NE')
hist(thel_gpp, xlab='Annual GPP Trend (gC m-2 y-1)', main='Thiel-sen Slope Over NE')

Pal <- colorRampPalette(c('blue','#ffffbf','red'))

save.image('/projectnb/modislc/users/twgreen/MODISProject/RData/spatial/trend_DMMod.RData')

pdf('/projectnb/modislc/users/twgreen/MODISProject/Figures/Spatial/different_trends_v2.pdf')
hist(thel_gppt, breaks=20, xlab='Annual GPP Trend (gC m-2 y-1)', main='Thiel-sen Slope Over NE (NAT=3, p<0.05)')
hist(thel_gpp, breaks=20, xlab='Annual GPP Trend (gC m-2 y-1)', main='Thiel-sen Slope Over NE (NAT=3, p<0.05)')
hist(thel_gslt, breaks=20, xlab='GPP GSL Trend (DoY)', main='Thiel-sen Slope Over NE (NAT=3, p<0.05)')
hist(thel_gsl, breaks=20, xlab='GPP GSL Trend (DoY)', main='Thiel-sen Slope Over NE (NAT=3, p<0.05)')
hist(thel_maxt, breaks=20, xlab='GPP Max Trend (gC m-2)', main='Thiel-sen Slope Over NE (NAT=3, p<0.05)')
hist(thel_max, breaks=20, xlab='GPP Max Trend (gC m-2)', main='Thiel-sen Slope Over NE (NAT=3, p<0.05)')
dev.off()

writeRaster(thel_gppt13v04, filename='/projectnb/modislc/users/twgreen/MODISProject/RData/spatial/Rast_v3/Rast_vars/h13v04gppt.tif')

a <- data.frame(values(thel_maxt))
b <- data.frame(values(thel_maxt11v04))
c <- data.frame(values(thel_maxt11v05))
d <- data.frame(values(thel_maxt13v04))

colnames(a) <- c('values')
colnames(b) <- c('values')
colnames(c) <- c('values')
colnames(d) <- c('values')

thel_maxtall <- rbind.fill(a,b,c,d)

pdf('/projectnb/modislc/users/twgreen/MODISProject/Figures/Spatial/alltiles_trends.pdf')
hist(thel_gpptall$values, breaks=30, xlab='Annual GPP Trend (gC m-2 y-1)', main='Thiel-sen Slope Over NE (NAT=3, p<0.05)')
hist(thel_gppall$values, breaks=30, xlab='Annual GPP Trend (gC m-2 y-1)', main='Thiel-sen Slope Over NE (NAT=3, p<0.05)')
hist(thel_gsltall$values, breaks=30, xlab='GPP GSL Trend (DoY)', main='Thiel-sen Slope Over NE (NAT=3, p<0.05)')
hist(thel_gslall$values, breaks=30, xlab='GPP GSL Trend (DoY)', main='Thiel-sen Slope Over NE (NAT=3, p<0.05)')
hist(thel_maxtall$values, breaks=30, xlab='GPP Max Trend (gC m-2)', main='Thiel-sen Slope Over NE (NAT=3, p<0.05)')
hist(thel_maxall$values, breaks=30, xlab='GPP Max Trend (gC m-2)', main='Thiel-sen Slope Over NE (NAT=3, p<0.05)')
dev.off()
