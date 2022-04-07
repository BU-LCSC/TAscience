library(sp)
library(raster)
library(gdalUtils)
library(rgdal)
library(RcppRoll)
library(rjson)


########################################
args <- commandArgs()
print(args)

xx <- as.numeric(args[3])
# xx <- 24

########################################
params <- fromJSON(file='/usr3/graduate/mkmoon/GitHub/TAscience/mcd12q2/Applying_MSLSP/Q2_Parameters.json')
source(params$setup$rFunctions)

tile = 'h10v03'
year = 2016:2020

# Base image
mcd12a4_path <- paste('/projectnb/modislc/projects/sat/data/e4ftl01.cr.usgs.gov/MOTA/MCD43A4.006')
search_str <- paste('*43A4.A',year[4],'*.',tile,'.006*',sep='')
files <- list.files(path=mcd12a4_path, pattern=glob2rx(search_str),full.names=T,include.dirs=F,recursive=T)
imgBase <- raster(get_subdatasets(files[250])[[9]])
# plot(imgBase,colNA='red')

# Point locations
x <- -127.6731 + xx*0.005
y <- 52.54944
points <- cbind(x,y)
v <- SpatialPoints(points, proj4string = CRS("+proj=longlat +datum=WGS84"))
ptShp <- spTransform(v, CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"))

numPix <- length(imgBase)
imgNum <- setValues(imgBase, 1:numPix)
pixNum <- extract(imgNum, ptShp)
 


########################################
## Extract splined EVI 
ncol <- 2400
nrow <- 2400
nbands <- 365
cnt <- ncol*nrow*nbands

# MODSI product; spline and phenology
data <- readBin('/projectnb/modislc/users/mkmoon/LCD_C6/C6_1/spline_out/h10v03/c6_tas.h10v03.2019.evi2.bip',
                what="integer",n=cnt,size=2,endian="little")
# data <- readBin('/projectnb/modislc/projects/sat/data/spline/h10v03/tg_v1.h10v03.2019.evi2.bip',
#                 what="integer",n=cnt,size=2,endian="little")
data <- array(data,c(nbands, ncol, nrow))
data <- aperm(data, c(3,2,1)) #for transposing
data <- brick(data)

splined <- matrix(NA,365,1)
for(i in 1:365){
  rast <- data[[i]]
  splined[i,] <- rast[pixNum]
}

phenP <- matrix(NA,4,1)
sds <- get_subdatasets('/projectnb/modislc/projects/sat/data/mcd12q/q2/2019/MCD12Q2.A2019001.h10v03.006.2021203231541.hdf')
phenP[1] <- raster(sds[2])[pixNum]
phenP[2] <- raster(sds[3])[pixNum]
phenP[3] <- raster(sds[7])[pixNum]
phenP[4] <- raster(sds[8])[pixNum]

# NBAR and spline from MSLSP

numChunks <- params$setup$numChunks
chunk <- numPix%/%numChunks

ckNum <- sprintf('%03d',(pixNum%/%chunk+1))
ckDir <- paste0('/projectnb/modislc/projects/sat/q2_by_mslsp/',tile,'/chunks/2019')
file <- list.files(path=ckDir,pattern=glob2rx(paste0('*',ckNum,'.rda')),full.names=T)
  
load(file)
  
red   <- band1[pixNum%%chunk,]
nir   <- band2[pixNum%%chunk,]
green <- band4[pixNum%%chunk,]
swir  <- band6[pixNum%%chunk,]
snowPix  <- bsnow[pixNum%%chunk,]
snowPix[is.na(snowPix)] <- 0

phenYr <- 2019
dates <- c(seq(as.Date(0,origin=paste0((phenYr-1),'-1-1')),as.Date(364,origin=paste0((phenYr-1),'-1-1')),by='day'),
           seq(as.Date(0,origin=paste0((phenYr-0),'-1-1')),as.Date(364,origin=paste0((phenYr-0),'-1-1')),by='day'),
           seq(as.Date(0,origin=paste0((phenYr+1),'-1-1')),as.Date(364,origin=paste0((phenYr+1),'-1-1')),by='day'))
numLyrs <- 24

# MSLSP spline  
  try({
  
  pheno_pars <- params$phenology_parameters
  qa_pars    <- params$qa_parameters
  
  pheno_pars$numLyrs <- numLyrs
  
  vi   <- 2.5*(nir - red) / (nir + 2.4*red + 1)
  
  #Despike
  spikes <- CheckSpike(vi, dates, pheno_pars)
  vi[spikes] <- NA
  
  # Dormant value
  vi_dorm <- quantile(vi[snowPix==0 & vi>0],probs=pheno_pars$dormantQuantile,na.rm=T)   #Calc vi dormant value
  
  # Snow
  ndsi <- (green - swir) / (green + swir)
  snowPix[ndsi > -0.2] <- 1
  snowPix <- Screen_SnowFills(vi,vi_dorm,snowPix,dates,pheno_pars)            #Screen poorly filled snow values
  
  vi[snowPix] <- vi_dorm   #Fill remaining snow values with dormant value for vi  
  vi[vi < vi_dorm] <- vi_dorm
  
  #Determine gaps that require filling
  gDates <- dates[!is.na(vi)]  
  dDiff <- diff(gDates) > pheno_pars$gapLengthToFill     #Gaps greater than 30 days will be filled
  dStart <- gDates[c(dDiff,FALSE)]
  dEnd <- gDates[c(FALSE,dDiff)]
  
  
  #Locate gaps in date vector
  all_dates <- seq(as.Date(paste0((phenYr-1),'-1-1')), as.Date(paste0((phenYr+1),'-12-31')), by="day")
  
  fill_locations <- matrix(FALSE,length(all_dates))
  for (d in 1:length(dStart)) {
    fill_locations[all_dates >= dStart[d] & all_dates < dEnd[d]] <- TRUE}
  
  fill_dates <- all_dates[fill_locations]
  
  if(length(fill_dates)>0){
    yrsWithGaps <- TRUE
  }else{
    yrsWithGaps <- FALSE
  }
  
  #
  splineStart <- as.Date(paste0(c(phenYr-2,phenYr-1,phenYr),'-01-01')) - - pheno_pars$splineBuffer
  numDaysFit  <-  365 + (pheno_pars$splineBuffer * 2)   
  splineEnd <- splineStart+(numDaysFit-1)
  numYrs <- 3
  daysVec <- 1:numDaysFit
  vecLength <- numDaysFit*numYrs
  
  
  #Fit spline
  smoothMat <- matrix(NA, numDaysFit, numYrs)
  maskMat <- matrix(0, numDaysFit, numYrs)
  fillMat <- smoothMat
  baseWeights <- maskMat
  
  
  for (y in 1:numYrs) {
    #Use try statement, because we don't want to stop processing if only an error in one year
    
    try({
      dateRange <- dates >= splineStart[y] & dates <= splineEnd[y] & !is.na(vi)   
      dateSub <- dates[dateRange]; viSub <- vi[dateRange]; snowSub <- snowPix[dateRange]
      
      #Get weights
      weights <- matrix(1,length(snowSub))
      weights[snowSub == 1] <- pheno_pars$snowWeight
      
      pred_dates <- seq(splineStart[y], splineEnd[y], by="day")
      
      #Assign weights and run cubic spline
      smoothed <- Smooth_VI(viSub, dateSub, pred_dates, weights, pheno_pars, vi_dorm)
      
      #Mask spline in gaps, and before/after first/last image
      maskMat[fill_locations[all_dates %in% pred_dates],y] <- 1    #Mask spline in gaps
      maskMat[pred_dates < dateSub[1],y] <- 1                      #Mask spline before first image and after last image
      maskMat[pred_dates > dateSub[length(dateSub)],y] <- 1
      
      #Mask spline in the buffer years (only interested in comparing splines in target year)
      maskMat[format(pred_dates,'%Y') != phenYr,y]  <- 1
      
      fillDs <- pred_dates %in% dateSub
      
      smoothMat[,y] <- smoothed
      baseWeights[fillDs,y] <- weights
      fillMat[fillDs,y] <- viSub
      
    },silent=TRUE)
    
  }
  
  
  xs <- rep(daysVec,numYrs)
  ys <- matrix(fillMat,vecLength)
  ysGood <- !is.na(ys)
  baseW <- matrix(baseWeights,vecLength)   #Base Weights are 1=clear observation, 0.5=snow-filled
  
  smoothMat_Masked <- smoothMat
  maskMat <- as.logical(maskMat)
  smoothMat_Masked[maskMat] <- NA
  
  
  #Loop through years, compare spline to other years, weight each year based on similiarity, fit spline, calculate phenology
  #Just product years now
  weightArray <- calculateWeights(smoothMat_Masked, numDaysFit, numYrs, pheno_pars) 
  
  prevYear <- daysVec <= pheno_pars$splineBuffer
  inYear <- daysVec > pheno_pars$splineBuffer & daysVec <= (pheno_pars$splineBuffer+365)
  nextYear <- daysVec > (pheno_pars$splineBuffer+365)
  
},silent=TRUE)

  y <- 2

  try({
  
  pred_dates <- seq(splineStart[y], splineEnd[y], by="day")
  
  if (yrsWithGaps) {
    
    indPrev <- y-1; indPrev[indPrev<1] <- 1
    indNext <- y+1; indNext[indNext>numYrs] <- numYrs
    
    weights <- rbind(weightArray[prevYear,,indPrev],
                     weightArray[inYear,,y],
                     weightArray[nextYear,,indNext])
    
    #Where are the gaps?
    toFill <- fill_locations[all_dates %in% pred_dates]
    
    weights[!toFill,] <- 0     #Set weight to zero for observations that aren't in a gap
    weights[,y] <- 1           #Set weights in target year to 1
    
    
    #Now that we have weights, calculate phenology
    #######################
    weights <- matrix(weights,vecLength) * baseW   #Multiple weights by base weight (1=good,0.5=snow-filled)
    theInds <- ysGood & weights > 0
    xs_sub <- xs[theInds]; w_sub <- weights[theInds]
    smoothed_vi <- Smooth_VI(ys[theInds], xs_sub, daysVec, w_sub, pheno_pars, vi_dorm)  #Fit spline
    
  } else {
    
    #Variables needed for next steps if the above gap filling was not done
    theInds <- matrix(FALSE,length(ysGood))
    theInds[((y-1)*numDaysFit+1):(y*numDaysFit)] <- TRUE
    xs_sub <- xs[theInds]; w_sub <- baseW[theInds]
    
    smoothed_vi <- smoothMat[,y]   #if no gaps to fill, just use existing spline
  }
  
  
  #Fit phenology
  peaks <- FindPeaks(smoothed_vi)
  if (all(is.na(peaks))) {outAll <- c(outAll,annualMetrics(viSub,dateSub,smoothed_vi,pred_dates,phenYr,pheno_pars));next}
  
  #Find full segments
  full_segs <- GetSegs(peaks, smoothed_vi, pheno_pars)
  if (is.null(full_segs)) {outAll <- c(outAll,annualMetrics(viSub,dateSub,smoothed_vi,pred_dates,phenYr,pheno_pars));next}  #If no valid segments, calc annual metrics, and move to next year
  
  #Only keep segments with peaks within year *****
  full_segs <- full_segs[inYear[sapply(full_segs, "[[", 2)] ]  #check if peaks are in the year
  if (length(full_segs)==0) {outAll <- c(outAll,annualMetrics(viSub,dateSub,smoothed_vi,pred_dates,phenYr,pheno_pars));next}  #If no valid peaks within a target year, calc annual metrics, and move to next year
  
  #Get PhenoDates
  pheno_dates <- GetPhenoDates(full_segs, smoothed_vi, pred_dates, pheno_pars)
  phen <- unlist(pheno_dates, use.names=F)
  if (all(is.na(phen))) {outAll <- c(outAll,annualMetrics(viSub,dateSub,smoothed_vi,pred_dates,phenYr,pheno_pars));next} #If no dates detected, calc annual metrics, and move to next year
  
  
  #Get metrics that describe the segments and the year
  
  #First, get metrics counting gap filled observations as "good" observations
  seg_metricsFill <- lapply(full_segs, GetSegMetricsLight, daysVec, sort(xs_sub))
  un <- unlist(seg_metricsFill, use.names=F)
  ln <- length(un)      
  gup_maxgap_frac_filled <- un[seq(1, ln, by=2)] * 100
  gdown_maxgap_frac_filled <- un[seq(2, ln, by=2)] * 100
  
  
  #Second, get segment metrics with snow observations counted as "good" observations
  filled_vi <- fillMat[,y]
  seg_metricsFill <- lapply(full_segs, GetSegMetricsLight, daysVec, daysVec[!is.na(filled_vi)])
  un <- unlist(seg_metricsFill, use.names=F)
  ln <- length(un)      
  gup_maxgap_frac_count_snow <- un[seq(1, ln, by=2)] * 100
  gdown_maxgap_frac_count_snow <- un[seq(2, ln, by=2)] * 100
  
  #And get calendar year metrics with snow counted as good
  numObs_count_snow <- sum(!is.na(filled_vi) & inYear)
  maxGap_annual_count_snow <- max(diff(c( pheno_pars$splineBuffer+1, daysVec[!is.na(filled_vi) & inYear], 365+pheno_pars$splineBuffer))) 
  
  
  #Now get the full segment metrics, not counting snow and not counting gap filled
  filled_vi[baseWeights[,y] < 1] <- NA    #If weight is less than 1, implies it is a snow-fill, and we don't want to count snow-filled as a valid observation. So set to NA.
  numObs <- sum(!is.na(filled_vi) & inYear)   #Number of observations in year
  maxGap_annual <- max(diff(c( pheno_pars$splineBuffer+1, daysVec[!is.na(filled_vi) & inYear], 365+pheno_pars$splineBuffer)))  #Max gap (in days) during year
  seg_metrics <- lapply(full_segs, GetSegMetrics, smoothed_vi, filled_vi[!is.na(filled_vi)], pred_dates, pred_dates[!is.na(filled_vi)]) #full segment metrics
  
  
  #Unlist and scale the seg metrics
  un <- unlist(seg_metrics, use.names=F)
  ln <- length(un)
  seg_amp <- un[seq(1, ln, by=9)] * 10000
  seg_max <- un[seq(2, ln, by=9)] * 10000
  seg_int <- un[seq(3, ln, by=9)] * 100  
  gup_rsq <- un[seq(4, ln, by=9)] * 10000
  gup_maxgap <- un[seq(6, ln, by=9)] 
  gdown_rsq <- un[seq(7, ln, by=9)] * 10000
  gdown_maxgap <- un[seq(9, ln, by=9)]
  
  
  ##
  theOrd <- order(seg_amp,decreasing=T)   
  
  # Filter for bad EVI layers
  if(seg_max[theOrd[1]] > 10000 | seg_max[theOrd[1]] < 0 | seg_amp[theOrd[1]] > 10000 | seg_amp[theOrd[1]] < 0){
    outAll <- c(outAll,c(NA,rep(NA,10),4,rep(NA,10),4,NA));next}
  
  numRecords <- length(seg_amp)  #how many cycles were recorded
  
  naCheck <- is.na(seg_amp)
  numCyc <- sum(naCheck == 0)  #how many cycles have good data (seg metrics has valid observations)
  
  
  # # QA
  # if(length(full_segs)==1){
  #   qual_1 <- GetQAs(gup_rsq, gdown_rsq, gup_maxgap, gdown_maxgap, theOrd, qa_pars)
  # }else{
  #   qual_1 <- GetQAs(gup_rsq, gdown_rsq, gup_maxgap, gdown_maxgap, theOrd, qa_pars)[[1]][1]
  #   qual_2 <- GetQAs(gup_rsq, gdown_rsq, gup_maxgap, gdown_maxgap, theOrd, qa_pars)[[2]][1]
  # } 
  
  
  
  ################################################
  #If no cycles have good data, record NA output and move to next
  if (numCyc == 0) {outAll <- c(outAll,annualMetrics(viSub,dateSub,smoothed_vi,pred_dates,phenYr,pheno_pars));next}
  
  
  if (numRecords == 1) {
    #If only one cycle was recorded, report it, and fill NA values for second cycle
    out <- c(1,phen,seg_max,seg_amp,seg_int,NA,c(rep(NA,10),4),numObs)
    
  } else {
    #If there are multiple cycles, sort by amplitude and report two highest amplitudes (highest amplitude first)
    theOrd <- order(seg_amp,decreasing=T)   
    
    phen1 <- phen[seq(theOrd[1], length(phen), by = numRecords)]
    phen2 <- phen[seq(theOrd[2], length(phen), by = numRecords)]
    
    
    if (naCheck[theOrd[2]]) {
      #If the second cycle did not have enough observations (seg_metrics = NA), only report the first cycle
      out <- c(numCyc,phen1,seg_max[theOrd[1]],seg_amp[theOrd[1]],seg_int[theOrd[1]],NA,
               c(rep(NA,10),4),numObs)  
    }else{
      out <- c(numCyc,phen1,seg_max[theOrd[1]],seg_amp[theOrd[1]],seg_int[theOrd[1]],NA,
               phen2,seg_max[theOrd[2]],seg_amp[theOrd[2]],seg_int[theOrd[2]],NA,numObs) 
    }
  }
  
},silent=TRUE)  #End of the try block
  
  

########################################
# Plot
snowPix[snowPix==1] <- 2
snowPix[snowPix==0] <- 1

evi   <- 2.5*((nir-red)/(nir + (2.4*red) +1)) # calculate EVI2 
dates <- seq(as.Date('2019-1-1'),as.Date('2019-12-31'),by='day')


setwd('/projectnb/modislc/projects/sat/q2_by_mslsp/figures/')
png(file=paste('c6_v9_',xx,'.png',sep=''),res=300,units='in',width=12,height=6.5)

par(oma=c(2,2,2,2),mar=c(3,4,1,0))
plot(dates,evi[366:(365+365)],col=snowPix,pch=19,
     ylim=c(-0.2,0.8),xlab='',ylab='EVI2',cex.axis=1.3,cex.lab=1.3)
points(dates,smoothed_vi[186:(185+365)],type='l',lwd=2)
points(dates,splined/10000,type='l',lwd=2,col='blue')
if(numRecords==1){
  abline(v=phen[c(1,7)],lty=5,lwd=1.5)  
}else{
  abline(v=phen1[c(1,7)],lty=5,lwd=1.5)  
}
abline(v=phenP[c(1,4)],lty=1,lwd=1.5,col='blue')
legend('topleft',c('Clean','Snow','Spline; MSLSP','Spline; Product'),
       pch=c(19,19,NA,NA),lty=c(NA,NA,1,1),lwd=c(NA,NA,2,2),col=c(1,2,1,'blue'),
       bty='n',cex=1.3)

dev.off()




# ################################################################################
# ################################################################################
# doy_offset <- as.integer(as.Date(paste((2019-1),'-12-31',sep='')) - as.Date("1970-1-1"))
# 
# sds <- get_subdatasets('/projectnb/modislc/projects/sat/data/mcd12q/q2/2019/MCD12Q2.A2019001.h10v03.006.2021203231541.hdf')
# phenP <- raster(sds[2])
# phenP[phenP>32000] <- NA
# phenP <- phenP-doy_offset
# 
# phenM <- raster('/projectnb/modislc/projects/sat/q2_by_mslsp/h10v03/metrics/2019/02_2019_OGI.tif')
# phenM[phenM>32000] <- NA
# phenM <- phenM-doy_offset
# 
# sum(is.na(values(phenP)))
# sum(is.na(values(phenM)))
# 
# par(mfrow=c(1,2))
# plot(phenP)
# plot(phenM)
# 
# plot(phenP,phenM)
# abline(0,1)



