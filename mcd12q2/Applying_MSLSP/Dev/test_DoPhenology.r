tile <- 'h10v03'; year <- 2016; cc <- 150

params <- fromJSON(file='/usr3/graduate/mkmoon/GitHub/TAscience/mcd12q2/Applying_MSLSP/Q2_Parameters.json')
source(params$setup$rFunctions)

load("/projectnb/modislc/projects/sat/q2_by_mslsp/h10v03/chunks/2016/chunk_150.rda")

i=578
red     <- band1[i,]
nir     <- band2[i,]
green   <- band4[i,]
swir    <- band6[i,]
snowPix <- bsnow[i,]
snowPix[is.na(snowPix)] <- 0

pheno_pars <- params$phenology_parameters
numLyrs <- 24
phenYr <- year
dates <- c(seq(as.Date(0,origin=paste0((phenYr-1),'-1-1')),as.Date(364,origin=paste0((phenYr-1),'-1-1')),by='day'),
           seq(as.Date(0,origin=paste0((phenYr-0),'-1-1')),as.Date(364,origin=paste0((phenYr-0),'-1-1')),by='day'),
           seq(as.Date(0,origin=paste0((phenYr+1),'-1-1')),as.Date(364,origin=paste0((phenYr+1),'-1-1')),by='day'))


#---------------------------------------------------------------------
#Calculate pheno metrics for each pixel
#This version using alternate years to gap fill
#Code adapted by Douglas Bolton
#Based on MODIS C6 algorithm developed by Josh Gray
#---------------------------------------------------------------------
DoPhenologyMODIS <- function(red, nir, green, swir, snowPix, dates, phenYr, params, numLyrs){
  
  
  #Despike, calculate dormant value, fill snow with dormant value, despike poorly fit snow values
  log <- try({
    
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
  #If there is an error despiking or other initial steps, return NAs
  if(inherits(log, "try-error")){return(matrix(NA,pheno_pars$numLyrs))}   
  
  outAll=c()
  
  y <- 2
  
  log <- try({
    
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
    
    
    # QA
    if(length(full_segs)==1){
      qual_1 <- GetQAs(gup_rsq, gdown_rsq, gup_maxgap, gdown_maxgap, theOrd, qa_pars)
    }else{
      qual_1 <- GetQAs(gup_rsq, gdown_rsq, gup_maxgap, gdown_maxgap, theOrd, qa_pars)[[1]][1]
      qual_2 <- GetQAs(gup_rsq, gdown_rsq, gup_maxgap, gdown_maxgap, theOrd, qa_pars)[[2]][1]
    } 
    
    
    
    ################################################
    #If no cycles have good data, record NA output and move to next
    if (numCyc == 0) {outAll <- c(outAll,annualMetrics(viSub,dateSub,smoothed_vi,pred_dates,phenYr,pheno_pars));next}
    
    
    if (numRecords == 1) {
      #If only one cycle was recorded, report it, and fill NA values for second cycle
      out <- c(1,phen,seg_max,seg_amp,seg_int,qual_1,c(rep(NA,10),4),numObs)
      
    } else {
      #If there are multiple cycles, sort by amplitude and report two highest amplitudes (highest amplitude first)
      theOrd <- order(seg_amp,decreasing=T)   
      
      phen1 <- phen[seq(theOrd[1], length(phen), by = numRecords)]
      phen2 <- phen[seq(theOrd[2], length(phen), by = numRecords)]
      
      
      if (naCheck[theOrd[2]]) {
        #If the second cycle did not have enough observations (seg_metrics = NA), only report the first cycle
        out <- c(numCyc,phen1,seg_max[theOrd[1]],seg_amp[theOrd[1]],seg_int[theOrd[1]],qual_1,
                 c(rep(NA,10),4),numObs)  
      }else{
        out <- c(numCyc,phen1,seg_max[theOrd[1]],seg_amp[theOrd[1]],seg_int[theOrd[1]],qual_1,
                 phen2,seg_max[theOrd[2]],seg_amp[theOrd[2]],seg_int[theOrd[2]],qual_2,numObs) 
      }
    }
    
  },silent=TRUE)  #End of the try block
  
  if(inherits(log, "try-error")){
    outAll <- matrix(NA,pheno_pars$numLyrs)
  }else{
    outAll <- out
  }
  
  return(outAll)
  
}




