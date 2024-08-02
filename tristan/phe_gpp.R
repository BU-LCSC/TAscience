library(terra)
library(sf)

library(rjson)


###############################
params <- fromJSON(file='/usr3/graduate/mkmoon/GitHub/PlanetLSP/data_paper/PLSP_Parameters.json')
source(params$setup$rFunctions)

params$phenology_parameters$gup_threshes   <- c(0.25,0.50,0.90)
params$phenology_parameters$gdown_threshes <- c(0.90,0.50,0.25)

dat <- readRDS('/projectnb/modislc/users/twgreen/MODISProject/RData/FLUXNET/mat4minkyu.rds')


##########################################
DoPhenologyGPP <- function(gpp, params){
  
  # Despike, calculate dormant value, fill negative VI values with dormant value
  pheno_pars <- params$phenology_parameters
  qa_pars    <- params$qa_parameters
  
  vi   <- gpp
  
  # Replace negative VIs with dormant value
  vi_dorm <- quantile(vi,probs=pheno_pars$dormantQuantile,na.rm=T)   # Calc vi dormant value using non-negative VIs
  vi[vi < vi_dorm] <- vi_dorm
  
  smoothed <- smooth.spline(1:365,vi,spar=pheno_pars$splineSpar)
  smoothed_vi <- smoothed$y
  
  pred_dates <- 1:365
  dateSub    <- 1:365
  viSub      <- vi
  
  log <- try({    
    ################################################
    #Fit phenology
    peaks <- FindPeaks(smoothed_vi)
    if (all(is.na(peaks))) {outAll <- c(outAll,annualMetrics(viSub,dateSub,smoothed_vi,pred_dates,phenYrs[y],pheno_pars,vi_dorm,waterMask));next}
      
    #Find full segments
    full_segs <- GetSegs(peaks, smoothed_vi, pheno_pars)
    if (is.null(full_segs)) {outAll <- c(outAll,annualMetrics(viSub,dateSub,smoothed_vi,pred_dates,phenYrs[y],pheno_pars,vi_dorm,waterMask));next}
    
    #Get PhenoDates
    pheno_dates <- GetPhenoDates(full_segs, smoothed_vi, pred_dates, pheno_pars)
    phen <- unlist(pheno_dates, use.names=F)
    if (all(is.na(phen))) {outAll <- c(outAll,annualMetrics(viSub,dateSub,smoothed_vi,pred_dates,phenYrs[y],pheno_pars,vi_dorm,waterMask));next}
      
    #EVI layers
    seg_metrics <- lapply(full_segs, GetSegMetrics,smoothed_vi,viSub,pred_dates,dateSub) #full segment metrics
    un <- unlist(seg_metrics, use.names=F)
    ln <- length(un)
    seg_amp <- un[seq(1, ln, by=9)]
    seg_max <- un[seq(2, ln, by=9)]
      
    ##
    theOrd <- order(seg_amp,decreasing=T)   
      
    numRecords <- length(seg_amp)  #how many cycles were recorded
    naCheck <- is.na(seg_amp)
    numCyc <- sum(naCheck == 0)  #how many cycles have good data (seg metrics has valid observations)
      
    ################################################
    if(numCyc == 0){outAll <- c(outAll,annualMetrics(viSub,dateSu,smoothed_vi,pred_dates,phenYrs[y],pheno_pars,vi_dorm,waterMask));next}
      
    if(numRecords == 1) {
      out <- c(1,phen,seg_max,seg_amp,rep(NA,9))
    }else{
      phen1 <- phen[seq(theOrd[1], length(phen), by = numRecords)]
      phen2 <- phen[seq(theOrd[2], length(phen), by = numRecords)]
      if(naCheck[theOrd[2]]){
        out <- c(numCyc,phen1,seg_max[theOrd[1]],seg_amp[theOrd[1]],rep(NA,9))  
      }else{
        out <- c(numCyc,phen1,seg_max[theOrd[1]],seg_amp[theOrd[1]],
                 phen2,seg_max[theOrd[2]],seg_amp[theOrd[2]])  
      }
    }
      
    },silent=TRUE) #End of the try block
  
  if(inherits(log, "try-error")){
    out <- rep(NA,19)
  }
  
  return(out)
}

##########################################
pheno_mat <- matrix(NA,dim(dat)[1],19)
for (i in 1:dim(dat)[1]){
  gpp <- as.numeric(dat[i,5:369])
  pheno_mat[i,] <- DoPhenologyGPP(gpp, params)
}

gsl <- pheno_mat[,8]-pheno_mat[,2]  
gsl <- matrix(NA,596,1)
for(i in 1:596){
  if(is.na(pheno_mat[i,17])){
    gsl[i] <- pheno_mat[i,8]-pheno_mat[i,2]  
  }else{
    tmp <- c(pheno_mat[i,2],pheno_mat[i,8],pheno_mat[i,11],pheno_mat[i,17])
    gsl[i] <- max(tmp)-min(tmp)
  }
}

par(mfrow=c(1,2))
plot(gsl,dat[,4],xlab='new_max',ylab='org_max')
abline(0,1)
plot(pheno_mat[,10],dat[,3],xlab='new_max',ylab='org_max')
abline(0,1)
# Save
write.csv(pheno_mat,file='/projectnb/modislc/users/mkmoon/TAscience/gpp_gsl_max_25.csv')


