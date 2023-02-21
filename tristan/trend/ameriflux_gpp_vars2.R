library(easypackages)
libraries('plyr','dplyr', 'sp', 'raster', 'sf','terra','npreg', 'zyp')

files <- list.files(path='/projectnb/modislc/users/twgreen/MODISProject/Am_fluxnet/dbfmf/', 
                    recursive = TRUE,
                    pattern = 'FULLSET_DD',
                    full.names = TRUE)
filesa <- list.files(path='/projectnb/modislc/users/twgreen/MODISProject/Am_fluxnet/dbfmf/', 
                    recursive = TRUE,
                    pattern = 'FULLSET_YY',
                    full.names = TRUE)

toDate <- function(year, month, day) {
  ISOdate(year, month, day)
}

totlist <- vector('list',length(files))
for(i in 1:length(files)){
  tryCatch( {
  sitepath <- files[i]
  sitedf <- read.csv(sitepath, na.strings = c('-9999'))
  sitenm <- substr(sitepath, 65, 74)
  Year <- as.numeric(substr(sitedf$TIMESTAMP, 1, 4))
  Month <- as.numeric(substr(sitedf$TIMESTAMP, 5, 6))
  Day <- as.numeric(substr(sitedf$TIMESTAMP, 7, 8 ))
  Date <- toDate(Year, Month, Day)
  DoY <- as.numeric(strftime(Date, format = '%j'))
  GPP <- sitedf$GPP_DT_VUT_REF
  
  inddf <- data.frame(sitenm, Year, DoY, GPP)
  filtdf <- filter(inddf, Year > 2000)
  filtdf <- na.omit(filtdf)

  stYear <- filtdf$Year[1]
  edYear <- filtdf$Year[length(filtdf$Year)]
  
  lsdf <- vector('list', length(unique(filtdf$Year)))
  for (gg in stYear:edYear){
    yeardf <- filter(filtdf, Year == gg)
    mod.ss <- ss(yeardf$DoY, yeardf$GPP, nknots = 10)

    yeardf$GPPy <- mod.ss$y

    maxGPP <- max(yeardf$GPPy)

    GPPAmp <- filter(yeardf, GPPy == maxGPP)

    GPPFHdf <- filter(yeardf, DoY <= GPPAmp$DoY)
    GPPBHdf <- filter(yeardf, DoY >= GPPAmp$DoY)
    GPPP15 <- GPPFHdf[which.min(abs((.25*maxGPP + min(GPPFHdf$GPPy)) - GPPFHdf$GPPy)),]
    GPPB15 <- GPPBHdf[which.min(abs((.25-maxGPP + min(GPPBHdf$GPPy)) - GPPBHdf$GPPy)),]
    GPPGSL <- GPPB15$DoY - GPPP15$DoY

    makdf <- data.frame(sitenm, gg, maxGPP, GPPGSL) 
    x <- gg + 1 - stYear
    lsdf[[x]] <- makdf
  }
  lsdfa <- ldply(lsdf, data.frame)
  lsdfa[lsdfa == 'NaN'] <- NA
  totlist[[i]] <- lsdfa

  print(i)
}, error=function(e){cat("ERROR :", conditionMessage(e), "\n")})
}

totlista <- vector('list',length(files))
for(i in 1:length(files)){
  tryCatch( {
    sitepath <- filesa[i]
    sitedf <- read.csv(sitepath, na.strings = c('-9999'))
    sitenm <- substr(sitepath, 65, 74)
    Year <- as.numeric(substr(sitedf$TIMESTAMP, 1, 4))
    GPP <- sitedf$GPP_DT_VUT_REF
    
    inddf <- data.frame(sitenm,Year,GPP)
    filtdf <- filter(inddf, Year > 2000)
    filtdf <- na.omit(filtdf)
    
    totlista[[i]] <- filtdf
    print(i)
  }, error=function(e){cat("ERROR :", conditionMessage(e), "\n")})
}

totdfa <- ldply(totlista, data.frame)
totdfa[totdfa == 'NaN'] <- NA

totdf <- ldply(totlist, data.frame)
totdf[totdf == 'NaN'] <- NA

totdfb <- merge(totdf, totdfa, by=c('sitenm','Year'))


colnames(totdf) <- c('sitenm','Year','GPP_Max','GPP_GSL')
