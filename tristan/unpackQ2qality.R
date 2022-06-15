library(sp)
library(raster)
library(gdalUtils)
library(rgdal)


##
UnpackDetailedQA <- function(x){
  bits <- as.integer(intToBits(x))
  quals <- sapply(seq(1, 16, by=2), function(i) sum(bits[i:(i+1)] * 2^c(0, 1)))[1:7]
  return(quals)
}

# Base image
q1 <- list.files(path='/projectnb/modislc/projects/sat/data/mcd12q/q1/2020.01.01',pattern=glob2rx(paste0('*h10v03*.hdf')),recursive=T,full.names=T)
imgBase <- raster(get_subdatasets(q1[1])[[1]])

# list of q2 product files
files <- list.files('/projectnb/modislc/projects/sat/data/mcd12q/q2',pattern=glob2rx('*h10v03*.hdf'),full.names = T,recursive = T)

# loop for years
# for(i in 1:19){
  
i = 5

  qaVals <- matrix(NA,length(imgBase),7)
  # get QA layer
  sds <- get_subdatasets(files[i])
  qa <- raster(sds[13])
  qa[qa>32000] <- NA
  qaV <- values(qa)
  
  # find non-NA pixels
  nonNA <- which(!is.na(qaV))
  print(length(nonNA))
  # loop for only non-NA pixels
  for(j in 1:length(nonNA)){
    qaVals[nonNA[j],] <- UnpackDetailedQA(qaV[nonNA[j]])  
    if(j%%100000==0) print(j)
  }
  
# }

qaGup <- setValues(imgBase,qaVals[,1])
plot(qaGup,colNA='grey30')

sum(values(qaGup)==0,na.rm=T)/length(nonNA)*100
