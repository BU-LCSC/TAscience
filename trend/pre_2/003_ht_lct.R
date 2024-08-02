rm(list = ls())

library(terra)
library(sf)

library(remotePARTS)

library(RColorBrewer)


############################################################
# From bash code
args <- commandArgs()
print(args)

rr <- as.numeric(substr(args[3],4,6))
# rr <- 2 # ABoVE

############################################################
refReg <- vect('/projectnb/modislc/users/mkmoon/TAscience/trend/data/shp/IPCC-WGI-reference-regions-v4.shp')
rrName <- unique(refReg$Acronym)
rName  <- rrName[rr]

# Load files
outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/parts/vals/02'
files  <- list.files(outDir,pattern=glob2rx('coef_coords_lc*.rda'),full.names=T)

lct   <- vector('list',315)
for(i in 1:315){
  load(files[i])
  rSub <- which(e1==rName)

  if(length(rSub)==1){
    tmp <- which(e2==(rSub-1))
    d2 <- lct_ch[tmp,]
    
    lct[[i]] <- d2[which(!is.na(d2[,2]) & d2[,2]!=15 & d2[,2]!=16 & d2[,2]!=17),2]
  }
  print(i)
}

## Save
outDir <- '/projectnb/modislc/users/mkmoon/TAscience/trend/data/rasters/parts/vals/by_eco/lct'
if (!dir.exists(outDir)) {dir.create(outDir)}
save(lct,file=paste0(outDir,'/lct_',rName,'.rda'))

print(table(lct))
