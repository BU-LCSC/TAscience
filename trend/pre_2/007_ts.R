rm(list = ls())

library(terra)

############################################################
# From bash code
args <- commandArgs()
print(args)

tile <- substr(args[3],1,6)
# tile <- 'h12v04'

print(tile)
print(cc)


############################################################
# NBAR files
path <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/nbar/',tile,'/',sprintf('%02d',cc))
files <- list.files(path,pattern=glob2rx('evi*.rda'),full.names=T)

print(files)

###
if(length(files)==14){
  ## Before
  Dat <- vector('list',7)
  for(i in 1:7){
    load(files[i])

    if(cc==1){    dat <- dat1
    }else if(cc==2){    dat <- dat2
    }else if(cc==3){    dat <- dat3
    }else if(cc==4){    dat <- dat4
    }else if(cc==5){    dat <- dat5
    }else if(cc==6){    dat <- dat6
    }else if(cc==7){    dat <- dat7
    }else if(cc==8){    dat <- dat8
    }else          {    dat <- dat9
    }

    if(i==1){
      # if(dim(dat)[1]>30000){
      #   sam <- sample(1:dim(dat)[1],30000)
      # }else{
      #   sam <- sample(1:dim(dat)[1],dim(dat)[1])
      # }
      if(cc==6){
        sam <- sample(1:dim(dat)[1],round(dim(dat)[1])*0.04)
      }else{
        sam <- sample(1:dim(dat)[1],round(dim(dat)[1])*0.02)
      }
    }
    Dat[[i]] <- dat[sam,]

    print(i)
  }
  print(length(sam))

  dat11 <- matrix(NA,length(sam),365)
  for(i in 1:length(sam)){
    t1 <- Dat[[1]][i,];  t2 <- Dat[[2]][i,];  t3 <- Dat[[3]][i,];  t4 <- Dat[[4]][i,]
    t5 <- Dat[[5]][i,];  t6 <- Dat[[6]][i,];  t7 <- Dat[[7]][i,]

    ts      <- rbind(t1,t2,t3,t4,t5,t6,t7)
    dat11[i,] <- apply(ts,2,median,na.rm=T)

    if(i%%100==0) print(i)
  }


  ## After
  Dat <- vector('list',7)
  for(i in 1:7){
    load(files[i+7])

    if(cc==1){    dat <- dat1
    }else if(cc==2){    dat <- dat2
    }else if(cc==3){    dat <- dat3
    }else if(cc==4){    dat <- dat4
    }else if(cc==5){    dat <- dat5
    }else if(cc==6){    dat <- dat6
    }else if(cc==7){    dat <- dat7
    }else if(cc==8){    dat <- dat8
    }else          {    dat <- dat9
    }

    Dat[[i]] <- dat[sam,]

    print(i)
  }

  dat22 <- matrix(NA,length(sam),365)
  for(i in 1:length(sam)){
    t1 <- Dat[[1]][i,];  t2 <- Dat[[2]][i,];  t3 <- Dat[[3]][i,];  t4 <- Dat[[4]][i,]
    t5 <- Dat[[5]][i,];  t6 <- Dat[[6]][i,];  t7 <- Dat[[7]][i,]

    ts      <- rbind(t1,t2,t3,t4,t5,t6,t7)
    dat22[i,] <- apply(ts,2,median,na.rm=T)

    if(i%%100==0) print(i)
  }

  # Save files
  if(dim(dat11)[1]>100 & dim(dat11)[1]==dim(dat22)[1] & dim(dat11)[2]==dim(dat22)[2]){
    outDir <- paste0('/projectnb/modislc/users/mkmoon/TAscience/trend/data/nbar/bfoaft/',sprintf('%02d',cc))
    if (!dir.exists(outDir)) {dir.create(outDir)}
    save(dat11,file=paste0(outDir,'/ts_1_',tile,'.rda'))
    save(dat22,file=paste0(outDir,'/ts_2_',tile,'.rda'))

    print('Done!')
  }
}