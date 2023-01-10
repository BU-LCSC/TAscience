###############################
### Download Q2 product
year <- 2008

setwd('/projectnb/modislc/projects/sat/data/mcd12q/')
for(year in 2011:2019){
  system(paste('qsub -N Q',year,' -V -l h_rt=12:00:00 /usr3/graduate/mkmoon/GitHub/TAscience/trend/run_000.sh ',year,sep=''))    
}


years <- c(2017,2015,2016,2018,2006,2014,2020,2008:2013,2019)
for(i in 8:14){
  system(paste('qsub -N Q',i,' -hold_jid Q',i-1,' -V -l h_rt=12:00:00 /usr3/graduate/mkmoon/GitHub/TAscience/trend/run_000.sh ',years[i],sep=''))
}



###############################
# Save metrics
setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/data/runLogs/')
for(tt in c(1:98)){
  for(mm in 1:25){
    tile <- sprintf('%03d',tt)
    system(paste('qsub -V -pe omp 2 -l h_rt=12:00:00 /usr3/graduate/mkmoon/GitHub/TAscience/trend/run_001.sh ',tile,mm,sep=''))  
  }
}

# # 10 by 10
# setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/data/runLogs/')
# for(tt in c(1:315)){
#   for(mm in 1:25){
#     tile <- sprintf('%03d',tt)
#     system(paste('qsub -V -pe omp 2 -l h_rt=12:00:00 /usr3/graduate/mkmoon/GitHub/TAscience/trend/run_001_10by10.sh ',tile,mm,sep=''))  
#   }
# }


###############################
# Save change as rasters
setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/data/runLogs/')
for(tt in 1:98){
  tile <- sprintf('%03d',tt)
  # for(ii in 1:6){
    system(paste('qsub -V -pe omp 4 -l h_rt=12:00:00 /usr3/graduate/mkmoon/GitHub/TAscience/trend/run_002.sh ',tile,sep=''))    
  # }
}


###############################
# Calculate PC and Cluster
setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/data/runLogs/')
# system(paste('qsub -V -pe omp 8 -l h_rt=12:00:00 /usr3/graduate/mkmoon/GitHub/TAscience/trend/run_003.sh ')) 
for(tt in 1:315){
  system(paste('qsub -V -pe omp 4 -l h_rt=12:00:00 /usr3/graduate/mkmoon/GitHub/TAscience/trend/run_003.sh ',tt,sep=''))      
}




###############################
# Map variables
setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/data/runLogs/')
for(tt in 1:6){
  system(paste('qsub -V -pe omp 4 -l h_rt=12:00:00 /usr3/graduate/mkmoon/GitHub/TAscience/trend/run_004.sh ',tt,sep=''))      
}
for(tt in 1:25){
  system(paste('qsub -V -pe omp 4 -l h_rt=12:00:00 /usr3/graduate/mkmoon/GitHub/TAscience/trend/run_004_1.sh ',tt,sep=''))      
}




###############################
# Get NBAR ts
tiles <- c('h08v06','h09v05',
           'h10v03','h10v05','h10v09',
           'h11v02','h11v04','h11v08','h11v10',
           'h12v02','h12v03','h12v04','h12v09','h12v11',
           'h13v02','h13v03','h13v09','h13v10','h13v11',
           'h14v01','h14v03','h14v09',
           'h17v03','h17v05','h17v07',
           'h18v02','h18v04','h18v08',
           'h19v03','h19v07','h19v09',
           'h20v02','h20v04','h20v06','h20v08','h20v10','h20v11',
           'h21v01','h21v03','h21v05','h21v07','h21v09',
           'h22v02','h22v04','h22v06','h22v08',
           'h23v03','h23v05',
           'h24v02','h24v04','h24v06',
           'h25v03','h25v05','h25v07',
           'h26v04','h26v06',
           'h27v05','h27v06','h27v07',
           'h28v06','h28v08','h28v11',
           'h29v09','h29v11',
           'h30v10','h30v12',
           'h31v11',
           'h32v09')
setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/data/runLogs/')
for(tt in 61:68){
  for(yy in c(2001:2007,2015:2021)){
    system(paste('qsub -V -pe omp 8 -l h_rt=12:00:00 /usr3/graduate/mkmoon/GitHub/TAscience/trend/run_005.sh ',tiles[tt],yy,sep=''))      
  }
}

##
setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/data/runLogs/')
for(tt in 1:68){
  for(cc in 6){
    system(paste('qsub -V -pe omp 4 -l h_rt=12:00:00 /usr3/graduate/mkmoon/GitHub/TAscience/trend/run_006.sh ',tiles[tt],cc,sep=''))      
  }
}

## Plot TS
setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/data/runLogs/')
for(cc in 1:9){
  system(paste('qsub -V -pe omp 2 -l h_rt=12:00:00 /usr3/graduate/mkmoon/GitHub/TAscience/trend/run_006.sh ',cc,sep=''))      
}
