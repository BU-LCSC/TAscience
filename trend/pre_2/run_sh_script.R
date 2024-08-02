###############################
## Metrics
setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/data/runLogs/')
for(mm in c(2,6,9,16,13,15,14,20,23)){
# for(mm in c(9)){
  for(tt in 1:315){
    tile <- sprintf('%03d',tt)
    vari <- sprintf('%03d',mm)
    system(paste('qsub -V -pe omp 4 -l h_rt=12:00:00 /usr3/graduate/mkmoon/GitHub/TAscience/trend/run_001.sh ',tile,vari,sep=''))        
  }
}

## Merge maps
setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/data/runLogs/')
for(vv in c(2,6,9,16,13,15,14,20,23)){
  system(paste('qsub -V -pe omp 8 -l h_rt=12:00:00 /usr3/graduate/mkmoon/GitHub/TAscience/trend/run_001.sh ',vv,sep=''))        
}



###############################
## Trends by ecoregions
setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/data/runLogs/')
# for(mm in c(2,6,9,16,14,15)){
for(mm in c(2,6,9,16,14,15)){
  for(rr in 1:58){
    vari <- sprintf('%03d',mm)
    ecor <- sprintf('%03d',rr)
    system(paste('qsub -V -pe omp 4 -l mem_per_core=8G -l h_rt=12:00:00 /usr3/graduate/mkmoon/GitHub/TAscience/trend/run_003.sh ',vari,ecor,sep=''))        
  }
}


###############################
## Climate change
setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/data/runLogs/')
for(vv in 1:15){
  vari <- sprintf('%02d',vv)
  system(paste('qsub -V -pe omp 2 -l h_rt=12:00:00 /usr3/graduate/mkmoon/GitHub/TAscience/trend/run_005.sh ',vari,sep=''))    
}



###############################
## attribution
setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/data/runLogs/')
for(mm in 1:6){
  for(rr in 1:44){
    vari <- sprintf('%03d',mm)
    ecor <- sprintf('%03d',rr)
    system(paste('qsub -V -pe omp 2 -l h_rt=12:00:00 /usr3/graduate/mkmoon/GitHub/TAscience/trend/run_006.sh ',vari,ecor,sep=''))        
  }
}
