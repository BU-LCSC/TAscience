###############################
# Get the products
setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/data/runLogs/')
system('qsub -V -pe omp 2 -l h_rt=12:00:00 /usr3/graduate/mkmoon/GitHub/TAscience/trend/pre/run_000.sh')  


###############################
# Save metrics, calculate changes, and save them as rasters
setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/data/runLogs/')
for(tt in 1:315){
  for(mm in 1:6){
    tile <- sprintf('%03d',tt)
    system(paste('qsub -V -pe omp 2 -l h_rt=12:00:00 /usr3/graduate/mkmoon/GitHub/TAscience/trend/run_001.sh ',tile,mm,sep=''))  
  }
}

# Merge
setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/data/runLogs/')
for(vv in 1:6){
  system(paste('qsub -V -pe omp 4 -l h_rt=12:00:00 /usr3/graduate/mkmoon/GitHub/TAscience/trend/run_002.sh ',vv,sep=''))    
}


###############################
## Trends by eco-regionss
setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/data/runLogs/')
for(mm in 1:6){
  for(rr in 1:44){
    vari <- sprintf('%03d',mm)
    ecor <- sprintf('%03d',rr)
    system(paste('qsub -V -pe omp 28 -l h_rt=12:00:00 /usr3/graduate/mkmoon/GitHub/TAscience/trend/run_004.sh ',vari,ecor,sep=''))        
  }
}


###############################
## Climate change
setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/data/runLogs/')
for(vv in 1:15){
  vari <- sprintf('%02d',vv)
  system(paste('qsub -V -pe omp 4 -l h_rt=12:00:00 /usr3/graduate/mkmoon/GitHub/TAscience/trend/run_006.sh ',vari,sep=''))    
}



###############################
## attribution
setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/data/runLogs/')
for(mm in 1:6){
  for(rr in 1:44){
    vari <- sprintf('%03d',mm)
    ecor <- sprintf('%03d',rr)
    system(paste('qsub -V -pe omp 2 -l h_rt=12:00:00 /usr3/graduate/mkmoon/GitHub/TAscience/trend/run_007.sh ',vari,ecor,sep=''))        
  }
}
