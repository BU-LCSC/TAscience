###############################
library(rjson)
params <- fromJSON(file='/usr3/graduate/mkmoon/GitHub/TAscience/mcd12q2/Applying_MSLSP/Q2_Parameters.json')

###############################
### 01_Make image chunks
setwd(paste0(params$setup$logDir,'01'))
for(tile in 'h10v03'){
  for(year in 2016:2020){
    system(paste('qsub -V -pe omp ',params$setup$numCores,' -l h_rt=12:00:00 ',params$setup$Script,'run_01_make_chunks.sh ',tile,year,sep=''))  
  }
}



### 02_Make pheno-metrics chunks 
setwd(paste0(params$setup$logDir,'02'))
for(tile in 'h10v03'){
  for(year in 2001:2020){
    # for(cc in 1:params$setup$numChunks){
      system(paste('qsub -V -l h_rt=12:00:00 ',params$setup$Script,'run_02_make_phe_chunks.sh ',tile,year,cc,sep=''))
    # }  
  }
}
