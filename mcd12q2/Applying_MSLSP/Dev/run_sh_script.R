###############################
library(rjson)
params <- fromJSON(file='/usr3/graduate/mkmoon/GitHub/TAscience/mcd12q2/Applying_MSLSP/Q2_Parameters.json')

###############################
### Compare with Product
setwd(paste0(params$setup$logDir,'comp'))
for(i in 0:99){
  system(paste('qsub -N ck_sp -V -pe omp 8 -l h_rt=03:00:00 -l mem_total=48G ',params$setup$Script,'Dev/run_script.sh ',i,sep=''))  
}

