# # tiles <- c('h12v03','h12v04','h20v07','h20v08','h23v02','h27v05')
# # tiles <- c('h11v09','h12v04','h21v02')
# tiles <- c('h09v04','h09v05','h09v06','h10v03','h10v04',
#             'h10v05','h10v06','h11v03','h11v04','h11v05',
#             'h12v03','h12v05','h13v03','h13v04',
#             'h14v03','h14v04')
# tile <- c('h10v02','h11v02','h12v02','h13v02','h14v02',
#            'h08v04','h08v05','h08v06','h09v02','h09v03')
tiles <- c('h12v04')

setwd('/projectnb/modislc/users/twgreen/MODISProject/runEnd/')
for(i in 1:length(tiles)){
  for(year in 2014:2021){
    system(paste('qsub -N DaT',tiles[i],' -V -pe omp 4 -l h_rt=12:00:00 /usr3/graduate/twgreen/Github/TAscience/tristan/trend/run_clim_tair.sh ', tiles[i],year,sep=''))
    system(paste('qsub -N DaV',tiles[i],' -V -pe omp 4 -l h_rt=12:00:00 /usr3/graduate/twgreen/Github/TAscience/tristan/trend/run_vpd.sh ', tiles[i],year,sep=''))
    system(paste('qsub -N DaS',tiles[i],' -V -pe omp 4 -l h_rt=12:00:00 /usr3/graduate/twgreen/Github/TAscience/tristan/trend/run_sw_in.sh ', tiles[i],year,sep=''))
  }  
}

# setwd('/projectnb/modislc/users/twgreen/MODISProject/runEnd/')
# for(i in 1:length(tiles)){
#   for(year in 2003:2021){
#     system(paste('qsub -N DaG',tiles[i],' -V -pe omp 2 -l h_rt=12:00:00 /usr3/graduate/twgreen/R_Scripts/run_gppDM.sh ', tiles[i],year,sep=''))
#      }  
# }
# 
# setwd('/projectnb/modislc/users/twgreen/MODISProject/runEnd/')
# for(i in 1:length(tiles)){
#   for(year in 2003:2021){
#     system(paste('qsub -N DaG',tiles[i],' -V -pe omp 2 -l h_rt=12:00:00 /usr3/graduate/twgreen/R_Scripts/run_ugh_up.sh ', tiles[i],year,sep=''))
#   }  
# }


setwd('/projectnb/modislc/users/twgreen/MODISProject/runEnd/')
for(i in 1:length(tiles)){
  for(year in 2001:2021){
    system(paste('qsub -N MP1',tiles[i],' -V -pe omp 4 -l h_rt=12:00:00 /usr3/graduate/twgreen/Github/TAscience/tristan/trend/ModelRun/run_amergppDM_p1.sh ', tiles[i],year,sep=''))
    system(paste('qsub -N MP2',tiles[i],' -V -pe omp 4 -l h_rt=12:00:00 /usr3/graduate/twgreen.Github/TAscience/tristan/trend/ModelRun/run_amergppDM_p2.sh ', tiles[i],year,sep=''))
    system(paste('qsub -N MP3',tiles[i],' -V -pe omp 4 -l h_rt=12:00:00 /usr3/graduate/twgreen/Github/TAscience/tristan/trend/ModelRun/run_amergppDM_p3.sh ', tiles[i],year,sep=''))
    system(paste('qsub -N MP4',tiles[i],' -V -pe omp 4 -l h_rt=12:00:00 /usr3/graduate/twgreen/Github/TAscience/tristan/trend/ModelRun/run_amergppDM_p4.sh ', tiles[i],year,sep=''))
    system(paste('qsub -N MP5',tiles[i],' -V -pe omp 4 -l h_rt=12:00:00 /usr3/graduate/twgreen/Github/TAscience/tristan/trend/ModelRun/run_amergppDM_p5.sh ', tiles[i],year,sep=''))
    system(paste('qsub -N MP6',tiles[i],' -V -pe omp 4 -l h_rt=12:00:00 /usr3/graduate/twgreen/Github/TAscience/tristan/trend/ModelRun/run_amergppDM_p6.sh ', tiles[i],year,sep=''))
    system(paste('qsub -N MP7',tiles[i],' -V -pe omp 4 -l h_rt=12:00:00 /usr3/graduate/twgreen/Github/TAscience/tristan/trend/ModelRun/run_amergppDM_p7.sh ', tiles[i],year,sep=''))
    system(paste('qsub -N MP8',tiles[i],' -V -pe omp 4 -l h_rt=12:00:00 /usr3/graduate/twgreen/Github/TAscience/tristan/trend/ModelRun/run_amergppDM_p8.sh ', tiles[i],year,sep=''))
    system(paste('qsub -N MP9',tiles[i],' -V -pe omp 4 -l h_rt=12:00:00 /usr3/graduate/twgreen/Github/TAscience/tristan/trend/ModelRun/run_amergppDM_p9.sh ', tiles[i],year,sep=''))
    system(paste('qsub -N M10',tiles[i],' -V -pe omp 4 -l h_rt=12:00:00 /usr3/graduate/twgreen/Github/TAscience/tristan/trend/ModelRun/run_amergppDM_p10.sh ', tiles[i],year,sep=''))
    system(paste('qsub -N M11',tiles[i],' -V -pe omp 4 -l h_rt=12:00:00 /usr3/graduate/twgreen/Github/TAscience/tristan/trend/ModelRun/run_amergppDM_p11.sh ', tiles[i],year,sep=''))
    system(paste('qsub -N M12',tiles[i],' -V -pe omp 4 -l h_rt=12:00:00 /usr3/graduate/twgreen/Github/TAscience/tristan/trend/ModelRun/run_amergppDM_p12.sh ', tiles[i],year,sep=''))
    system(paste('qsub -N M13',tiles[i],' -V -pe omp 4 -l h_rt=12:00:00 /usr3/graduate/twgreen/Github/TAscience/tristan/trend/ModelRun/run_amergppDM_p13.sh ', tiles[i],year,sep=''))
    system(paste('qsub -N M14',tiles[i],' -V -pe omp 4 -l h_rt=12:00:00 /usr3/graduate/twgreen/Github/TAscience/tristan/trend/ModelRun/run_amergppDM_p14.sh ', tiles[i],year,sep=''))
    system(paste('qsub -N M15',tiles[i],' -V -pe omp 4 -l h_rt=12:00:00 /usr3/graduate/twgreen/Github/TAscience/tristan/trend/ModelRun/run_amergppDM_p15.sh ', tiles[i],year,sep=''))
    system(paste('qsub -N M16',tiles[i],' -V -pe omp 4 -l h_rt=12:00:00 /usr3/graduate/twgreen/Github/TAscience/tristan/trend/ModelRun/run_amergppDM_p16.sh ', tiles[i],year,sep=''))
  }
}



