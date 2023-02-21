###############################
# Get NBAR data
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
for(i in 61:68){
  for(year in c(2001:2007,2015:2021)){
    # if(i==56){
      # system(paste('qsub -N N',tiles[i],' -V -pe omp 2 -l h_rt=12:00:00 /usr3/graduate/mkmoon/GitHub/TAscience/trend/nbar/run_nbara4.sh ', tiles[i],year,sep=''))  
      # system(paste('qsub -N N',tiles[i],' -V -pe omp 2 -l h_rt=12:00:00 /usr3/graduate/mkmoon/GitHub/TAscience/trend/nbar/run_nbara2.sh ', tiles[i],year,sep=''))  
    # }else{
      system(paste('qsub -hold_jid N',tiles[i-1],' -N N',tiles[i],' -V -pe omp 2 -l h_rt=12:00:00 /usr3/graduate/mkmoon/GitHub/TAscience/trend/nbar/run_nbara4.sh ', tiles[i],year,sep=''))  
      # system(paste('qsub -hold_jid N',tiles[i-1],' -N N',tiles[i],' -V -pe omp 2 -l h_rt=12:00:00 /usr3/graduate/mkmoon/GitHub/TAscience/trend/nbar/run_nbara2.sh ', tiles[i],year,sep=''))  
    # }
  }  
}


# Get NBAR omitted
setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/data/runLogs/')
for(i in 57:68){
  for(year in c(2001:2007,2015:2021)){
    # if(i==31){
      system(paste('qsub -N O',tiles[i],' -V -pe omp 2 -l h_rt=12:00:00 /usr3/graduate/mkmoon/GitHub/TAscience/trend/nbar/run_nbara4_omit.sh ', tiles[i],year,sep=''))
      # system(paste('qsub -N O',tiles[i],' -V -pe omp 2 -l h_rt=12:00:00 /usr3/graduate/mkmoon/GitHub/TAscience/trend/nbar/run_nbara2_omit.sh ', tiles[i],year,sep=''))
    # }else{
      # system(paste('qsub -hold_jid O',tiles[i-1],' -N O',tiles[i],' -V -pe omp 2 -l h_rt=12:00:00 /usr3/graduate/mkmoon/GitHub/TAscience/trend/nbar/run_nbara4_omit.sh ', tiles[i],year,sep=''))
      # system(paste('qsub -hold_jid O',tiles[i-1],' -N O',tiles[i],' -V -pe omp 2 -l h_rt=12:00:00 /usr3/graduate/mkmoon/GitHub/TAscience/trend/nbar/run_nbara2_omit.sh ', tiles[i],year,sep=''))
    # }
  }
}


###############################
# Run "change_folder_name.R" code
setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/data/runLogs/')
for(i in 1){
  for(year in 2000:2014){
    system(paste('qsub -N cfld',tiles[i],' -V -pe omp 2 -l h_rt=12:00:00 /usr3/graduate/mkmoon/GitHub/TAscience/trend/nbar/run_change_folder.sh ', tiles[i], year,sep=''))  
  }
}

###############################
# Run spline code 
tiles <- c('h12v04')

setwd('/projectnb/modislc/users/mkmoon/LCD_C6/v7/des_sp/spline/h12v04')
for(i in 1){
  #Create output directory if it doesn't exist
  for(start_yr in 2017){
    # outDir <- paste0('/projectnb/modislc/users/mkmoon/LCD_C6/C6_1/spline_out/',tiles[i],'/',(start_yr+2))
    # if (!dir.exists(outDir)) {dir.create(outDir)}
    system(paste('qsub /projectnb/modislc/users/mkmoon/LCD_C6/C6_1/run_spline_fromJosh.sh ', tiles[i],' ',start_yr,sep=''))    
  }
}


# To see splined results
setwd('/projectnb/modislc/users/mkmoon/LCD_C6/v9/runLogs/')
for(i in 0:40){
  system(paste('qsub -N ck_sp -V -pe omp 8 -l h_rt=03:00:00 -l mem_total=32G /usr3/graduate/mkmoon/GitHub/MCD12Q2/run_ck_spline.sh ',i,sep=''))  
}



###################
# Check NBAR
setwd('/projectnb/modislc/users/mkmoon/LCD_C6/C6_1/figures/nbar/')
for(i in 1:3){
  system(paste('qsub -N NbCk',tiles[i],' -V -pe omp 2 -l h_rt=12:00:00 /usr3/graduate/mkmoon/GitHub/MCD12Q2/run_check_nbar.sh ',tiles[i],sep=''))  
}



