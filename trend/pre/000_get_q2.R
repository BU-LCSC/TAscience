# From bash
args <- commandArgs()
print(args)

year <- as.numeric(args[3])


############################################################
year <- 2022
tile_list <- substr(list.files('/projectnb/modislc/projects/sat/data/mcd12q/q2/c61/2021/'),18,23)


############################################################
# Get C61 MCD12Q2 product
setwd('/projectnb/modislc/projects/sat/data/mcd12q/q2/c61/')
url <- paste('https://e4ftl01.cr.usgs.gov/MOTA/MCD12Q2.061/',year,'.01.01/',sep='')

for(i in 1:length(tile_list)){
  system(paste('wget --user=mkmoon --password=M159k258! -l1 -r --no-parent -A "MCD12Q2.A*.',tile_list[i],'.061.*.hdf" ',url,sep=''))
  print(i)
}


############################################################
# Get C61 MCD12Q1 product
setwd('/projectnb/modislc/projects/sat/data/mcd12q/q1/')
url <- paste('https://e4ftl01.cr.usgs.gov/MOTA/MCD12Q1.061/',year,'.01.01/',sep='')

for(i in 1:length(tile_list)){
  system(paste('wget --user=mkmoon --password=M159k258! -l1 -r --no-parent -A "MCD12Q1.A*.',tile_list[i],'.061.*.hdf" ',url,sep=''))
  print(i)
}
