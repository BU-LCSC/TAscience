# Get omitted list of NBAR
args <- commandArgs()
print(args)

tile <- substr(args[3],1,6)
year <- as.numeric(substr(args[3],7,10))
# tile <- 'h11v04'; year <- 2004

# 2016.12.31
setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/data/nbar/')
yy <- year
mm1 <- '01'
mm2 <- '12'
dd1 <- '01'
if(yy==2004|yy==2008|yy==2012|yy==2016|yy==2020){
  dd2 <- '30'
}else{
  dd2 <- '31'
}
doy <- '365'

url <- paste('https://e4ftl01.cr.usgs.gov/MOTA/MCD43A4.061/',yy,'.',mm1,'.',dd1,'/',sep='')
system(paste('wget --user=mkmoon --password=M159k258! -l1 -r --no-parent -A "MCD43A4.A',yy,'001.',tile,'.061.*.hdf" ',url,sep=''))
url <- paste('https://e4ftl01.cr.usgs.gov/MOTA/MCD43A4.061/',yy,'.',mm2,'.',dd2,'/',sep='')
system(paste('wget --user=mkmoon --password=M159k258! -l1 -r --no-parent -A "MCD43A4.A',yy,doy,'.',tile,'.061.*.hdf" ',url,sep=''))

# for MCD43A2
sstr <- paste('*A',year,'*',tile,'*',sep='')
files <- list.files('/projectnb/modislc/users/mkmoon/TAscience/trend/data/nbar/e4ftl01.cr.usgs.gov/MOTA/MCD43A4.061',pattern=glob2rx(sstr),recursive=T)
dates <- substr(files,1,10)
omitted <- NULL
k <- 1
for(i in 1:(length(dates)-1)){
  if((as.numeric(as.Date(dates[i+1],'%Y.%m.%d')) - as.numeric(as.Date(dates[i],'%Y.%m.%d')))!=1){
    lng <- as.numeric(as.Date(dates[i+1],'%Y.%m.%d')) - as.numeric(as.Date(dates[i],'%Y.%m.%d')) - 1
    for(j in 1:lng){
      omitted[k] <- as.character(as.Date((as.numeric(as.Date(dates[i],'%Y.%m.%d'))+j),origin='1970-1-1'))
      k <- k + 1
    }
  } 
}
# omitted <- omitted[-which(omitted=="2016-12-31")]
print(omitted)

nrun <- 1
while(length(omitted)>0){
  setwd('/projectnb/modislc/users/mkmoon/TAscience/trend/data/nbar/')
  for(i in 1:length(omitted)){
    yy <- substr(omitted[i],1,4)
    mm <- substr(omitted[i],6,7)
    dd <- substr(omitted[i],9,10)
    doy <- strftime(omitted[i],'%j')
    
    url <- paste('https://e4ftl01.cr.usgs.gov/MOTA/MCD43A4.061/',yy,'.',mm,'.',dd,'/',sep='')
    system(paste('wget --user=mkmoon --password=M159k258! -l1 -r --no-parent -A "MCD43A4.A',yy,doy,'.',tile,'.061.*.hdf" ',url,sep=''))
  }
  
  files <- list.files('/projectnb/modislc/users/mkmoon/TAscience/trend/data/nbar/e4ftl01.cr.usgs.gov/MOTA/MCD43A4.061',pattern=glob2rx(sstr),recursive=T)
  dates <- substr(files,1,10)
  omitted <- NULL
  k <- 1
  for(i in 1:(length(dates)-1)){
    if((as.numeric(as.Date(dates[i+1],'%Y.%m.%d')) - as.numeric(as.Date(dates[i],'%Y.%m.%d')))!=1){
      lng <- as.numeric(as.Date(dates[i+1],'%Y.%m.%d')) - as.numeric(as.Date(dates[i],'%Y.%m.%d')) - 1
      for(j in 1:lng){
        omitted[k] <- as.character(as.Date((as.numeric(as.Date(dates[i],'%Y.%m.%d'))+j),origin='1970-1-1'))
        k <- k + 1
      }
    } 
  }
  # omitted <- omitted[-which(omitted=="2016-12-31")]
  print(omitted)
  
  nrun <- nrun+1
  print(nrun)
  
  if(nrun > 10) break
}


