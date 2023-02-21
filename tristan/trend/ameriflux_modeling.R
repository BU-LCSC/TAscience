library(easypackages)
libraries('gam', 'ggplot2', 'ggpmisc', 'visreg', 'ggpubr', 'dplyr', 'plyr')

gppsite <- readRDS('/projectnb/modislc/users/twgreen/MODISProject/RData/Ameriflux_Site/DBF_MF/sitegpppheno_v2.rds')
dayMsite <- readRDS('/projectnb/modislc/users/twgreen/MODISProject/RData/Ameriflux_Site/DBF_MF/sitedaym.rds')
modissite <- readRDS('/projectnb/modislc/users/twgreen/MODISProject/RData/Ameriflux_Site/DBF_MF/sitemodis.rds')

dayMsite <- ldply(dayMsite, data.frame)
dayMsite[dayMsite == 'NaN'] <- NA

colnames(dayMsite) <- c('sitenm','Year','DayMTair','DayMSWin','DayMVPD','DayMcta')
colnames(modissite) <- c('sitenm','Year','GUP','GDW','GSL','GUP15','GDW15',
                         'EVIAmp','EVIMax','EVIArea','Peak')
dfle <- merge(gppsite, dayMsite, by=c('sitenm','Year'))
dfpr <- merge(dfle, modissite, by=c('sitenm','Year'))

dfpra$GPP_GSL[dfpra$GPP_GSL < 10] <- NA
dfpra$GPP_GSL[dfpra$GPP_GSL > 320] <- NA
dfpr$GPP[dfpr$GPP > 2750] <- NA
dfpra <- na.omit(dfpra)
dfpra <- dfpr[-c(67:82),]

View(dfpr)

lm_modMF <- gam(GPP_GSL ~ s(GUP) + s(GDW) + s(GUP15) + s(GDW15) + s(Peak) + 
                  s(EVIArea) + s(EVIAmp) + s(DayMVPD) + s(DayMcta), data=dfpra)

lm_modMFa <- gam(GPP_Max ~ s(GSL) + s(GDW) + s(GUP) + s(GDW15)  + 
                   s(EVIAmp) + s(EVIMax) + s(DayMTair) +
                   s(DayMSWin) + s(DayMcta), data=dfpra)

dfpra$GPP_GSLp <- predict(lm_modMF)
dfpra$GPP_Maxp <- predict(lm_modMFa)

lm_modMFb <- gam(GPP ~ s(GPP_GSLp) + s(GPP_Maxp) + s(DayMSWin) + s(DayMVPD), data=dfpra)

dfpra$GPPp <- predict(lm_modMFb)


pdf('/projectnb/modislc/users/twgreen/MODISProject/Figures/Spatial/AmfGamsMF.pdf')
ggplot(dfpra, aes(x=GPP_GSL, y=GPP_GSLp)) + geom_point()  + stat_ma_line(method='SMA') + geom_abline(slope=1, intercept=0) +
  stat_ma_eq(use_label(c('eq','R2','P')), method='SMA') + xlab('Site GPP GSL (DoY)') + ylab('DayMet GPP GSL (DoY)') + theme_classic() 

ggplot(dfpra, aes(x=GPP_Max, y=GPP_Maxp)) + geom_point()  + stat_ma_line(method='SMA') + geom_abline(slope=1, intercept=0) +
  stat_ma_eq(use_label(c('eq','R2','P')), method='SMA') + xlab('Site GPP Max (gC m-2)') + ylab('DayMet GPP Max (gC m-2)') + theme_classic() 

ggplot(dfpra, aes(x=GPP, y=GPPp)) + geom_point()  + stat_ma_line(method = 'SMA') + geom_abline(slope=1, intercept=0) +
  stat_ma_eq(use_label(c('eq','R2','P')), method='SMA') + xlim(100,2500) + ylim(100,2500)+ xlab('Site Annual GPP (gC m-2 y-1)') + ylab('DayMet Annual GPP (gC m-2 y-1)') + theme_classic() 
dev.off()

comb <- merge(dfpr, prel, by=c('sitenm','Year'))

ggplot(comb, aes(x=GPP_GSLp.x, y=GPP_GSLp.y)) + geom_point()  + stat_poly_line() + geom_abline(slope=1, intercept=0) +
  stat_poly_eq()+ xlab('Site GPP GSL (DoY)') + ylab('DayMet GPP GSL (DoY)') + theme_classic() 

ggplot(comb, aes(x=GPP_Max., y=GPP_Maxp.y)) + geom_point()  + stat_poly_line() + geom_abline(slope=1, intercept=0) +
  stat_poly_eq()+ xlab('Site GPP Max (gC m-2)') + ylab('DayMet GPP Max (gC m-2)') + theme_classic() 

ggplot(comb, aes(x=GPP.y, y=GPPp.y)) + geom_point()  + stat_poly_line() + geom_abline(slope=1, intercept=0) +
  stat_poly_eq()+ xlim(100,2500) + ylim(100,2500)+ xlab('GAMS old model GPP (gC m-2 y-1)') + ylab('GAMS New Model GPP (gC m-2 y-1)') + theme_classic() 

prel$sitenm[prel$sitenm == 'AMF_US-Ha1'] <- 'FLX_US-Ha1'
prel$sitenm[prel$sitenm == 'AMF_US-UMd'] <- 'FLX_US-UMd'
prel$sitenm[prel$sitenm == 'AMF_US-MMS'] <- 'FLX_US-MMS'


saveRDS(dfprel, '/projectnb/modislc/users/twgreen/MODISProject/RData/spatial/prel.rds')


climdf <- dfpra %>%
  group_by(sitenm) %>%
  summarise_at(c('GPP', 'GPP_Max', 'GPP_GSL', 'DayMTair','DayMSWin','DayMVPD',
                 'GUP','GDW','GSL','GUP15','GDW15','EVIAmp','EVIMax','EVIArea','Peak'), mean, na.rm=TRUE)

colnames(climdf) <- c('sitenm', 'Clim_GPP', 'Clim_Max', 'Clim_GSL','Clim_Tair','Clim_SWin','Clim_VPD',
                      'Clim_GUP','Clim_GDW','Clim_GSLa','Clim_GUP15','Clim_GDW15','Clim_EVIAmp','Clim_EVIMax',
                      'Clim_EVIArea','Clim_Peak')
filtenvdfa <- merge(dfpra, climdf, by='sitenm')

filtenvdfa$GPP_an <- filtenvdfa$GPP - filtenvdfa$Clim_GPP
filtenvdfa$Max_an <- filtenvdfa$GPP_Max - filtenvdfa$Clim_Max
filtenvdfa$GSL_an <- filtenvdfa$GPP_GSL - filtenvdfa$Clim_GSL
filtenvdfa$Tair_an <- filtenvdfa$DayMTair - filtenvdfa$Clim_Tair
filtenvdfa$Swin_an <- filtenvdfa$DayMSWin - filtenvdfa$Clim_SWin
filtenvdfa$VPD_an <- filtenvdfa$DayMVPD - filtenvdfa$Clim_VPD
filtenvdfa$GUP_an <- filtenvdfa$GUP - filtenvdfa$Clim_GUP
filtenvdfa$GDW_an <- filtenvdfa$GDW - filtenvdfa$Clim_GDW
filtenvdfa$GSL_ana <- filtenvdfa$GSL - filtenvdfa$Clim_GSLa
filtenvdfa$GUP15_an <- filtenvdfa$GUP15 - filtenvdfa$Clim_GUP15
filtenvdfa$GDW15_an <- filtenvdfa$GDW15 - filtenvdfa$Clim_GDW15
filtenvdfa$EVIMax_an <- filtenvdfa$EVIMax - filtenvdfa$Clim_EVIMax
filtenvdfa$EVIAmp_an <- filtenvdfa$EVIAmp - filtenvdfa$Clim_EVIAmp
filtenvdfa$EVIArea_an <- filtenvdfa$EVIArea - filtenvdfa$Clim_EVIArea
filtenvdfa$Peak_an <- filtenvdfa$Peak - filtenvdfa$Clim_Peak

lm_modMF <- gam(GSL_an ~ s(GUP_an) + s(GDW_an) + s(GUP15_an) + s(GDW15_an) + s(Peak_an) + 
                  s(EVIArea_an) + s(EVIAmp_an) + s(VPD_an) + s(DayMcta), data=filtenvdfa)

lm_modMFa <- gam(Max_an ~ s(GSL_ana) + s(GDW_an) + s(GUP_an) + s(GDW15_an)  + 
                   s(EVIAmp_an) + s(EVIMax_an) + s(Tair_an) +
                   s(Swin_an) + s(DayMcta), data=filtenvdfa)

filtenvdfa$GPP_GSLanp <- predict(lm_modMF)
filtenvdfa$GPP_Maxanp <- predict(lm_modMFa)

lm_modMFb <- gam(GPP_an ~ s(GPP_GSLanp) + s(GPP_Maxanp) + s(Swin_an) + s(VPD_an), data=filtenvdfa)
filtenvdfa$GPP_anp <- predict(lm_modMFb)

pdf('/projectnb/modislc/users/twgreen/MODISProject/Figures/Spatial/amerifluxanomal.pdf')
ggplot(filtenvdfa, aes(x=GSL_an, y=GPP_GSLanp)) + geom_point()  + stat_poly_line() + geom_abline(slope=1, intercept=0) +
  stat_poly_eq()+ xlab('Site GPP GSL Anomaly') + ylab('Model GPP GSL Anomaly') + xlim(-50,50) + ylim(-50,50) + theme_classic() 

ggplot(filtenvdfa, aes(x=Max_an, y=GPP_Maxanp)) + geom_point()  + stat_poly_line() + geom_abline(slope=1, intercept=0) +
  stat_poly_eq()+ xlab('Site GPP Max Anomaly') + ylab('Model GPP Max Anomaly') + xlim(-7,7) + ylim(-7,7) + theme_classic() 

ggplot(filtenvdfa, aes(x=GPP_an, y=GPP_anp)) + geom_point()  + stat_poly_line() + geom_abline(slope=1, intercept=0) +
  stat_poly_eq()+ xlab('Site Annual GPP Model Anomaly') + ylab('Model Annual GPP Anomaly') + xlim(-700,700) + ylim(-700,700) + theme_classic() 
dev.off()




