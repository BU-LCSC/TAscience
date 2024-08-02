library(remotePARTS)
library(dplyr)
library(ggplot2)

data("ndvi_AK10000")
ndvi_AK3000 <- ndvi_AK10000[seq_len(3000),] # first 3000 pixels from the random 10K

reshape2::melt(ndvi_AK10000, measure = c("ndvi1982", "ndvi1998", "ndvi2013")) %>% 
  ggplot(aes(x = lng, y = lat, col = value )) + 
  geom_point(size = .1) +
  labs(col = "ndvi") +
  facet_wrap(~ gsub("ndvi", "", variable), ncol = 3) +
  scale_color_viridis_c(option = "magma") +
  labs(x = "Longitude", y = "Latitude")


ndvi_AK10000 %>%  
  ggplot(aes(x = lng, y = lat, col = land)) + 
  geom_point(size = .1) + 
  scale_color_viridis_d(direction = -1, end = .9) +
  labs(y = "Latitude", x = "Longitude", col = "Land cover", fill = "Land cover")


ndvi.cols <- grep("ndvi", names(ndvi_AK3000), value = TRUE)
Y <- as.matrix(ndvi_AK3000[, ndvi.cols])
coords <- as.matrix(ndvi_AK3000[, c("lng", "lat")])
ARfit <- fitAR_map(Y = Y, coords = coords)
head(coefficients(ARfit))

ARfit$coefficients[, "t"] <- ARfit$coefficients[,"t"]/rowMeans(ndvi_AK3000[, ndvi.cols])
ndvi_AK3000$AR_coef <- coefficients(ARfit)[, "t"] # save time trend coefficient

ndvi_AK10000 %>% 
  ggplot(aes(x = lng, y = lat, col = AR_coef)) + 
  geom_point(size = .1) + 
  scale_color_gradient2(high = "red", low = "blue", 
                        mid = "grey90", midpoint = 0) + 
  guides(fill = "none") + 
  labs(y = "Latitude", x = "Longitude", col = expression(beta[1]))


D <- distm_scaled(coords)
r <- 0.1
V <- covar_exp(D, r)
nugget <- 0.2 
I <- diag(nrow(V)) # identity matrix
Sigma <- nugget*I + (1-nugget)*V

GLS.0 <- fitGLS(formula = AR_coef ~ land, data = ndvi_AK3000, V = V, nugget = nugget)
fitGLS(formula = AR_coef ~ 0 + land, data = ndvi_AK3000, V = Sigma, nugget = 0) # equivalent
coefficients(GLS.0)

corfit <- fitCor(resids = residuals(ARfit), coords = coords, covar_FUN = "covar_exp", 
                 start = list(range = 0.1), fit.n = 3000)
range.opt = corfit$spcor
V.opt <- covar_exp(D, range.opt)

GLS.opt <- fitGLS(formula = AR_coef ~ 0 + land, data = ndvi_AK3000, V = V.opt, nugget = NA, no.F = FALSE)
nug.opt = GLS.opt$nugget

rbind(GLS.0 = c(range = r, nugget = GLS.0$nugget, logLik = GLS.0$logLik, MSE = GLS.0$MSE),
      GLS.opt = c(range = range.opt, nugget = GLS.opt$nugget, logLik = GLS.opt$logLik, MSE = GLS.opt$MSE))

# fitopt <- fitGLS_opt(formula = AR_coef ~ 0 + land, data = ndvi_AK3000, 
#                      coords = ndvi_AK3000[, c("lng", "lat")], 
#                      covar_FUN = "covar_exp", 
#                      start = c(range = .1, nugget = .2),
#                      method = "BFGS", # use BFGS algorightm (see ?stats::optim())
#                      control = list(reltol = 1e-5)  # lower the convergence tolerance (see ?stats::optim()) 

GLS.int <- fitGLS(AR_coef ~ 1, data = ndvi_AK3000, V = V.opt, nugget = nug.opt, no.F = TRUE)
GLS.lat <- fitGLS(AR_coef ~ 1 + lat, data = ndvi_AK3000, V = V.opt, nugget = nug.opt, no.F = FALSE)


