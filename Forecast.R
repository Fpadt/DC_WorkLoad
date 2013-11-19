library(fpp)

voice <- read.table("E:/DC_WorkLoad/VOICE.csv", sep=";", quote="\"")

names(voice) <- c("DATE","Q")
voice.ts     <- ts(voice[, -1], start = c(2013,1,2), frequency = 365)

plot(voice.ts)

# https://www.otexts.org/fpp/8/1
# The null-hypothesis for an ADF test is that the data are non-stationary. 
# So large p-values are indicative of non-stationarity, and small p-values  
# suggest stationarity. Using the usual 5% threshold, differencing is required  
# if the p-value is greater than 0.05.

# Another popular unit root test is the Kwiatkowski-Phillips-Schmidt-Shin 
# (KPSS) test. This reverses the hypotheses, so the null-hypothesis is that 
# the data are stationary. In this case, small p-values (e.g., less than 0.05)
# suggest that differencing is required.

# https://www.otexts.org/fpp/2/6
# A test for a group of autocorrelations is called a portmanteau test, 
# from a French word describing a suitcase containing a number of items.
# A related (and more accurate) test is the Ljung-Box test 
# the p-values are relatively large). So we can conclude that the residuals 
# are not distinguishable from a white noise series.
# If they are calculated from raw data (rather than the residuals from a model),
# then set K=0.

tsdisplay(voice.ts)
adf.test(voice.ts, alternative = "stationary")            # p-value = 0.01613
Box.test(voice.ts, type="Ljung-Box", fitdf = 0)           # p-value = 1.345e-07
kpss.test(voice.ts)                                       # p-value = 0.01713 -> diffferencing required
ndiffs(voice.ts)                                          # 1
nsdiffs(voice.ts) # Error Time series too short for seasonal differencing

tsdisplay(diff(voice.ts, lag=1, differences=1))
adf.test(voice.ts, alternative = "stationary")            # p-value = 0.01613
Box.test(voice.ts, type="Ljung-Box", fitdf = 0)           # p-value = 1.345e-07
kpss.test(voice.ts)                                       # p-value = 0.01713 -> diffferencing required
ndiffs(voice.ts)                                          # 1
nsdiffs(voice.ts) # Error Time series too short for seasonal differencing

fit <- auto.arima(voice.ts, seasonal = FALSE)
plot(forecast(fit, 10))
# Series: voice.ts 
# ARIMA(5,1,5)                    
# 
# Coefficients:
#   ar1      ar2      ar3      ar4     ar5      ma1      ma2     ma3     ma4      ma5
# -0.4700  -0.3628  -0.4350  -0.3603  0.5237  -0.0382  -0.3068  0.0550  0.0395  -0.6206
# s.e.   0.1092   0.1081   0.1195   0.1083  0.0821   0.1036   0.0921  0.0901  0.0780   0.1186
# 
# sigma^2 estimated as 208595636:  log likelihood=-2410.69
# AIC=4843.39   AICc=4844.66   BIC=4880.67

