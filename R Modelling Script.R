#Gathering all the data

install.packages('haven')
library(haven)
All_Munis <- read_dta("C:/Users/AYSPS/Dropbox (GSU Research)/High Yield Muni Research/Data & R-Code/All Munis.dta")
High_Yield_Muni_monthly <- read_dta("C:/Users/AYSPS/Dropbox (GSU Research)/High Yield Muni Research/Data & R-Code/High Yield Muni monthly.dta")
High_Yield_Muni_ex_PR_monthly <- read_dta("C:/Users/AYSPS/Dropbox (GSU Research)/High Yield Muni Research/Data & R-Code/High Yield Muni ex PR monthly.dta")

#Preparing different time-lines of data
oldmunisdata <- All_Munis[25:240,] #1982-1999
newmunisdata <- All_Munis[241:456,] #2000-2017
newspmunisdata <- All_Munis[361:456,] #2010-2017
oldhymunisdata <- High_Yield_Muni_monthly[2:133,] #1996-2006
newhymunisdata <- High_Yield_Muni_monthly[134:265,] #2007-2017
newhymunisdatatwothousand <- High_Yield_Muni_monthly[50:265,] #2000-2017
hymunisexpr <- High_Yield_Muni_ex_PR_monthly[2:97,] #2010-2017

install.packages('Ecdat')
install.packages('fGarch')
install.packages('evir')
install.packages('forecast')
install.packages('lmtest')
library("Ecdat")
library("fGarch")
library("evir")
library("forecast")

#Setting up time-series
ts_oldmunisdata_mreturn = ts(oldmunisdata$mreturn, frequency = 12)
ts_newmunisdata_mreturn = ts(newmunisdata$mreturn, frequency = 12)
ts_newspmunisdata_mreturn = ts(newspmunisdata$mreturn, frequency = 12)
ts_oldhymunisdata_mreturn = ts(oldhymunisdata$mreturn, frequency = 12)
ts_newhymunisdata_mreturn = ts(newhymunisdata$mreturn, frequency = 12)
ts_newhymunisdatatwothousand_mreturn = ts(newhymunisdatatwothousand$mreturn, frequency = 12)
ts_hymunisexpr_mreturn = ts(hymunisexpr$mreturn, frequency = 12)

#Regression with ARIMA errors

#Made Changes
fitoldmunisdata_dynamic=auto.arima(ts_oldmunisdata_mreturn, xreg = as.matrix(oldmunisdata[, c(10:20)]), stepwise = FALSE, approx = FALSE)
fitoldmunisdata_dynamic_2 = arima(ts_oldmunisdata_mreturn, xreg = as.matrix(oldmunisdata[, c(10:20)]), order = c(6,1,1), seasonal = list(order = c(0,0,0), period = 12))

#Made Changes
fitoldhymunisdata_dynamic=auto.arima(ts_oldhymunisdata_mreturn, xreg = as.matrix(oldhymunisdata[, c(10:20)]), stepwise = FALSE, approx = FALSE)
fitoldhymunisdata_dynamic_2 = arima(ts_oldhymunisdata_mreturn, xreg = as.matrix(oldhymunisdata[, c(10:20)]), order = c(5,0,1), seasonal = list(order = c(0,0,1), period = 12))

#Best possible model
fitnewmunisdata_dynamic=auto.arima(ts_newmunisdata_mreturn, xreg = as.matrix(newmunisdata[, c(10:20, 22)]))
fitnewhymunisdata_dynamic=auto.arima(ts_newhymunisdata_mreturn, xreg = as.matrix(newhymunisdata[, c(10:20, 22)]))
fitnewhymunisdatatwothousand_dynamic=auto.arima(ts_newhymunisdatatwothousand_mreturn, xreg = as.matrix(newhymunisdatatwothousand[, c(10:20, 22)]))
fitnewspmunisdata_dynamic=auto.arima(ts_newspmunisdata_mreturn, xreg = as.matrix(newspmunisdata[, c(10:20, 22)]))

#Removed shortcut in auto.arima estimation process to fit a better model
fithymunisexpr_dynamic=auto.arima(ts_hymunisexpr_mreturn, xreg = as.matrix(hymunisexpr[, c(10:20, 22)]), stepwise = FALSE, approx = FALSE)

#Printing all models
print(fitoldmunisdata_dynamic_2)
print(fitnewmunisdata_dynamic)
print(fitoldhymunisdata_dynamic_2)
print(fitnewhymunisdata_dynamic)
print(fitnewhymunisdatatwothousand_dynamic)
print(fitnewspmunisdata_dynamic)
print(fithymunisexpr_dynamic)

library("lmtest")

#Is there a decline in the January effect? 

write.csv(coeftest(fitoldmunisdata_dynamic_2)) #1982-1999
write.csv(coeftest(fitnewmunisdata_dynamic)) #2000-2017

#Yes, the coefficients on the second model are smaller. 

write.csv(coeftest(fitoldhymunisdata_dynamic_2)) #1996-2006
write.csv(coeftest(fitnewhymunisdata_dynamic)) #2007-2017

#No, the coefficients on the second model are larger.

#January effect has declined for relatively safe bonds, but has increased for high yield debt.

#Residual Tests
Box.test(fitoldmunisdata_dynamic_2$residuals,lag=24,type="Ljung-Box")
Box.test(fitnewmunisdata_dynamic$residuals,lag=24,type="Ljung-Box")
Box.test(fitoldhymunisdata_dynamic_2$residuals,lag=24,type="Ljung-Box")
Box.test(fitnewhymunisdata_dynamic$residuals,lag=24,type="Ljung-Box")

#Does the January effect change with credit quality? 

write.csv(coeftest(fitnewmunisdata_dynamic)) #2000-2017
write.csv(coeftest(fitnewhymunisdatatwothousand_dynamic)) #2000-2017

#Yes, the coefficients on the second model are larger. 

write.csv(coeftest(fitnewspmunisdata_dynamic)) #2010-2017
write.csv(coeftest(fithymunisexpr_dynamic)) #2010-2017

#Yes, again. 

#Results indicate that january effect increases as credit quality declines. 

#Residual Tests
Box.test(fitnewmunisdata_dynamic$residuals,lag=24,type="Ljung-Box")
Box.test(fitnewhymunisdatatwothousand_dynamic$residuals,lag=24,type="Ljung-Box")
Box.test(fitnewspmunisdata_dynamic$residuals,lag=24,type="Ljung-Box")
Box.test(fithymunisexpr_dynamic$residuals, lag=24,type="Ljung-Box")