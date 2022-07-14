##Daily
## PART 1 ##
library(readxl)
library(quantmod)

## Data Collection ##
NSE <- getSymbols.yahoo("^NSEI", from = "2020-04-01", to = "2022-03-31", verbose = F, auto.assign = F)
NSE <- na.omit(NSE)

BATAINDIA <- getSymbols.yahoo("BATAINDIA.NS", from = "2020-04-01", to = "2022-03-31", verbose = F, auto.assign = F)
BATAINDIA <- na.omit(BATAINDIA)

BERGEPAINT <- getSymbols.yahoo("BERGEPAINT.NS", from = "2020-04-01", to = "2022-03-31", verbose = F, auto.assign = F)
BERGEPAINT <- na.omit(BERGEPAINT)
T_Bill_D <- read_excel("Tbills_daily.xlsx")
T_Bill_D <- as.data.frame(T_Bill_D)
T_Billxts <- xts(T_Bill_D[,-1], order.by = as.Date(T_Bill_D$Date))

## Return Calculation ##
Excess_Return_NSE <-( (dailyReturn(NSE$NSEI.Close)[-1, ] -T_Billxts[-1, ]))
head(Excess_Return_NSE)
Excess_Return_BATAINDIA <-( (dailyReturn(BATAINDIA$BATAINDIA.NS.Close)[-1, ]-T_Billxts[-1, ]))
Excess_Return_BERGEPAINT <-( (dailyReturn(BERGEPAINT$BERGEPAINT.NS.Close)[-1, ]-T_Billxts[-1, ]))
returns1 <- cbind(Excess_Return_NSE,Excess_Return_BATAINDIA)
returns2 <- cbind(Excess_Return_NSE,Excess_Return_BERGEPAINT)
names(returns1) <- c("EXCESS_NSE","EXCESS_BATAINDIA")
head(returns1)
names(returns2)<- c("EXCESS_NSE","EXCESS_BERGEPAINT")
head(returns2)
## Regression Model ##
regression1 <- lm(returns1$EXCESS_BATAINDIA~returns1$EXCESS_NSE)
summary(regression1)

regression2 <- lm(returns2$EXCESS_BERGEPAINT~returns2$EXCESS_NSE)
summary(regression2)


## PART 2 ##

library(tseries)

## Return Calculation ##
returns_bataindia <- as.xts(tail(data.frame(BATAINDIA$BATAINDIA.NS.Close),-1)/head(data.frame(BATAINDIA$BATAINDIA.NS.Close),-1)-1, frequency = 365)
returns_bataindia <- na.omit(returns_bataindia)
returns_bergepaint <- as.xts(tail(data.frame(BERGEPAINT$BERGEPAINT.NS.Close),-1)/head(data.frame(BERGEPAINT$BERGEPAINT.NS.Close),-1)-1, frequency = 365)
returns_bergepaint <- na.omit(returns_bergepaint)

## Data Manipulation ##
colnames(returns_bataindia) <- "returns_bataindia"
colnames(returns_bergepaint) <- "returns_bergepaint"
mean(returns_bataindia)
var(returns_bataindia)

## Data Visualization ##
plot(BATAINDIA$BATAINDIA.NS.Close)
plot(returns_bataindia)

plot(BERGEPAINT$BERGEPAINT.NS.Close)
plot(returns_bergepaint)

## model identification AR & MA ##
adf.test(returns_bataindia, alternative = "stationary")
plot(acf( returns_bataindia, lag.max = 10))
plot(pacf(returns_bataindia , lag.max = 10))
arima_final1 <- arima(returns_bataindia, order= c(0,0,0))
arima_final1
predicted <- predict(arima_final1, n.ahead = 10)
predicted
tsdiag(arima_final1)

adf.test(returns_bergepaint, alternative = "stationary")
plot(acf(returns_bergepaint , lag.max = 10))
plot(pacf(returns_bergepaint , lag.max = 10))
arima_final2 <- arima(returns_bergepaint, order= c(0,0,0))
arima_final2
predicted <- predict(arima_final2, n.ahead = 10)
predicted
tsdiag(arima_final2)

## PART 3 ##

library(quantmod)
library(rugarch)
library(rmgarch)

## Data Collection ##
bataindia1 <- getSymbols("BATAINDIA.NS", from = "2020-04-01", to = "2022-03-31")
bataindia1 <- na.omit(bataindia1)

bergepaint1 <- getSymbols("BERGEPAINT.NS", from = "2020-04-01", to = "2022-03-31")
bergepaint1<- na.omit(bergepaint1)

## Return Calculation ##
R.bataindia <- dailyReturn(BATAINDIA.NS)
R.bergepaint <- dailyReturn(BERGEPAINT.NS)

## Implementing Univariate GARCH ##
ug_spec = ugarchspec()
ug_spec

## Implementing EGARCH ##
eg_spec = ugarchspec(variance.model = list(model="eGARCH"))
eg_spec

#Estimating the models
ugfit1 = ugarchfit(spec = ug_spec, data =R.bataindia ) 
ugfit1

ugfit2 = ugarchfit(spec = ug_spec, data = R.bergepaint) 
ugfit2

#Forecasting
ugforecast1 = ugarchforecast(ugfit1, n.ahead=10) 
ugforecast1

ugforecast2 = ugarchforecast(ugfit2, n.ahead=10) 
ugforecast2

#Weekly
## PART 1 ##
library(readxl)
library(quantmod)

## Data Collection ##
NSE <- getSymbols.yahoo("^NSEI", from = "2020-04-01", to = "2022-03-31", verbose = F, auto.assign = F, periodicity = "weekly")
NSE <- na.omit(NSE)
View(NSE)
bataindia <- getSymbols.yahoo("BATAINDIA.NS", from = "2020-04-01", to = "2022-03-31", verbose = F, auto.assign = F, periodicity = "weekly")
bataindia <- na.omit(bataindia)

bergepaint <- getSymbols.yahoo("BERGEPAINT.NS", from = "2020-04-01", to = "2022-03-31", verbose = F, auto.assign = F, periodicity = "weekly")
bergepaint <- na.omit(bergepaint)


## Data Manipulation ##
T_Bill_w <- read_excel("Tbills_weekly.xlsx")
T_Bill_w <- as.data.frame(T_Bill_w)
View(T_Bill_w)
T_Billxts_w <- xts(T_Bill_w[,-1], order.by = as.Date(T_Bill_w$Date))

## Return Calculation ##
Excess_Return_NSE_W <-(as.data.frame( (dailyReturn(NSE$NSEI.Close)[-1, ]) -T_Bill_w$`T-Bill%`))
head(Excess_Return_NSE_W)
Excess_Return_BATAINDIA_W <-( (dailyReturn(bataindia$BATAINDIA.NS.Close)[-1, ]-T_Bill_w$`T-Bill%`))
Excess_Return_BERGEPAINT_W <-( (dailyReturn(bergepaint$BERGEPAINT.NS.Close)[-1, ]-T_Bill_w$`T-Bill%`))
returns1_W <- cbind(Excess_Return_NSE_W,Excess_Return_BATAINDIA_W)
returns2_W <- cbind(Excess_Return_NSE_W,Excess_Return_BERGEPAINT_W)
names(returns1_W) <- c("EXCESS_NSE_W","EXCESS_BATAINDIA_W")
head(returns1_W)
names(returns2_W)<- c("EXCESS_NSE_W","EXCESS_BERGEPAINT_W")

## Regression Model ##
regression1_W <- lm(returns1_W$EXCESS_BATAINDIA_W~returns1_W$EXCESS_NSE_W)
summary(regression1_W)

regression2_W <- lm(returns2_W$EXCESS_BERGEPAINT_W~returns2_W$EXCESS_NSE_W)
summary(regression2_W)

## PART 2 ##

library(tseries)

## Return Calculation ##
returns_bataindia<- as.xts(tail(data.frame(bataindia$BATAINDIA.NS.Close),-1)/head(data.frame(bataindia$BATAINDIA.NS.Close),-1)-1, frequency = 365)
returns_bataindia <- na.omit(returns_bataindia)
returns_bergepaint <- as.xts(tail(data.frame(bergepaint$BERGEPAINT.NS.Close),-1)/head(data.frame(bergepaint$BERGEPAINT.NS.Close),-1)-1, frequency = 365)
returns_bergepaint <- na.omit(returns_bergepaint)

## Data Manipulation ##
colnames(returns_bataindia) <- "returns_bataindia"
colnames(returns_bergepaint) <- "returns_bergepaint"

## Data Visualization ##
plot(bataindia$BATAINDIA.NS.Close)
plot(returns_bataindia)

plot(bergepaint$BERGEPAINT.NS.Close)
plot(returns_bergepaint)

## model identification AR & MA ##
adf.test(returns_bataindia, alternative = "stationary")
plot(acf(returns_bataindia , lag.max = 10))
plot(pacf(returns_bataindia , lag.max = 10))
arima_final1 <- arima(returns_bataindia, order= c(0,0,0))
arima_final1
predicted <- predict(arima_final1, n.ahead = 10)
predicted
tsdiag(arima_final1)

adf.test(returns_bergepaint, alternative = "stationary")
plot(acf(returns_bergepaint , lag.max = 10))
plot(pacf(returns_bergepaint , lag.max = 10))
arima_final2 <- arima(returns_bergepaint, order= c(0,0,3))
arima_final2
predicted <- predict(arima_final2, n.ahead = 10)
predicted
tsdiag(arima_final2)

## PART 3 ##

library(quantmod)
library(rugarch)
library(rmgarch)

## Data Collection ##
bataindia1 <- getSymbols("BATAINDIA.NS", from = "2020-04-01", to = "2022-03-31", periodicity = "weekly")
bataindia1 <- na.omit(bataindia1)

bergepaint1 <- getSymbols("BERGEPAINT.NS", from = "2020-04-01", to = "2022-03-31",periodicity = "weekly" )
bergepaint1<- na.omit(bergepaint1)

## Return Calculation ##
R.bataindia <- dailyReturn(BATAINDIA.NS)
R.bergepaint <- dailyReturn(BERGEPAINT.NS)

## Implementing Univariate GARCH ##
ug_spec = ugarchspec()
ug_spec

## Implementing EGARCH ##
eg_spec = ugarchspec(variance.model = list(model="eGARCH"))
eg_spec

#Estimating the models
ugfit1 = ugarchfit(spec = ug_spec, data = R.bataindia) 
ugfit1

ugfit2 = ugarchfit(spec = ug_spec, data =R.bergepaint) 
ugfit2

#Forecasting
ugforecast1 = ugarchforecast(ugfit1, n.ahead=10) 
ugforecast1

ugforecast2 = ugarchforecast(ugfit2, n.ahead=10) 
ugforecast2

#Monthly 
## PART 1 ##
library(readxl)
library(quantmod)

## Data Collection ##
NSE <- getSymbols.yahoo("^NSEI", from = "2020-04-01", to = "2022-03-31", verbose = F, auto.assign = F, periodicity = "monthly")
NSE <- na.omit(NSE)
bataindia <- getSymbols.yahoo("BATAINDIA.NS", from = "2020-04-01", to = "2022-03-31", verbose = F, auto.assign = F, periodicity = "monthly")
bataindia<- na.omit(bataindia)

bergepaint <- getSymbols.yahoo("BERGEPAINT.NS", from = "2020-04-01", to = "2022-03-31", verbose = F, auto.assign = F, periodicity = "monthly")
bergepaint <- na.omit(bergepaint)


## Data Manipulation ##
T_Bill_M <- read_excel("Tbills_monthly.xlsx")
T_Bill_M <- as.data.frame(T_Bill_M)
View(T_Bill_M)
T_Billxts_M <- xts(T_Bill_M[,-1], order.by = as.Date(T_Bill_M$Date))
length(dailyReturn(NSE$NSEI.Close))
dailyReturn(NSE$NSEI.Close)
## Return Calculation ##
Excess_Return_NSE_M <-(as.data.frame( (dailyReturn(NSE$NSEI.Close)) -T_Bill_M$`T-Bill% (Monthly)`))
head(Excess_Return_NSE_M)
Excess_Return_BATAINDIA_M <-( (dailyReturn(bataindia$BATAINDIA.NS.Close)-T_Bill_M$`T-Bill% (Monthly)`))
Excess_Return_BERGEPAINT_M <-( (dailyReturn(bergepaint$BERGEPAINT.NS.Close)-T_Bill_M$`T-Bill% (Monthly)`))
returns1_M <- cbind(Excess_Return_NSE_M[-1, ],Excess_Return_BATAINDIA_M[-1, ])
returns2_M <- cbind(Excess_Return_NSE_M[-1, ],Excess_Return_BERGEPAINT_M[-1, ])
names(returns1_M) <- c("EXCESS_NSE_M","EXCESS_BATAINDIA_M")
head(returns1_M)
names(returns2_M)<- c("EXCESS_NSE_M","EXCESS_BERGEPAINT_M")

## Regression Model ##
regression1_M <- lm(returns1_M$EXCESS_BATAINDIA_M~returns1_M$EXCESS_NSE_M)
summary(regression1_M)


regression2_M <- lm(returns2_M$EXCESS_BERGEPAINT_M~returns2_M$EXCESS_NSE_M)
summary(regression2)

## PART 2 ##

library(tseries)

## Return Calculation ##
returns_bataindia <- as.xts(tail(data.frame(bataindia$BATAINDIA.NS.Close),-1)/head(data.frame(bataindia$BATAINDIA.NS.Close),-1)-1, frequency = 365)
returns_bataindia <- na.omit(returns_bataindia)
returns_bergepaint <- as.xts(tail(data.frame(bergepaint$BERGEPAINT.NS.Close),-1)/head(data.frame(bergepaint$BERGEPAINT.NS.Close),-1)-1, frequency = 365)
returns_bergepaint <- na.omit(returns_bergepaint)

## Data Manipulation ##
colnames(returns_bataindia) <- "returns_bataindia"
colnames(returns_bergepaint) <- "returns_bergepaint"

## Data Visualization ##
plot(bataindia$BATAINDIA.NS.Close)
plot(returns_bataindia)

plot(bergepaint$BERGEPAINT.NS.Close)
plot(returns_bergepaint)

## model identification AR & MA ##
adf.test(returns_bataindia, alternative = "stationary")
plot(acf(returns_bataindia , lag.max = 10))
plot(pacf(returns_bataindia , lag.max = 10))
arima_final1 <- arima(returns_bataindia, order= c(0,0,0))
arima_final1
predicted <- predict(arima_final1, n.ahead = 10)
predicted
tsdiag(arima_final1)

adf.test(returns_bergepaint, alternative = "stationary")
plot(acf(returns_bergepaint , lag.max = 10))
plot(pacf(returns_bergepaint , lag.max = 10))
arima_final2 <- arima(returns_bergepaint, order= c(0,0,2))
arima_final2
predicted <- predict(arima_final2, n.ahead = 10)
predicted
tsdiag(arima_final2)

## PART 3 ##

library(quantmod)
library(rugarch)
library(rmgarch)

## Data Collection ##
bataindia1 <- getSymbols("BATAINDIA.NS", from = "2020-04-01", to = "2022-03-31", periodicity = "monthly")
bataindia1 <- na.omit(bataindia1)

bergepaint1 <- getSymbols("BERGEPAINT.NS", from = "2020-04-01", to = "2022-03-31",periodicity = "monthly" )
bergepaint1<- na.omit(bergepaint1)

## Return Calculation ##
R.bataindia <- dailyReturn(BATAINDIA.NS)
R.bergepaint <- dailyReturn(BERGEPAINT.NS)

## Implementing Univariate GARCH ##
ug_spec = ugarchspec()
ug_spec

## Implementing EGARCH ##
eg_spec = ugarchspec(variance.model = list(model="eGARCH"))
eg_spec

#Estimating the models
ugfit1 = ugarchfit(spec = ug_spec, data = R.bataindia) 
ugfit1

ugfit2 = ugarchfit(spec = ug_spec, data = R.bergepaint) 
ugfit2

#Forecasting
ugforecast1 = ugarchforecast(ugfit1, n.ahead=10) 
ugforecast1

ugforecast2 = ugarchforecast(ugfit2, n.ahead=10) 
ugforecast2

