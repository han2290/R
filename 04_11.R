install.packages("zoo")
library("zoo")

?decompose
require(graphics)
m <- decompose(co2)
m$figure
plot(m)

library(readxl)

setwd("C:/dev/r/Chapter 1")
aapl <- read.zoo("aapl.csv",sep=",", header = TRUE, format = "%Y-%m-%d")
aapl
plot(aapl, main ="APPLE")

head(aapl)
tail(aapl)
aapl[which.max(aapl)]
#?��것처?�� ?��?��?��?�� ?��?�� ?��료로?�� ?��?��?��?�� 구하�? ?��?��?��.
?diff
tail(aapl)
tail(lag(aapl,k=-1))
#?��?���?
ret_simple <- diff(aapl) / lag(aapl,k=-1)*100
ret_cont <- diff(log(aapl))*100
plot(ret_simple)
tail(ret_simple)
tail(ret_cont)

?coredata
coredata(ret_simple)
summary(coredata(ret_simple))

ret_simple[which.min(ret_simple)]

hist(ret_simple,breaks = 100,xlab = "%")

aapl_2013 <- window(aapl,start = '2013-01-01',end = '2013-12-31')
aapl_2013[which.max(aapl_2013)]

#Value at Risk: 최�?� ?��?�� 금액
quantile(ret_simple,probs=0.01)
#?��?��?��?�� -7.04?��?���? ?��?�� ?��률이 1%?��?��.

install.packages("forecast")
library("forecast")
hp <- read.zoo("UKHP.csv", sep=",", header=TRUE, format = "%Y-%m"
               ,FUN = as.yearmon)
hp_ret <- diff(hp)/lag(hp,k=-1)*100#?��?��
mod <- auto.arima(hp_ret,stationary = TRUE,
                  seasonal = FALSE, ic="aic")
mod
#ar1, ar2, mean?�� ??�?�� 값을 보여준?��.
#�? 모델?�� ??�?�� 값중 aic가 가?�� ?���? ?��?��?��므�? 가?�� ?��?��?��?��.
confint(mod)
par("mar")

par(mar=c(1,1,1,1))

tsdiag(mod)

plot(mod$x,lty=1,main="test")
lines(fitted(mod),lty=2,lwd=2,col="red")

accuracy(mod)

predict(mod,n.ahead = 3)
plot(forecast(mod))


install.packages("urca")
library("urca")
price <- read.zoo("JetFuelHedging.csv",sep=",",FUN=as.yearmon,
                  format = "%Y-%m",header = TRUE)
#최적?��지비율
simple_mod <- lm(diff(price$JetFuel)~diff(price$HeatingOil)+0)
#?��기서 +0??� ?��?��, 근데 ?�� ~�? ?��?��? 비율?
summary(simple_mod)
plot(price$JetFuel, xlab = "Date",ylab = "USD")
lines(price$HeatingOil,col="red")

?ur.df
#?��?��근�?�?��
#?��?��?��??� ?��지�? 추세가 ?��?��...
plot(price$JetFuel)
jf_adf <- ur.df(price$JetFuel, type="drift")
summary(jf_adf)
ho_adf <- ur.df(price$HeatingOil,type="drift")
summary(ho_adf)
#공적분을 ?�� ?��, ?�� 그래?��가 ?��률적 추세�? 가?��?�� ?��?��. 그래?�� 
#?��?��근�?�?��?�� ?��?��.

#?��?��?�� 균형 모델 추정 �? ?���? ?��?��?�� 검?��
mod_static <- summary(lm(price$JetFuel~price$HeatingOil))
error <- residuals(mod_static)
error_cadf <- ur.df(error,type="none")#?��차에?�� ?��?�� ?��?���? 검?��?�� ?��주면!
summary(error_cadf)#결과 ?��?��
#??�?��값이?�� 검?��?��계량 값을 비교?��보면 검?��?��계량 값이 ?�� ?���? ?��문에
#귀무�?�?��?�� 기각?��?��. ?��?��?�� ?��?��?��?��?��.


#?���? 교정 모델 ?��?��
dif <- diff(price$JetFuel)
dho <- diff(price$HeatingOil)
error_lag <- lag(error,k=-1)
mod_ecm <- lm(dif~dho+error_lag+0)
summary(mod_ecm)



#변?��?�� 모델
intc <- read.zoo("intc.csv",header = TRUE,
                 sep = ",", format = "%Y-%m",FUN = as.yearmon)
plot(intc)

#Grarch
install.packages("rugarch")
library("rugarch")
intc_garch11_spec <- ugarchspec
(variance.model = list(
  garchOrder =c(1,1)),
  mean.model = list(armaOrder=c(0,0)))
intc_garch11_spec
intc_garch11_fit <- ugarchfit(spec = intc_garch11_spec,data=intc)
intc_garch11_fit