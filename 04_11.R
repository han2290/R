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
#?´ê²ƒì²˜?Ÿ¼ ? •?ƒ?„±?´ ?•„?‹Œ ?žë£Œë¡œ?Š” ?ˆ˜?µ?œ¨?„ êµ¬í•˜ê¸? ?ž˜?“¤?‹¤.
?diff
tail(aapl)
tail(lag(aapl,k=-1))
#?ˆ˜?µë¥?
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

#Value at Risk: ìµœë?€ ?†?‹¤ ê¸ˆì•¡
quantile(ret_simple,probs=0.01)
#?ˆ˜?µ?œ¨?´ -7.04?´?•˜ë¡? ?‚˜?˜¬ ?™•ë¥ ì´ 1%?´?‹¤.

install.packages("forecast")
library("forecast")
hp <- read.zoo("UKHP.csv", sep=",", header=TRUE, format = "%Y-%m"
               ,FUN = as.yearmon)
hp_ret <- diff(hp)/lag(hp,k=-1)*100#?ˆ˜?µ
mod <- auto.arima(hp_ret,stationary = TRUE,
                  seasonal = FALSE, ic="aic")
mod
#ar1, ar2, mean?— ??€?•œ ê°’ì„ ë³´ì—¬ì¤€?‹¤.
#ê°? ëª¨ë¸?— ??€?•œ ê°’ì¤‘ aicê°€ ê°€?ž¥ ?‚®ê²? ?‚˜?™”?œ¼ë¯€ë¡? ê°€?ž¥ ? ?•©?•˜?‹¤.
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
#ìµœì ?•´ì§€ë¹„ìœ¨
simple_mod <- lm(diff(price$JetFuel)~diff(price$HeatingOil)+0)
#?—¬ê¸°ì„œ +0??€ ? ˆ?Ž¸, ê·¼ë° ?™œ ~ë¥? ?–ˆ?‚˜? ë¹„ìœ¨?
summary(simple_mod)
plot(price$JetFuel, xlab = "Date",ylab = "USD")
lines(price$HeatingOil,col="red")

?ur.df
#?‹¨?œ„ê·¼ê?€? •
#?ƒ?ˆ˜?•­??€ ?žˆì§€ë§? ì¶”ì„¸ê°€ ?—†?Š”...
plot(price$JetFuel)
jf_adf <- ur.df(price$JetFuel, type="drift")
summary(jf_adf)
ho_adf <- ur.df(price$HeatingOil,type="drift")
summary(ho_adf)
#ê³µì ë¶„ì„ ?•  ?•Œ, ?‘ ê·¸ëž˜?”„ê°€ ?™•ë¥ ì  ì¶”ì„¸ë¥? ê°€? ¸?•¼ ?•œ?‹¤. ê·¸ëž˜?„œ 
#?‹¨?œ„ê·¼ê?€? •?„ ?•œ?‹¤.

#? •?ƒœ?  ê· í˜• ëª¨ë¸ ì¶”ì • ë°? ?ž”ì°? ? •?ƒ?„± ê²€? •
mod_static <- summary(lm(price$JetFuel~price$HeatingOil))
error <- residuals(mod_static)
error_cadf <- ur.df(error,type="none")#?ž”ì°¨ì—?„œ ?‹¤?‹œ ?‹¨?œ„ê·? ê²€? •?„ ?•´ì£¼ë©´!
summary(error_cadf)#ê²°ê³¼ ?™•?¸
#??€?š°ê°’ì´?ž‘ ê²€? •?†µê³„ëŸ‰ ê°’ì„ ë¹„êµ?•´ë³´ë©´ ê²€? •?†µê³„ëŸ‰ ê°’ì´ ?” ?ž‘ê¸? ?•Œë¬¸ì—
#ê·€ë¬´ê?€?„¤?„ ê¸°ê°?•œ?‹¤. ?”°?¼?„œ ?•ˆ? •? ?´?‹¤.


#?˜¤ë¥? êµì • ëª¨ë¸ ?‹œ?–‰
dif <- diff(price$JetFuel)
dho <- diff(price$HeatingOil)
error_lag <- lag(error,k=-1)
mod_ecm <- lm(dif~dho+error_lag+0)
summary(mod_ecm)



#ë³€?™?„± ëª¨ë¸
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
