#채권가치 평가
install.packages("drat")
drat::addRepo("ghrr")
options("repos")


install.packages("RQuantLib", type="binary")
library(RQuantLib)





install.packages("termstrc")
library(termstrc)
data(govbonds)
str(govbonds)[['GERMANY']]

prepro <- prepro_bond('GERMANY',govbonds)
prepro

m <- prepro$m[[1]]; m
n <- ncol(m);n
s <- round(sqrt(n));s
c(floor(min(m[,1])), max(m[,ncol(m)]))


#####
Quandl.api_key("mJsu_Gwr6p_L5dQ4YALx")
G <- Quandl('WIKI/GOOGL',
                start_date='2014-04-01',end_date='2016-02-19')
str(G)

G <- G$Close

SP500 <- Quandl("SPDJ/SPX",start_date='2014-04-01',end_date='2016-02-19')
SP500 <- SP500$'S&P 500'
str(SP500)
LIBOR <- Quandl('FED/RILSPDEPM01_N_B',start_date='2014-04-01',end_date='2016-02-19')
LIBOR <- LIBOR$Value
cdates <- Reduce(intersect,list(G$Date,SP500$Date,LIBOR$Date))

###################
G <- G[G$Date %in% cdates,'Close']

LIBOR <- LIBOR[LIBOR$Date,]
SP500
cov()



##########################################
#파생상품 가격 결정
install.packages("fOptions")
library(fOptions)

GBSOption(TypeFlag = "c",S=900,X=950,Time=1/4,r=0.02,sigma=0.22,b=0.02)
GBSOption(TypeFlag = "p",S=900,X=950,Time=1/4,r=0.02,sigma=0.22,b=0.02)@price

##########################################콜옵션
sapply(c('delta','gamma','vega','theta','rho'), function(greek)
    GBSGreeks(Selection = greek, TypeFlag = "c",S=900,X=950,
              Time = 1/4, r=0.02, b=0.02, sigma = 0.22)
  )

deltas <- sapply(c(1/4,1/20,1/50), function(t)
  sapply(500:1500, function(S)
    GBSGreeks(Selection = 'delta', TypeFlag = "c",S=S,X=950,
              Time = t, r=0.02, b=0.02, sigma = 0.22))
)

plot(500:1500, deltas[,1], ylab='Delta of call option',
     xlab='Price of the underlying (S)', type='l')
lines(500:1500, deltas[,2],col='blue')
lines(500:1500, deltas[,3],col='red')
legend("bottomright", legend = c('t=1/4','t=1/4','t=1/50'),
       col=c('black','blue','red'),pch=19)

####풋옵션
straddles <- sapply(c('c','p'), function(type)
    sapply(500:1500, function(S)
      GBSGreeks(Selection = 'delta',TypeFlag = type, S=S,
                X=950, Time=1/4, r=0.02, b=0.02, sigma = 0.22)))

plot(500:1500, rowSums(straddles),type='l',
     xlab='Price of the underlying (S)', ylab = 'Delta of straddle')

########################################
##변동성 자체를 추정하기 힘듦.
#그것을 추정하는 방법이 내재 변동성이다.
#구글의 옵션:
#만기일이 2013-09-21, 행사 가격이 미화 700달러~1150달러
#옵션들의 2013년 6월 25일 매도가격에 대한 자료가 있음
#이 정보에 따르면 해당 일자의 가격은 866.2 달러, 만기일까지 시간이 88일이 남음.
#따라서 88/360을 time 모수로 사용.
#무위험 이자율과 수행 비용은 앞으로 2% 남았다는 것을 가정한다.
setwd('C:/dev/r/Chapter 6')
goog <- read.csv('goog_calls.csv')

#주어진 모수들로 변동성 계산을 위해 각 데이터의 라인을 다음과 같이 루프로 실행
volatilites <- sapply(seq_along(goog$Strike),function(i)
  GBSVolatility(price=goog$Ask.Price[!is.na(i)],TypeFlag = "c",
                S=866.2, X = goog$Strike[!is.na(i)],Time=88/360,r=0.02,b=0.02)
  )
# "ACE", "ACT","ALTR","APOL","ARG", "BCR","BEAM","BF.B","BHI","BMC","BRCM","BRK.B",
#"BTU","CAM","CBG",
install.packages("tseries")
library(tseries)
###########모델 검정
symbols <- c("A", "AA", "AAPL", "ABC", "ABT", "ACN", 
             "ADBE", "ADI", "ADM", "ADP", "ADSK", "AEE", "AEP", "AES","AET", "AFL",
             "AGN", "AIG", "AIV", "AIZ", "AKAM", "ALL",  "ALXN", "AMAT", "AMD",
             "AMGN", "AMP", "AMT", "AMZN", "AN", "ANF", "AON", "APA", "APC", "APD",
             "APH",  "ATI", "AVB", "AVP", "AVY", "AXP", "AZO", "BA",
             "BAC", "BAX", "BBBY", "BBT", "BBY",  "BDX",  "BEN", 
              "BIIB", "BK", "BLK", "BLL",  "BMS", "BMY",  
             "BSX",  "BXP", "C", "CA", "CAG", "CAH",  "CAT", "CB", 
             "CBS", "CCE", "CCI", "CCL", "CELG", "CERN", "CF", "CHK", "CHRW", "CI",
             "CINF", "CL", "CLF", "CLX", "CMA", "CMCSA", "CME")

res <- lapply(symbols,function(symbol)
    get.hist.quote(symbol,quote='AdjClose',quiet=TRUE,
                   start = as.Date('2014-04-01'),end = as.Date('2016-03-01'))
  )
LIBOR <- Quandl('FED/RILSPDEPM01_N_B',start_date='2014-04-01',end_date='2016-03-01')
SP500 <- Quandl("SPDJ/SPX",start_date='2014-04-01',end_date='2016-03-01')
#SP500 <- 
cdates <- intersect(LIBOR$Date,SP500[SP500$`Effective date` %in% cdates,'S&P 500'])
d <- data.frame(date = as.Date(cdates, origin = '1970-01-01'))
d$day <- format(d$date, format='%d')
d$my <- format(d$date,format='%Y-%m')
fds <- with(d,tapply(day,my,min))
fds <- as.Date(paste(row.names(fds),fds,sep="-"))
res <- lapply(res,function(x) x[which(zoo::index(x) %in% fds)])
res <- do.call(merge,res)
str(res)
res <- as.data.frame(res)
names(res) <- symbols





