#팩토리얼 함수 구현
myfactorial <- function(x){
  fact <- 1
  i <- x
  while(i>1){
    fact <- fact*i
    i <- i-1
  }
  return(fact)
}

myfactorial(5)

#자산 두개에 투자를 하는데
#분산효과, 각자산 표준편차 0.2, 0.3 인 경우
div <- seq(-0.2,1.2,length=100)
rhos <- c(-1,-0.5,0,0.5,1)
out <- matrix(0,nrow=100,ncol=5)
for(i in 1:5){
  out[,i] <- sapply(div, function(a){
    rho <- rhos[i]
    ans <- a^2*0.2^2+(1-a)^2*0.3^2+2*a*(1-a)*rho*0.2*0.3
    return(ans)
    })
}
out
matplot(div,out,type='l')
nms <- c('rho=-1','rho=-0.5','rho=0.5','rho=1')
legend("topright",legend = nms,lty=1:5, col = 1:5, bty="n")

# 포트폴리오 최적화 - 라그랑지 구현
#최소 리스크, 수익률 제한에서...
tail(out,2)
minvariance <- function(assets, mu = 0.005){
  return <- log(tail(assets,-1)/head(assets,-1))
  Q <- rbind(cov(return,use="complete.obs"),rep(1,ncol(assets)),
             colMeans(return,na.rm=TRUE))
  Q <- cbind(Q, rbind(t(tail(Q,2)),matrix(0,2,2)))
  B <- c(rep(0,ncol(assets)),1,mu)
  solve(Q,B)
}
install.packages("Quandl")
library(Quandl)
IT <- Quandl('DAROCZI/IT', start_Date = '2014-04-01', end_date='2016-02-19')

str(IT)
head(IT)

assets <- IT[,-1]
return <- log(tail(assets,-1)/head(assets,-1))
head(return)
#공분산 행렬
cov(return)
Q <- rbind(cov(return,use="complete.obs"),rep(1,ncol(assets)),
           colMeans(return,na.rm=TRUE))
round(Q,5)

Q <- cbind(Q, rbind(t(tail(Q,2)),matrix(0,2,2)))
round(Q,5)

mu <- 0.005#수익률
B <- c(rep(0,ncol(assets)),1,mu)
B

solve(Q,B)
head(solve(Q,B),ncol(assets))
##################
minvariance(IT[,-1])



####포트폴리오 투자선 Frontier
frontier <- function(assets){
  returns <- log(tail(assets,-1) / head(assets,-1))
  Q <- cov(return,use="complete.obs")
  n <- ncol(assets)
  r <- colMeans(return,na.rm=TRUE)

  Q1 <- rbind(Q,rep(1,n),r)
  Q1 <- cbind(Q1,rbind(t(tail(Q1,2)),matrix(0,2,2)))
  rbase <- seq(min(r),max(r),length = 100)
  s <- sapply(rbase,function(x){
    y <- head(solve(Q1, c(rep(0,n),1,x)),n)#비중이랑 람다가 있는 부분.
    y %*% Q %*% y
  })
  plot(s,rbase,xlab='Return',ylab='Variance')
}

frontier(assets)


#############패키지로 한 번에 ㄱㄱ######################
install.packages("timeSeries")
library(timeSeries)
IT <- timeSeries(IT[,2:6],IT[,1])
IT
IT[is.na(IT)]
#수익률
log(lag(IT)/IT)
IT_return <- returns(IT)#?returns
IT_return

#수익률 그래프
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.CumReturns(IT_return,legend.loc = 'topleft',main='')

#투자선 그래프
install.packages("fPortfolio")
library(fPortfolio)
plot(portfolioFrontier(IT_return))

Spec = portfolioSpec()
setSolver(Spec) = "solverRshorExact"
Frontier <- portfolioFrontier(as.timeSeries(IT_return),constraints = "LongOnly")
frontierPlot(Frontier,col=rep('orange',2),pch=19)
monteCarloPoints(Frontier, mcSteps = 1000, cex = 0.25, pch=19)
grid()

rm(Q)
rm(r)
rm(b)
#####접선 포트폴리오 자본시장선
n <- 6;mu <- 0.005
Q <- cbind(cov(return,use="complete.obs"),rep(0,n-1))#수익률의 공분산,1
Q <- rbind(Q,rep(0,n))
str(Q)
r <- c(colMeans(return,na.rm=TRUE),0.0001)#무위험수익률 대입(0.0001)
r#기대 수익

Q <- rbind(Q,rep(1,n),r)
Q <- cbind(Q,rbind(t(tail(Q,2)),matrix(0,2,2)))
b <- c(rep(0,n),1,mu)
Q
round(Q,6)

w <- solve(Q,b)
w <- head(w,-3)
w / sum(w)

return





