library(readxl)
setwd("C:/dev/r/source")
anova <- read_excel("낚시터.xlsx")
group <- as.character(anova$낚시터)
group
?lm
boxplot(anova$횟수~group)
bartlett.test(anova$횟수,group)#각 표본들의 분산이 같은지 다른지 판단.

?tapply
with(anova,tapply(anova$횟수, group, mean))#각 낚시터의 평균
with(anova,tapply(anova$횟수, group, sd))#각 낚시터들의 표준편차

out0 <- lm(anova$횟수~group)
summary(out0)
?aov
out1 <- aov(anova$횟수~group)#lm과 같은 모드로 사용될 수 있다.
summary(out1)

?anova#anova table로 보여주는 함수
out2 <- anova(out1)
summary(out2)

?TukeyHSD
TukeyHSD(out1)



#이원분산분석
anova2 <- read_excel("이원분산분석.xls")
anova2

group1 <- as.character(anova2$흡연석여부)
group2 <- as.character(anova2$위치)

boxplot(anova2$매출액~group1)
boxplot(anova2$매출액~group2)

with(anova2,tapply(anova2$매출액, group1, mean))
with(anova2,tapply(anova2$매출액, group2, mean))

#group1*group2는 상호작용을 뜻한다.
out1 = aov(anova2$매출액~group1+group2 + group1*group2)
anova(out1)
#상호작용도 유의하므로 두 독립변수는 서로에게도 영향을 준다.

TukeyHSD(out1)


#상관관계.

#산정도
plot(dist~speed, data=cars,type="p",pch=20,col = "blue",cex=2)

cov(cars$dist,cars$speed)
#단위의 문제가 있으므로 공분산으로 해석하기는 어렵다.
#따라서 공분신을 표준화한 상관계수를 사용한다.

cor(cars$dist,cars$speed)
#0과 1의 사이의 값을 가지므로 양의 상관관계를 가지고있다.

?cor.test
#두 개의 샘플에 대해서 상관관계가 있는지 검사해줌
cor.test(~dist+speed, data=cars,method=c("pearson"))
#귀무가설: 상관계수가 0이다.
#대립가설: 상관계수가 0이 아니다.
#p-value가 0에 가깝게 나왔으므로 대립가설 채택
#또한 상관계수가 0과 1 사이의 값을 가지므로 양의 상관관계

ad <- read_excel("단순 회귀분석.xls")
plot(ad$매출액~ad$광고비, data = ad, type = "p", pch=20,col="blue",cex=2)
cov(ad$매출액, ad$광고비)
cor(ad$매출액, ad$광고비)
cor.test(ad$매출액,ad$광고비, data=ad, method=c("pearson"))


#교차분석(바다의 선호도,산의 선호도)
vacation <- c(68,32)
prob <- c(0.5,0.5)
?chisq.test
chisq.test(vacation,p=prob)#카이제곱 테스트,적합도 테스트, 기대확률p
#p-value가 낮으므로 귀무가설을 기각한다.
#따라서 바다와 산의 선호도가 다르다고 할 수 있다.


#문제
#기대확률 9:3:3:1
#유의수준 0.05
mendel <- c(315,101,108,32)
prob <- c(9,3,3,1)/16
chisq.test(mendel,p=prob)
#귀무가설: 관측빈도와 기대빈도는 같다.
#대립가설: 관측빈도와 기대빈도는 다르다.
#p-value가 0.9254로 유의수준 0.05보다 매우 크게 나왔으므로
#귀무가설을 기각하지 못한다.
#따라서 귀무가설을 채택하며 mendel의 법칙은 맞고 할 수 있다.

purch <- read_excel("교차_카이제곱.xls")
?xtabs
purch.table <- xtabs(~purch$구매의사+purch$지역,data=purch)
purch.table

chisq.test(purch.table)
#table을 넣어줄 때는 p를 넣어주지 않아도 되는가?
#귀무가설: 지역에 따라서 구매의사가 같을 것이다.
#결과는 지역에 따라 구매의사가 차이가 있다는 것을 나타낸다.

#회귀분석 회귀식을 추정하고 그것이 유의한지 확인한다.
ad <- read_excel("단순 회귀분석.xls")
ad
plot(ad$매출액~ad$광고비, data = ad, type = "p", pch=20,col="blue",cex=2)
reg <- lm(ad$매출액~ad$광고비)
#Intercept가 y절편, 뒤의 ad$광고비가 추정된 B1
summary(reg)
#B0, B1
confint(reg)
#정규분포를 따르는지 검사
res <- residuals(reg)
shapiro.test(res)
#res가 정규분포를 따르지 않는다.



#문제
ad2 <- read_excel("친절도-재구매.xlsx")
ad2
plot(ad2$재구매~ad2$친절도, data = ad2, type = "p", pch=20,col="blue",cex=2)

reg2 <- lm(ad2$재구매~ad2$친절도)#선형 회귀선 추정
summary(reg2)#B0는 1.1250이고 B1은 0.7591이다.
#표준 에러는 B0는 0.5923이고 B1은 0.2069이다.
#t-value는 B0는 1.899이고 B1는 3.670이다.
#p-value는 B0는 0.06788이고 B1는 0.00101이다.
#만약 유의수준이 0.05라면 B0는 유의하지 않다.
#하지만 그 값을 뺼 경우 추정된 식이 이상하게 되므로 그래도 가져다 쓴다.
#그에 반해 B1은 유의하다.
#이 두 값을 기반으로 하여 이 두 변수에 대한 회귀식은
#p-value는 낮으므로 유의하긴 하다.
#하지만 R-squared가 0.3248정도로 100%중 32% 정도를 설명하고 있다.
#거의 설명하지 못하고 있다고 할 수 있다.
confint(reg2)#신뢰구간 확인, default: 95%
#**결론***
#회귀분석: 두 변수에 대한 유의성을 회귀선을 유추하고
#표준오차보다 회귀식으로 회귀식이 유의한지 유의하지 않은지 검정
#하지만 위의 방법은 최소자승법의 조건들을 만족해야 최고의 방법이다.
#마지막 순서로 그 세가지 조건을 만족하는지 검사한다.
#library(car)
#잔차
install.packages("car")
library(car)
library(car)
res = residuals(reg)
res
#**조건1. 정규분포를 따른다.
?shapiro.test
shapiro.test(res)#잔차가 유의하지 않다?
#p-value가 낮으므로 '귀무가설: 정규분포를 따른다'를 기각한다.

plot(ad2$재구매~ad2$친절도,cex=1,lwd=1)
abline(reg,lwd=2,col="red")
#**조건2. 잔차의 평균이 0이다.
#**조건3. 공분산이 0이다.
#**조건4. 동분산을 갖는다.




#@@@다중회귀분석 
phone <- read_excel("다중 회귀분석_요인저장_변수 계산.xls")
phone
reg1 = lm(phone$만족감~phone$외관+phone$유용성+phone$편의성)
summary(reg1)
confint(reg1)
res1　 <- residuals(reg1)
#정규성 검정
shapiro.test(res1)

#잔차 독립성 검정
durbinWatsonTest(res1)

#다중공선성

library(car)
####다중회귀 문제####
s <- read_excel("다중회귀분석.xlsx")
s
reg2 <- lm(s$재구매~s$친절도+s$사은품)
summary(reg2)
#세 변수 모두 유의함을 확인.
#그리고 또한 조정된 결정계수 확인 결과 회귀식이 전체의 29.7%를 설명함
#추정된 회귀식의 p-value가 0.00324로 귀무가설을 기각하므로 회귀식이 유의함.
confint(reg2)
#세 변수에 대한 신뢰구간은 위와 같다.
res2 <- residuals(reg2)
res2

#회귀식이 유의함을 확인했으므로 이제 선택한 최소자승법이 BLUE임을 확인한다.


#***정규성 검정
shapiro.test(res2)
#p-value가 0.068이므로 유의수준이 0.05라면
#'귀무가설: 정규분포를 따른다'고 할 수 있다.

#***잔차 독립성 검정
# 2에 가까우면 독립적인 관계 1.4보다 작으면 양의 상관
durbinWatsonTest(res2)

#***다중공선성
vif=vif(reg2)# 10보다 작으면 다중공선성이 없다고 
vif
install.packages("devtools")
devtools::install_github("han2290/Rstudy")































