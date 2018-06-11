#양측검정,왼쪽,오른쪽
#평균차이
#대응표본 여부
#분산이 같은가 다른가
#신뢰도
?t.test
#
x <- c(168,160,170,162,168,163,164,167,175,179,161,155)
t.test(x)
t.test(x,conf.level = 0.90)
#위와 같은 표본이 있을 떄, 95%의 확률로 모평균은 

#2014년까지의 홈런기록, 2015년도에는 홈런을 몇 개 칠 수 있을 지
#90%, 95%, 99% 신뢰 구간에서 예측하고 실제 홈런 개수와 비교
#95~14
#13,9,32,38,54,36,39,47,56,14,30,41,30,8,16,5,15,21,13,32
x <- c(13,9,32,38,54,36,39,47,56,14,30,41,30,8,16,5,15,21,13,32)
t.test(x,conf.level = 0.90)
#t값은 7.8837, 자유도는 19, p-value는 0에 가까움
#양측검정이며 차이는 없다.
#90%의 신뢰 구간을 가지며 그 범위는 21~33이다.

#신뢰수준 99, 허용오차 +-100ml 표준편차 150ml
#표본의 갯수

#모비율 구간 보는 법
#binom.test 이항,..즉 이산을 따른다고 할 떄
?binom.test
binom.test(5,77)

#prop.test 정규 이산을 연속으로 바꾸어 주겠다...인자 correct 
prop.test(5,77)

#프리미엄 우유 가격은 20% 더 비쌈
#50명 중 4명이 선택을 바꿈, 그렇다면 90% 신뢰구간으로 프리미엄 우유에 대한
#선택이 달라질 비율의 신뢰구간을 구하라.
binom.test(4,50)
prop.test(4,50)

##모비율 추정


install.packages("EnvStats")
library(EnvStats)
varTest(trees$Volume)#분산에 대한 범
x
varTest(x)#x = dltmdduq homerun 

#나트륩 15개 함량, 검정하라, n=15, 유의수준 15
a <- c(308,302,290,292,327,290,320,285,315,285,295,288,310,325,300)
?t.test
t.test(a,mu=300)
#less
t.test(a,alternative = "less",mu=300)
#greater
t.test(a,alternative = "greater",mu=300)



#검정 바꾸기
binom.test(5,77,p=0.5)#양측
binom.test(5,77,p=0.5,alternative = "less")#단측,작다



#varTest(a)
varTest(a)

setwd("C:/dev/r/source2/준비파일")

diet <- read_excel("8장_대응표본의 가설검정.xlsx")
diet
attach(diet)
diet$`복용 전`
`복용 전`
t.test(`복용 전`,`복용 후`)


#
battery <- read_excel("독립표본 t 검정.xls")
battery
attach(battery)
head(battery)
제조사
battery$제조사
t.test(battery$제조사,battery$작동시간)





