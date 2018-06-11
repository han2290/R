setwd("C:\dev\r\source")
anova <- read_excel("일원분산분석.xls")
group <- as.character(anova$편의점)
boxplot(anova$만족도~group)#~표시는 독립변수
bartlett.test(anova$만족도~group)
#group에 의한 만족도를 보면 

with(anova, tapply(anova$만족도, group, mean))
with(anova, tapply(anova$만족도, group, sd))

out1 <- lm(anova$만족도~group)#이것을 분산분석에서 사용할 수 있다.

summary(out1)
#result?

out2 <- aov(anova$만족도~group)#q
out2
summary(out2)

out3 <- anova(out1)
out3

#보수적으로 검정
?TukeyHSD
TukeyHSD(out2)# p adj 조정된 p,

anova<- read_excel("낚시터.xls")
anova

group <- as.character(anova$낚시터)
group
boxplot(anova$횟수~group)
bartlett.test(anova$횟수~group)

out1 <- aov(anova$횟수~group)
out1
TukeyHSD(out1)
out2 <- anova(out1)
out2


#이원
anova<- read_excel("이원분산분석.xls")
anova

group1 <- as.character(anova$흡연석여부)
group2 <- as.character(anova$위치)

boxplot(anova$매출액~group1)
boxplot(anova$매출액~group2)

out1 <- lm(anova$매출액~group1)#평균의 차이인가 그럴껄?
out1
summary(out1)
bartlett.test(anova$매출액~group1)

out2 <- lm(anova$매출액~group2)
out2
summary(out2)
bartlett.test(anova$매출액~group2)

#만약 상호작용까지 보려면 상호작용에 대한 가설도 세워야함.

