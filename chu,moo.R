#배추 데이터 파악하기
chu<-read.csv(file.choose(), fileEncoding = "UTF-8")
weather<-read.csv(file.choose())
View(chu)
View(weather)
be<-cbind(chu, weather)
View(be)
hist(be$outcome)
qqnorm(be$outcome)
qqline(be$outcome) 
#배추 생산량은 정규분포를 따른 다고 볼 수 있다.

#배추 생산량-강수량과 일조량 다중회귀
bmodel<-lm(outcome~sun+rain, data=be)
summary(bmodel)

#배추 생산량- 강수량 단순회귀
cor(be$outcome, be$rain) #상관계수가 0과 가깝기 때문에 강수량과 배추 생산량 사이는 유의미한 관계를 가진다고 할 수 없다.
bmodel_r=lm(outcome~rain, data=be)
a=bmodel_r$coef[1]; b=bmodel_r$coef[2]
a;b
plot(be$rain, be$outcome, col="red")
abline(a,b)

#배추 생산량- 일조량 단순회귀
cor(be$outcome, be$sun) #상관계수가 1과 가깝기 때문에 일조량과 배추 생산량은 정의 관계를 가진다고 할 수 있다.
bmodel_s=lm(outcome~sun, data=be)
a=bmodel_s$coef[1]; b=bmodel_s$coef[2]
a;b
plot(be$sun, be$outcome, col="red")
abline(a,b)

#다중회귀모델과 단순회귀모델(강수량) 검정
#H0: bmodel  bmodel
#H1: 
anova(bmodel,bmodel_r) #p-value는 0.05보다 작음
#다중회귀 모델과 강수량 관련 단순 회귀모델은 같은 모델로 볼 수 없다.

#다중회귀모델과 단순회귀모델(일조량) 검정
#H0: bmodel  bmodel
#H1: 
anova(bmodel,bmodel_s) #p-value는 0.05보다 큼
#다중회귀 모델과 일조량 관련 단순 회귀모델은 같은 모델로 볼 수 있다.

#예측값과 신뢰구간구하기
View(bmodel_s)
predict(bmodel_s, newdata = data.frame(sun= c(202.675,214.9254)), interval = "confidence")
#따라서 2018년도의 배추 생산량은 7435.315이고 신뢰구간은6833.854 ~ 8036.775라고 할 수 있다.
#또한 2019년도의 배추 생산량은 7705.621이고 신뢰구간은 6979.808 ~ 8431.434라 할 수 있다.


#무 데이터 파악하기
mu<-read.csv(file.choose(), fileEncoding = "UTF-8")
weather<-read.csv(file.choose())
rain<-weather[,2]
rain
sun<-weather[,3]
sun
View(mu)
moo<-cbind(mu,sun,rain)
moo
hist(moo$outcome)
qqnorm(moo$outcome)
qqline(moo$outcome)
#따라서 무 생산량 데이터는 정규분포를 따른다고 볼 수 있다.

#무 생산량- 강수량과 일조량 다중회귀
mmodel<-lm(outcome~rain+sun, data=moo)
summary(mmodel)

#무생산량- 강수량 단순회귀
library(car)
cor(moo$outcome, moo$rain) #상관계수가 0에 가깝기 때문에 무 생산량과 강수량은 유의미한 관계를 갖는다고 할 수 없다.
mmodel_r=lm(outcome~rain, data=moo)
a=mmodel_r$coef[1]; b=mmodel_r$coef[2]
a;b
plot(moo$rain, moo$outcome, col="red")
abline(a,b)


#무 생산량- 일조량 단순회귀
cor(moo$outcome, moo$sun) #상관계수가 1과 가깝기 때문에 무 생산량과 일조량 사이에는 양의 관계를 가진다고 할 수 있다.
mmodel_s=lm(outcome~sun, data=moo)
a=mmodel_s$coef[1]; b=mmodel_s$coef[2]
a;b
plot(moo$sun, moo$outcome, col="red")
abline(a,b)

#다중회귀모델과 단순회귀모델(강수량) 검정
#H0: mmodel  mmodel_r
#H1: 
anova(mmodel,mmodel_r) #p-value는 0.05보다 작음
#다중회귀 모델과 강수량 단순회귀모델은 같은 모델로 볼 수 없다.

#다중회귀모델과 단순회귀모델(일조량) 검정
#H0: mmodel  mmodel_s
#H1: 
anova(mmodel,mmodel_s) #p-value는 0.05보다 큼
#다중회귀 모델과 일조량 관련 단순 회귀모델은 같은 모델로 볼 수 있다.

#생산량예측과 신뢰구간구하기
View(mmodel_s)
predict(mmodel_s, newdata = data.frame(sun= c(202.675,214.9254)), interval = "confidence")
#따라서 2018년도의 무 생산량은 5842.545이고 신뢰구간은 5383.027 ~ 6302.063라고 할 수 있다.
#또한 2019년도의 무 생산량은 6072.068이고 신뢰구간은 5517.544 ~ 6626.592라 할 수 있다.
