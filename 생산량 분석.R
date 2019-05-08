#날씨 데이터
weather<-read.csv(“weather.csv” , fileEncoding = "UTF-8")
weather
weather<-weather[,-1]
install.packages(“dplyr”)
library(dplyr)
weather=rename(weather, rain=1, sun=2) #데이터셋의 변수명을 강수량과 일조량으로 변경한다.

##생산량 데이터###

##배추 생산량
chu<-read.csv(“bae.csv”, fileEncoding = "UTF-8") #배추 생산량 자료 불러오기
View(chu)
be<-cbind(chu, weather) #날씨 데이터와 통합하여 하나의 데이터셋으로 만든다.
be
hist(be$outcome)
qqnorm(be$outcome)
qqline(be$outcome) #배추 생산량이 정규분포를 따른다고 볼 수 있다.

#배추 생산량과 강수량/일조량 간의 다중회귀분석 모델 만들기
bmodel<-lm(outcome~sun+rain, data=be)
summary(bmodel)

#배추 생산량과 강수량의 단순회귀분석 모델
cor(be$outcome, be$rain) #0에 가까운 값이므로 두 변수가 유의미한 관계를 가지지 않는다고 할 수 있다.
bmodel_r=lm(outcome~rain, data=be)
a=bmodel_r$coef[1]; b=bmodel_r$coef[2]
a;b
plot(be$rain, be$outcome, col="red")
abline(a,b) #기울기가 0에 가깝기 때문에 유의미한 관계라고 볼 수 없다.

#배추 생산량과 일조량의 단순회귀분석 모델
cor(be$outcome, be$sun) #-1에 가까운 값이므로 두 변수는 음의 상관관계를 갖는다고 할 수 있다. 
bmodel_s=lm(outcome~sun, data=be)
a=bmodel_s$coef[1]; b=bmodel_s$coef[2]
a;b
plot(be$sun, be$outcome, col="red")
abline(a,b)

#변수 별 검정
anova(bmodel,bmodel_r) 
anova(bmodel,bmodel_s)

#예측하기
predict(bmodel_s,newdata=data.frame(weather$sun=))


##무 생산량
mu<-read.csv(“moo.csv”, fileEncoding = "UTF-8")
View(mu)
moo<-cbind(mu,weather) #날씨 데이터와 통합하여 하나의 데이터셋으로 만든다.
moo
hist(moo$outcome)
qqnorm(moo$outcome)
qqline(moo$outcome) #무 생산량이 정규분포를 따르는 것을 알 수 있다.

#무 생산량과 강수량/일조량 간의 다중회귀분석 모델 만들기
mmodel<-lm(outcome~rain+sun, data=moo)
summary(mmodel)

#무 생산량과 강수량의 단순회귀분석 모델
cor(moo$outcome, moo$rain) #0과 가까운 값을 가진다.
mmodel_r=lm(outcome~rain, data=moo)
a=mmodel_r$coef[1]; b=mmodel_r$coef[2]
a;b
plot(moo$rain, moo$outcome, col="red")
abline(a,b)

#무 생산량과 일조량의 단순회귀분석 모델
cor(moo$outcome, moo$sun) 1과 가까운 값을 가진다.
mmodel_s=lm(outcome~sun, data=moo)
a=mmodel_s$coef[1]; b=mmodel_s$coef[2]
a;b
plot(moo$sun, moo$outcome, col="red")
abline(a,b)

#변수 별 검정
anova(mmodel,mmodel_r) #p-value가 0에 가까움
anova(mmodel,mmodel_s) #p-value가 1에 가까움

#예측하기
predict(mmodel_s,newdata=data.frame(weather$sun=))


##마늘 생산량
lic<-read.csv(“ma.csv”, fileEncoding = "UTF-8")
View(lic)
ma<-cbind(lic,weather) #날씨 데이터와 통합하여 하나의 데이터셋으로 만든다.
View(ma)
hist(ma$outcome)
qqnorm(ma$outcome)
qqline(ma$outcome) #마늘 생산량이 정규분포를 따른다고 할 수 있다.

#마늘 생산량과 강수량/일조량 간의 다중회귀분석 모델 만들기
gmodel<-lm(outcome~rain+sun, data=ma)
summary(gmodel) #강수량 변수의 p-value가 0.05 이상으로 통계적으로 유의미하지 않다.

#마늘 생산량과 강수량의 단순회귀분석 모델
cor(ma$outcome, ma$rain)
gmodel_r=lm(outcome~rain, data=ma)
a=gmodel_r$coef[1]; b=gmodel_r$coef[2]
a;b
plot(ma$rain, ma$outcome)
abline(a,b) #직선의 기울기가 0에 가까우므로 마늘 생산량과 강수량이 유의미한 관계를 가진다고 볼 수 없다.

#마늘 생산량과 일조량의 단순회귀분석 모델
cor(ma$outcome, ma$sun)
gmodel_s=lm(outcome~sun, data=ma)
a=gmodel_s$coef[1]; b=gmodel_s$coefficients[2]
a;b
plot(ma$sun, ma$outcome)
abline(a,b)

#변수 별 검정
anova(gmodel,gmodel_r) #p-value가 0.05보다 작기 때문에 귀무가설 채택.
anova(gmodel,gmodel_s) #p-value가 0.05보다 크기 때문에 귀무가설 기각.

#예측하기
predict(gmodel_s,newdata=data.frame(weather$sun=))

