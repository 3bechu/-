###날씨 데이터###
weather<-read.csv(“weather.csv” , fileEncoding = "UTF-8")
weather
weat<-weather[,-1]
install.packages(“dplyr”)
library(dplyr)
weather=rename(weather, rain=1, sun=2) #데이터셋의 변수명을 강수량과 일조량으로 변경한다.

#날씨데이터 파악하기
weather
barplot(weather$sun,names.arg=weather$year,xlab="Year",ylab="sun")
str(weather$sun)
summary(weather$sun)
qqnorm(weather$sun)
qqline(weather$sun) #날씨 데이터가 정규분포를 따르는 것을 알 수 있다.

#상관계수 구하기
cor(weather$sun,weather$year,use="complete.obs") #상관계수가 1에 가까우므로 정비례 관계를 가질 것으로 예상할 수 있다.
cor(weather$rain,weather$year,use="complete.obs") #상관계수가 0에 가까우므로 유의미한 관계를 가진다고 할 수 없다.
#따라서 연도별 예측값은 일조량만 구한다.

#일조량-연도 단순회귀
model_w<-lm(sun~year,data=weather)
summary(model_w) 
plot(x=weather$year,y=weather$sun,col="red")
a=model_w$coef[1]; b=model_w$coef[2]
abline(a,b)

#일조량 예측값과 신뢰구간(2018년도 일조량)
predict(model_w, newdata = data.frame(year = c(2018)), interval = "confidence")
#실제 관측값은 202.675로 신뢰구간 내 위치한다. 따라서 이 선형 회귀 식의 정확도가 높다고 할 수 있다.

#일조량 예측값과 신뢰구간(2019년도 일조량)
predict(model_w, newdata = data.frame(year = c(2019)), interval = "confidence")
#예측값은 214.9254임을 알 수 있다. 이 예측값과 각 작물의 단순회귀모델을 이용해 2018, 2019년도의 생산량을 예측한다.



##생산량 데이터###

##배추 데이터 파악하기
chu<-read.csv(“bae.csv”, fileEncoding = "UTF-8") #배추 생산량 자료 불러오기
View(chu)
be<-cbind(chu, weat) #날씨 데이터와 통합하여 하나의 데이터셋으로 만든다.
be
hist(be$outcome)
qqnorm(be$outcome)
qqline(be$outcome) #배추 생산량이 정규분포를 따른다고 볼 수 있다.

#배추 생산량과 강수량/일조량 간의 다중회귀분석 모델 만들기
bmodel<-lm(outcome~sun+rain, data=be)
summary(bmodel)

#배추 생산량과 강수량의 선형회귀분석 모델
cor(be$outcome, be$rain) #0에 가까운 값이므로 두 변수가 유의미한 관계를 가지지 않는다고 할 수 있다.
bmodel_r=lm(outcome~rain, data=be)
a=bmodel_r$coef[1]; b=bmodel_r$coef[2]
a;b
plot(be$rain, be$outcome, col="red")
abline(a,b) #기울기가 0에 가깝기 때문에 유의미한 관계라고 볼 수 없다.

#배추 생산량과 일조량의 선형회귀분석 모델
cor(be$outcome, be$sun) #1에 가까운 값이므로 두 변수는 정의 상관관계를 갖는다고 할 수 있다. 
bmodel_s=lm(outcome~sun, data=be)
a=bmodel_s$coef[1]; b=bmodel_s$coef[2]
a;b
plot(be$sun, be$outcome, col="red")
abline(a,b)

#다중회귀모델과 단순회귀모델(강수량) 검정
#H0: bmodel=bmodel_r
#H1: bmodel=/=bmodel_r
anova(bmodel,bmodel_r) #p-value가 0.05보다 작다.
#다중회귀모델과 강수량 관련 단순회귀모델은 같은 모델이라고 할 수 없다.

#다중회귀모델과 단순회귀모델(일조량) 검정
#H0: bmodel=bmodel_s
#H1: bmodel=/=bmodel_s
anova(bmodel,bmodel_s) #p-value가 0.05보다 크다.
#다중회귀모델과 강수량 관련 단순회귀모델은 같은 모델로 볼 수 있다.

#예측값과 신뢰구간 구하기
View(bmodel_s)
predict(bmodel_s,newdata=data.frame(sun= c(202.675,214.9254)), interval = "confidence")
#따라서 2018년도의 배추 생산량은 7435.315이고 신뢰구간은6833.854 ~ 8036.775라고 할 수 있다.
#또한 2019년도의 배추 생산량은 7705.621이고 신뢰구간은 6979.808 ~ 8431.434라 할 수 있다.


##무 데이터 파악하기
mu<-read.csv(“moo.csv”, fileEncoding = "UTF-8")
View(mu)
moo<-cbind(mu,weat) #날씨 데이터와 통합하여 하나의 데이터셋으로 만든다.
moo
hist(moo$outcome)
qqnorm(moo$outcome)
qqline(moo$outcome) #무 생산량이 정규분포를 따르는 것을 알 수 있다.

#무 생산량과 강수량/일조량 간의 다중회귀분석 모델 만들기
mmodel<-lm(outcome~rain+sun, data=moo)
summary(mmodel)

#무 생산량과 강수량의 선형회귀분석 모델
cor(moo$outcome, moo$rain) #0과 가까운 값을 가진다. 따라서 무 생산량과 강수량이 유의미한 관계를 가진다고 보기는 어렵다.
mmodel_r=lm(outcome~rain, data=moo)
a=mmodel_r$coef[1]; b=mmodel_r$coef[2]
a;b
plot(moo$rain, moo$outcome, col="red")
abline(a,b)

#무 생산량과 일조량의 선형회귀분석 모델
cor(moo$outcome, moo$sun) #1과 가까운 값을 가지므로 무 생산량은 일조량과 양의 관계를 가진다고 할 수 있다.
mmodel_s=lm(outcome~sun, data=moo)
a=mmodel_s$coef[1]; b=mmodel_s$coef[2]
a;b
plot(moo$sun, moo$outcome, col="red")
abline(a,b)

#다중회귀모델과 단순회귀모델(강수량) 검정
#H0: mmodel=mmodel_r
#H1: mmodel=/=mmodel_r
anova(mmodel,mmodel_r) #p-value는 0.05보다 작다.
#다중회귀 모델과 강수량 단순회귀모델은 같은 모델로 볼 수 없다.

#다중회귀모델과 단순회귀모델(일조량) 검정
#H0: mmodel=mmodel_s
#H1: mmodel=/=mmodel_s
anova(mmodel,mmodel_s) #p-value가 0.05보다 크다
#다중회귀 모델과 일조량 관련 단순 회귀모델은 같은 모델로 볼 수 있다.

#생산량예측과 신뢰구간구하기
View(mmodel_s)
predict(mmodel_s, newdata = data.frame(sun= c(202.675,214.9254)), interval = "confidence")
#따라서 2018년도의 무 생산량은 5842.545이고 신뢰구간은 5383.027 ~ 6302.063라고 할 수 있다.
#또한 2019년도의 무 생산량은 6072.068이고 신뢰구간은 5517.544 ~ 6626.592라 할 수 있다.


##마늘 생산량
lic<-read.csv(“ma.csv”, fileEncoding = "UTF-8")
View(lic)
ma<-cbind(lic,weat) #날씨 데이터와 통합하여 하나의 데이터셋으로 만든다.
View(ma)
hist(ma$outcome)
qqnorm(ma$outcome)
qqline(ma$outcome) #마늘 생산량이 정규분포를 따른다고 할 수 있다.

#마늘 생산량과 강수량/일조량 간의 다중회귀분석 모델 만들기
gmodel<-lm(outcome~rain+sun, data=ma)
summary(gmodel) #강수량 변수의 p-value가 0.05 이상으로 통계적으로 유의미하지 않다.

#마늘 생산량과 강수량의 선형회귀분석 모델
cor(ma$outcome, ma$rain)
gmodel_r=lm(outcome~rain, data=ma)
a=gmodel_r$coef[1]; b=gmodel_r$coef[2]
a;b
plot(ma$rain, ma$outcome)
abline(a,b) #직선의 기울기가 0에 가까우므로 마늘 생산량과 강수량이 유의미한 관계를 가진다고 볼 수 없다.

#마늘 생산량과 일조량의 선형회귀분석 모델
cor(ma$outcome, ma$sun)
gmodel_s=lm(outcome~sun, data=ma)
a=gmodel_s$coef[1]; b=gmodel_s$coefficients[2]
a;b
plot(ma$sun, ma$outcome)
abline(a,b)

#다중회귀모델과 단순회귀모델(강수량) 검정
#H0: gmodel=gmodel_r
#H1: gmodel=/=gmodel_r
anova(gmodel,gmodel_r) #p-value가 0.05보다 작다.
#다중회귀 모델과 강수량 단순회귀모델은 같은 모델로 볼 수 없다.

#다중회귀모델과 단순회귀모델(일조량) 검정
#H0: gmodel=gmodel_s
#H1: gmodel=/=gmodel_s
anova(gmodel,gmodel_s) #p-value가 0.05보다 크다.
#다중회귀 모델과 일조량 단순 회귀모델은 같은 모델로 볼 수 있다.

#마늘 생산량예측과 신뢰구간구하기
View(gmodel_s)
predict(gmodel_s, newdata = data.frame(sun= c(202.675,214.9254)), interval = "confidence")
#따라서 2018년도의 마늘 생산량은 1306.454이고 신뢰구간은 1249.075 ~ 1363.833라고 할 수 있다.
#또한 2019년도의 마늘 생산량은 1331.854이고 신뢰구간은 1262.612 ~ 1401.096라 할 수 있다.


##기타 다른 작물
#양파 생산량 데이터 파악하기
yang<-read.csv("yang.csv", fileEncoding = "UTF-8")
pa<-cbind(yang,weat)
pa
View(pa)
hist(pa$outcome)
qqnorm(pa$outcome)
qqline(pa$outcome)
#양파 생산량 데이터는 정규분포를 따른다고 볼 수 있다.

#양파 생산량- 강수량과 일조량 다중회귀
ymodel<-lm(outcome~sun+rain, data=pa)
summary(ymodel)

#변수 별 상관계수 구하기
cor(pa$rain,pa$outcome) #상관계수가 0과 가까운 값을 가지므로 양파 생산량과 강수량은 유의미한 관계를 가진다고 할 수 없다.
cor(pa$sun,pa$outcome) #상관계수 0과 가까운 값을 가지므로 양파 생산량과 일조량은 유의미한 관계를 가진다고 할 수 없다.
#즉, 마늘 생산량은 강수량 혹은 일조량의 영향을 받지 않는다. 따라서 날씨 변수를 통해 양파 생산량을 예측하기 어렵다.


#고추 생산량 데이터 파악하기
gogo<-read.csv(file.choose(), fileEncoding = "UTF-8")
go<-cbind(gogo,weat) #날씨 데이터와 통합하여 하나의 데이터셋으로 만든다.
go
View(go)
hist(go$outcome)
qqnorm(go$outcome)
qqline(go$outcome)
#고추 생산량 데이터는 정규분포를 따르는 것을 알 수 있다.

#고추 생산량- 강수량과 일조량 다중회귀
gmodel<-lm(outcome~sun+rain, data=go)
summary(gmodel)

#변수 별 상관계수 구하기
cor(go$rain,go$outcome,use="complete.obs") #상관계수가 0과 가까운 값을 가지므로 고추 생산량과 강수량은 유의미한 관계를 가진다고 할 수 없다.
cor(go$sun,go$outcome,use="complete.obs") #상관계수 0과 가까운 값을 가지므로 고추 생산량과 일조량은 유의미한 관계를 가진다고 할 수 없다.
#고추 생산량은 날씨 변수의, 강수량과 일조량의 영향을 받지 않는다. 따라서 날씨 변수를 통해 고추 생산량을 예측할 수 없다.






