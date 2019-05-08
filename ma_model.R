#마늘 생산량 데이터 파악
lic<-read.csv(file.choose(), fileEncoding = "UTF-8")
View(lic)
ma<-cbind(lic,weather[,-1])
View(ma)
hist(ma$outcome)
qqnorm(ma$outcome)
qqline(ma$outcome)#마늘 생산량 데이터는 정규분포를 따른다고 볼 수 있다.

#마늘 생산량- 강수량과 일조량 다중회귀
gmodel<-lm(outcome~rain+sun, data=ma)
summary(gmodel)

#마늘 생산량-강수량 단순회귀
cor(ma$outcome, ma$rain) #상관계수가 0과 가깝기 때문에 강수량과 마늘 생산량 사이는 유의미한 관계를 가진다고 할 수 없다.
gmodel_r=lm(outcome~rain, data=ma)
a=gmodel_r$coef[1]; b=gmodel_r$coef[2]
a;b
plot(ma$rain, ma$outcome)
abline(a,b)

#마늘 일조량 단순회귀
cor(ma$outcome, ma$sun) ##상관계수가 1과 가깝기 때문에 일조량과 마늘 생산량은 정의 관계를 가진다고 할 수 있다.
gmodel_s=lm(outcome~sun, data=ma)
a=gmodel_s$coef[1];b=gmodel_s$coefficients[2]
a;b
plot(ma$sun, ma$outcome)
abline(a,b)

#다중회귀모델과 단순회귀모델(강수량) 검정
#H0: gmodel  gmodel_r
#H1: 
anova(gmodel,gmodel_r) #p-value는 0.05보다 작음
#다중회귀 모델과 강수량 단순회귀모델은 같은 모델로 볼 수 없다.

#다중회귀모델과 단순회귀모델(일조량) 검정
#H0: gmodel  gmodel_s
#H1: 
anova(gmodel,gmodel_s) #p-value는 0.05보다 큼
#다중회귀 모델과 일조량 관련 단순 회귀모델은 같은 모델로 볼 수 있다.

#마늘 생산량예측과 신뢰구간구하기
View(gmodel_s)
predict(gmodel_s, newdata = data.frame(sun= c(202.675,214.9254)), interval = "confidence")
#따라서 2018년도의 마늘 생산량은 1306.454이고 신뢰구간은 1249.075 ~ 1363.833라고 할 수 있다.
#또한 2019년도의 마늘 생산량은 1331.854이고 신뢰구간은 1262.612 ~ 1401.096라 할 수 있다.