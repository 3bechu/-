###양파 생산량 데이터
yang<-read.csv("yang.csv")
yang
str(yang)
summary(yang$outcome)
hist(yang$outcome)
qqnorm(yang$outcome)
qqline(yang$outcome) #정규분포를 따른 다고 할 수 있다.

###회귀 분석 모델 만들기
weather$rain
weather$sun
cor(weather$rain,yang$outcome,use="complete.obs") #0과 가까운 값
cor(weather$sun,yang$outcome,use="complete.obs") #0과 가까운 값

#다중 회귀
model<-lm(yang$outcome~weather$rain+weather$sun,data=yang)
summary(model) #두가지 변수에 대한 p-value모두 0.05보다 크기때문에 귀무가설 기각.

#일조량 선형회귀
model_s<-lm(yang$outcome~weather$sun)
summary(model_s)
plot(x=weather$sun,y=yang$outcome,col="red")
a=model_s$coef[1]; b=model_s$coef[2]
abline(a,b) #그래프의 기울기가 0에 가깝기 때문에 유의미한 관계라고 볼 수 없다.

#강수량 선형회귀
model_r<-lm(yang$outcome~weather$rain)
summary(model_r)
plot(x=weather$rain,y=yang$outcome,col="red")
a=model_r$coef[1]; b=model_r$coef[2]
abline(a,b) #기울기가 0에 가깝기 때문에 유의미한 관계라고 볼 수 없다.

#변수 별 검정
anova(model,model_r) #p-value가 0.05보다 크므로 귀무가설 기각
anova(model,model_s) #p-value가 0.05보다 크므로 귀무가설 기각

#예측하기
predict(model_s,newdata=data.frame(weather$sun=))