###마늘 생산량 데이터
ma<-read.csv("ma.csv")
ma
str(ma)
summary(ma$outcome)
hist(ma$outcome)
qqnorm(ma$outcome)
qqline(ma$outcome) #정규분포를 따른 다고 할 수 있다.

###회귀 분석 모델 만들기
weather$rain
weather$sun
cor(weather$rain,ma$outcome,use="complete.obs") #0과 가까운 값
cor(weather$sun,ma$outcome,use="complete.obs") #1과 가까운 

#다중 회귀
model<-lm(ma$outcome~weather$rain+weather$sun,data=ma)
summary(model) #rain변수 p-value 0.05이상임

#일조량 선형회귀
model_s<-lm(ma$outcome~weather$sun)
summary(model_s)
plot(x=weather$sun,y=ma$outcome,col="red")
a=model_s$coef[1]; b=model_s$coef[2]
abline(a,b)

#강수량 선형회귀
model_r<-lm(ma$outcome~weather$rain)
summary(model_r)
plot(x=weather$rain,y=ma$outcome,col="red")
a=model_r$coef[1]; b=model_r$coef[2]
abline(a,b) #기울기가 0에 가깝기 때문에 유의미한 관계라고 볼 수 없다.

#변수 별 검정
anova(model,model_r) #p-value가 0.05보다 작기 때문에 귀무가설 채택.
anova(model,model_s) #p-value가 0.05보다 크기 때문에 귀무가설 기각.

#예측하기
predict(model_s,newdata=data.frame(weather$sun=))