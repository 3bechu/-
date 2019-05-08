###무 생산량 데이터
moo<-read.csv("moo.csv")
moo
str(moo)
summary(moo$outcome)
hist(moo$outcome)
qqnorm(moo$outcome)
qqline(moo$outcome) #정규분포를 따른 다고 할 수 있다.

###회귀 분석 모델 만들기
weather$rain
weather$sun
cor(weather$rain,moo$outcome,use="complete.obs") #0과 가까운 값
cor(weather$sun,moo$outcome,use="complete.obs") #1과 가까운 

#다중 회귀
model<-lm(moo$outcome~weather$rain+weather$sun,data=moo)
summary(model)

#일조량 선형회귀
model_s<-lm(moo$outcome~weather$sun)
summary(model_s)
plot(x=weather$sun,y=moo$outcome,col="red")
a=model_s$coef[1]; b=model_s$coef[2]
abline(a,b)

#강수량 선형회귀
model_r<-lm(moo$outcome~weather$rain)
summary(model_r)
plot(x=weather$rain,y=moo$outcome,col="red")
a=model_r$coef[1]; b=model_r$coef[2]
abline(a,b) #기울기가 0에 가깝기 때문에 유의미한 관계라고 볼 수 없다.

#변수 별 검정
anova(model,model_r) #p-value가 0과 가까움
anova(model,model_s) #p-value가 1과 가까움

#예측하기
predict(model_s,newdata=data.frame(weather$sun=))