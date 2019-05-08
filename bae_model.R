weather<-read.csv(file.choose())
weather
####생산량 데이터###
bae
str(bae)
summary(bae$outcome)
hist(bae$outcome)
qqnorm(bae$outcome)
qqline(bae$outcome) 
#배추 생산량은 정규분포 따른다고 볼 수 있다.

##회귀분석 모델 만들기
weather$rain
weather$sun
cor(weather$rain,bae$outcome,use="complete.obs") #0에 가까운 값이므로 두 변수는 유의미한 관계를 갖지 않는다고 할 수 있다.
cor(weather$sun,bae$outcome,use="complete.obs") #-1과 가까운 값이므로 두 변수는 음의 관계를 갖는다고 할 수 있다.

#다중 회귀
model<-lm(bae$outcome~weather$rain+weather$sun,data=bae)
summary(model)

#일조량 선형회귀
model_s<-lm(bae$outcome~weather$sun)
summary(model_s)
plot(x=weather$sun,y=bae$outcome,col="red")
a=model_s$coef[1]; b=model_s$coef[2]
abline(a,b)

#강수량 선형회귀
model_r<-lm(bae$outcome~weather$rain)
summary(model_r)
plot(x=weather$rain,y=bae$outcome,col="red")
a=model_r$coef[1]; b=model_r$coef[2]
abline(a,b) #기울기가 0에 가깝기 때문에 유의미한 관계라고 볼 수 없다.

#변수 별 검정
#h0:???
#h1:????
anova(model,model_r) 
anova(model,model_s)

#예측하기
predict(model_s,newdata=data.frame(weather$sun=))
