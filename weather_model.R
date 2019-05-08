#날씨데이터 파악하기
weather
barplot(weather$sun,names.arg=weather$year,xlab="Year",ylab="sun")
str(weather$sun)
summary(weather$sun)
qqnorm(weather$sun)
qqline(weather$sun) #정규분포를 따르는 것을 알 수 있다.

#상관계수 구하기
cor(weather$sun,weather$year,use="complete.obs") #상관계수가 1에 가까우므로 정의 관계를 가질 것으로 예상할 수 있다
cor(weather$rain,weather$year,use="complete.obs") #상관계수가 0에 가까우므로 유의미한 관계를 가진다고 할 수 없다.
#따라서 연도별 예측값은 일조량만 구한다.

#일조량-년도 단순회귀
model_w<-lm(sun~year,data=weather)
summary(model_w) #sun에 대한 p-value 값이 0.05이하이므로 귀무가설 채택
plot(x=weather$year,y=weather$sun,col="red")
a=model_w$coef[1]; b=model_w$coef[2]
abline(a,b)

#일조량 예측값과 신뢰구간(2018년도 일조량)
predict(model_w, newdata = data.frame(year = c(2018)), interval = "confidence")
#실제 관측값은 202.675로 신뢰구간 내 위치한다. 따라서 이 선형 회귀 식은 정확도가 높은 것으로 볼 수 있다.

#일조량 예측값과 신뢰구간(2019년도 일조량)
predict(model_w, newdata = data.frame(year = c(2019)), interval = "confidence")
#예측값은 214.9254
