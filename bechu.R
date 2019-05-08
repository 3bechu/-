#배추 다중회귀
chu<-read.csv(file.choose(), fileEncoding = "UTF-8")
View(chu)
be<-cbind(chu, weather)
View(be)
hist(be$outcome)
qqnorm(be$outcome)
qqline(be$outcome)
bmodel<-lm(outcome~sun+rain, data=be)
summary(bmodel)

#배추 강수량 단순회귀
cor(be$outcome, be$rain)
fit=lm(outcome~rain, data=be)
a=fit$coef[1]; b=fit$coef[2]
a;b
plot(be$rain, be$outcome, col="red")
abline(a,b)

#배추 일조량 단순회귀
cor(be$outcome, be$sun)
fit=lm(outcome~sun, data=be)
a=fit$coef[1]; b=fit$coef[2]
a;b
plot(be$sun, be$outcome, col="red")
abline(a,b)

#무 다중회귀
mu<-read.csv(file.choose(), fileEncoding = "UTF-8")
weather<-read.csv(file.choose(), fileEncoding = "UTF-8")
View(weather)
weather<-weather[,-1]
View(mu)
moo<-cbind(mu,weather)
moo
hist(moo$outcome)
qqnorm(moo$outcome)
qqline(moo$outcome)
mmodel<-lm(outcome~rain+sun, data=moo)
summary(mmodel)

#무 강수량 단순회귀
library(car)
cor(moo$outcome, moo$rain)
fit=lm(outcome~rain, data=moo)
a=fit$coef[1]; b=fit$coef[2]
a;b
plot(moo$rain, moo$outcome, col="red")
abline(a,b)


#무 일조량 단순회귀
cor(moo$outcome, moo$sun)
fit=lm(outcome~sun, data=moo)
a=fit$coef[1]; b=fit$coef[2]
a;b
plot(moo$sun, moo$outcome, col="red")
abline(a,b)

#양파 다중회귀
yang<-read.csv(file.choose(), fileEncoding = "UTF-8")
pa<-cbind(yang,weather)
View(pa)
hist(pa$outcome)
qqnorm(pa$outcome)
qqline(pa$outcome)
ymodel<-lm(outcome~sun+rain, data=pa)
summary(ymodel)

#마늘 다중회귀
lic<-read.csv(file.choose(), fileEncoding = "UTF-8")
View(lic)
ma<-cbind(lic,weather)
View(ma)
hist(ma$outcome)
qqnorm(ma$outcome)
qqline(ma$outcome)
gmodel<-lm(outcome~rain+sun, data=ma)
summary(gmodel)

#마늘 강수량 단순회귀
cor(ma$outcome, ma$rain)
fit=lm(outcome~rain, data=ma)
a=fit$coef[1]; b=fit$coef[2]
a;b
plot(ma$rain, ma$outcome)
abline(a,b)

#마늘 일조량 단순회귀
cor(ma$outcome, ma$sun)
fit=lm(outcome~sun, data=ma)
a=fit$coef[1];b=fit$coefficients[2]
a;b
plot(ma$sun, ma$outcome)
abline(a,b)

