#양파 생산량 데이터 파악하기
yang<-read.csv(file.choose(), fileEncoding = "UTF-8")
pa<-cbind(yang,weather[,-1])
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
cor(pa$rain,pa$outcome,use="complete.obs") #상관계수가 0과 가까운 값을 가지므로 양파 생산량과 강수량은 유의미한 관계를 가진다고 할 수 없다.
cor(pa$sun,pa$outcome,use="complete.obs") #상관계수 0과 가까운 값을 가지므로 양파 생산량과 일조량은 유의미한 관계를 가진다고 할 수 없다.
#즉, 마늘 생산량은 날씨 변수 중 강수량과 일조량 모두 영향을 받지 않는다. 따라서 날씨 변수를 통해 양파 생산량을 예측할 수 없다.


#고추 생산량 데이터 파악하기
gogo<-read.csv(file.choose(), fileEncoding = "UTF-8")
go<-cbind(gogo,weather[,-1])
go
View(go)
hist(go$outcome)
qqnorm(go$outcome)
qqline(go$outcome)
#고추 생산량 데이터는 정규분포를 따른다고 볼 수 있다.

#고추 생산량- 강수량과 일조량 다중회귀
gmodel<-lm(outcome~sun+rain, data=go)
summary(gmodel)
#변수 별 상관계수 구하기
cor(go$rain,go$outcome,use="complete.obs") #상관계수가 0과 가까운 값을 가지므로 고추 생산량과 강수량은 유의미한 관계를 가진다고 할 수 없다.
cor(go$sun,go$outcome,use="complete.obs") #상관계수 0과 가까운 값을 가지므로 고추 생산량과 일조량은 유의미한 관계를 가진다고 할 수 없다.
#즉, 고추 생산량은 날씨 변수 중 강수량과 일조량 모두 영향을 받지 않는다. 따라서 날씨 변수를 통해 고추 생산량을 예측할 수 없다.
