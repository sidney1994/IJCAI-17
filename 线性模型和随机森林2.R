data<-read.csv("feature1.csv",header=T)
data2<-read.csv("feature2.csv",header=T)
data[,38:44]<-lapply(data[,38:44],as.numeric)
data2[,38:44]<-lapply(data2[,38:44],as.numeric)

summary(data)
summary(data2)
head(data2)
head(data)
data$shop_level<-as.factor(data$shop_level)
#per_pay+shop_level+cate1+avermon+avertue+averwed+averthu+averfri+aversat+aversun+lastue+lastwed+lasthu+lastfri+lastsat+lastsun+lastmon+last_one_week_sum+last_two_week_sum+last_three_week_sum+total_sum+last_one_week+last_two_week+last_three_week+total_sum.1+viewsum+lasttwoweekview+lasttueview+lastwedview+lastthuwiew+lastfriview+lastsatview+lastsunview+lastmonview+centercity+centercity1+avertue1+averwed1+averthu1+averfri1+aversat1+aversun1+avermon1
#avermon+avertue+averwed+averthu+averfri+aversat+aversun+per_pay+shop_level+cate1+lastmon+lastue+lastwed+lasthu+lastfri+lastsat+lastsun-1
data$cate1<-as.factor(data$cate1)
data2$shop_level<-as.factor(data2$shop_level)
data2$cate1<-as.factor(data2$cate1)
lm.sol1<-lm(Mon~avertue+averwed+aversun+shop_level+cate1+lastmon+lastue+lastwed+lasthu+lastsat-1,data=data)
summary(lm.sol1)
lm.pred1<-predict(lm.sol1,data2)
data.frame(lm.pred1)

lm.sol2<-lm(Tus~avermon+avertue+averwed+aversat+lastmon+lastue+lastwed+lasthu+lastsat-1,data=data)
summary(lm.sol2)
lm.pred2<-predict(lm.sol2,data2)
data.frame(lm.pred2)

lm.sol3<-lm(Wed~avermon+averwed+averthu+averfri+aversun+lastmon+lastue+lastwed+lasthu+lastfri+lastsat+lastsun-1,data=data)
summary(lm.sol3)
lm.pred3<-predict(lm.sol3,data2)
data.frame(lm.pred3)

lm.sol4<-lm(Thu~avermon+avertue+averwed+averthu+averfri+aversat+aversun+lastmon+lastue+lastwed+lasthu+lastsat-1,data=data)
summary(lm.sol4)
lm.pred4<-predict(lm.sol4,data2)
data.frame(lm.pred4)

lm.sol5<-lm(Fri~avermon+avertue+averwed+averfri+aversat+shop_level+lastmon+lastue+lastwed+lasthu+lastfri+lastsat-1,data=data)
summary(lm.sol5)
lm.pred5<-predict(lm.sol5,data2)
data.frame(lm.pred5)


lm.sol6<-lm(Sat~avermon+averthu+averfri+aversat+aversun+per_pay+shop_level+cate1+lastmon+lastue+lastwed+lastfri+lastsat+lastsun-1,data=data)
summary(lm.sol6)
lm.pred6<-predict(lm.sol6,data2)
data.frame(lm.pred6)
lm.sol7<-lm(Sun~avermon+avertue+aversat+aversun+per_pay+cate1+lastmon+lastue+lastfri+lastsat+lastsun-1,data=data)
summary(lm.sol7)
lm.pred7<-predict(lm.sol7,data2)
data.frame(lm.pred7)
#########################
#广义线性模型
library(randomForest)
?randomForest
glm.sol<-randomForest(Mon~avermon+avertus+averwed+averthu+averfri+aversat+aversun+per_pay+shop_level+cate1,data=data,importance=TRUE ,proximity=TRUE)
summary(glm.sol)
pre<-predict(glm.sol, data=data)
data.frame(pre)
###################################################随机森林
library(randomForest)
rf1<-randomForest(Tus~avermon+avertue+averwed+averthu+averfri+aversat+aversun+per_pay+shop_level+cate1++lastmon+lastue+lastwed+lasthu+lastfri+lastsat+lastsun,na.action=na.omit,data=data)
data.frame(predict(rf1,data2))
rf2<-randomForest(Wed~avermon+avertue+averwed+averthu+averfri+aversat+aversun+per_pay+shop_level+cate1++lastmon+lastue+lastwed+lasthu+lastfri+lastsat+lastsun,na.action=na.omit,data=data)
data.frame(predict(rf2,data2))
rf3<-randomForest(Thu~avermon+avertue+averwed+averthu+averfri+aversat+aversun+per_pay+shop_level+cate1++lastmon+lastue+lastwed+lasthu+lastfri+lastsat+lastsun,na.action=na.omit,data=data)
data.frame(predict(rf3,data2))
rf4<-randomForest(Fri~avermon+avertue+averwed+averthu+averfri+aversat+aversun+per_pay+shop_level+cate1++lastmon+lastue+lastwed+lasthu+lastfri+lastsat+lastsun,na.action=na.omit,data=data)
data.frame(predict(rf4,data2))
rf5<-randomForest(Sat~avermon+avertue+averwed+averthu+averfri+aversat+aversun+per_pay+shop_level+cate1++lastmon+lastue+lastwed+lasthu+lastfri+lastsat+lastsun,na.action=na.omit,data=data)
data.frame(predict(rf5,data2))
rf6<-randomForest(Sun~avermon+avertue+averwed+averthu+averfri+aversat+aversun+per_pay+shop_level+cate1++lastmon+lastue+lastwed+lasthu+lastfri+lastsat+lastsun,na.action=na.omit,data=data)
data.frame(predict(rf6,data2))

rf7<-randomForest(Mon~avermon+avertue+averwed+averthu+averfri+aversat+aversun+per_pay+shop_level+cate1++lastmon+lastue+lastwed+lasthu+lastfri+lastsat+lastsun,na.action=na.omit,data=data)
data.frame(predict(rf7,data2))

##############################################################bagging
library(ipred)
install.packages("boost", dependencies=TRUE) 
bagging1 <- bagging(Tus~avermon+avertue+averwed+averthu+averfri+aversat+aversun+per_pay+shop_level+cate1++lastmon+lastue+lastwed+lasthu+lastfri+lastsat+lastsun,na.action=na.omit,data=data)
baggingpred1<-data.frame(predict(bagging1,data2))
library(adabag)
library(rpart)
library(mlbench)
library(careet)
li
install.packages("careet")
adaboost1<-boosting(Tus~avermon+avertue+averwed+averthu+averfri+aversat+aversun+per_pay+shop_level+cate1++lastmon+lastue+lastwed+lasthu+lastfri+lastsat+lastsun,data=data)








######################逐步回归
step1<-lm(Tus~per_pay+shop_level+cate1+avermon+avertue+averwed+averthu+averfri+aversat+aversun+lastue+lastwed+lasthu+lastfri+lastsat+lastsun+lastmon+last_one_week_sum+last_two_week_sum+last_three_week_sum+total_sum+last_one_week+last_two_week+last_three_week+total_sum.1+viewsum+lasttwoweekview+lasttueview+lastwedview+lastthuwiew+lastfriview+lastsatview+lastsunview+lastmonview+centercity+centercity1+avertue1+averwed1+averthu1+averfri1+aversat1+aversun1+avermon1-1,data=data)
tstep1<-step(step1)
summary(tstep1)
plot(residuals(tstep1))


step2<-lm(Wed~per_pay+shop_level+cate1+avermon+avertue+averwed+averthu+averfri+aversat+aversun+lastue+lastwed+lasthu+lastfri+lastsat+lastsun+lastmon+last_one_week_sum+last_two_week_sum+last_three_week_sum+total_sum+last_one_week+last_two_week+last_three_week+total_sum.1+viewsum+lasttwoweekview+lasttueview+lastwedview+lastthuwiew+lastfriview+lastsatview+lastsunview+lastmonview+centercity+centercity1+avertue1+averwed1+averthu1+averfri1+aversat1+aversun1+avermon1-1,data=data)
tstep2<-step(step2)
summary(tstep2)
drop(tstep2)
plot(tstep2)
