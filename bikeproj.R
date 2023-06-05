



#bikes cleaned csv\
install.packages("colorspace")
install.packages("ggplot2")
library(ggplot2)
install.packages("dunn.test")
library(dunn.test)
#installing required packages

#pulling the data
bdata<-read.csv(file.choose())

#first glance analysis
summary(bdata)

#checking linear model
model1<-lm(bdata$yr,bdata$casual)

par(mfrow=c(1,4))
by(bdata$cnt, bdata$season,qqnorm)

t.test(bdata$casual,bdata$registered)





by(bdata$cnt, bdata$season,shapiro.test)
#not normal
by(bdata$casual, bdata$weathersit,shapiro.test)
#not normal
by(bdata$casual,bdata$holiday,shapiro.test)
#not normal

by(bdata$cnt,bdata$season,shapiro.test)

shapiro.test(bdata$cnt)

bdata$logcnt<-log(bdata$cnt)

shapiro.test(bdata$logcnt)

par(mfrow=c(1,4))
by(bdata$casual,bdata$season,hist)


bdata$logtemp<-log(bdata$temp)

by(bdata$logtemp,bdata$season,shapiro.test)

kruskal.test(bdata$temp~bdata$cnt)
#some relation

dunn.test(bdata$cnt,bdata$season)


help("dunn.test")

par(mfrow=c(1,1))
qqnorm(bdata$atemp)

shapiro.test(bdata$atemp)

#histo of holiday abd non holiday
par(mfrow=c(1,2))
by(bdata$casual,bdata$holiday,hist)

par(mfrow=c())
plot(bdata$atemp,bdata$cnt, xlab="Feeling Temp", ylab = "Bike rental count",main="Temperature VS Total bike rental count")
abline(lm2,col="red")

par(mfrow=c(1,2))
by(bdata$cnt,bdata$weathersit,boxplot)

plot(bdata$windspeed,bdata$cnt, xlab="Feeling Temp", ylab = "Bike rental count",main="Temperature VS Total bike rental count")
abline(lm6,col="red")

plot(bdata$hum,bdata$cnt, xlab="Feeling Temp", ylab = "Bike rental count",main="Temperature VS Total bike rental count")
abline(lm7,col="red")

by(bdata$cnt,bdata$holiday,hist)

by(bdata$cnt,bdata$workingday,hist)

#season and total count
lm1<-lm(bdata$cnt~bdata$season,data = bdata)
summary(lm1)
#normal residuals
qqnorm(lm1$residuals)

#feeling temp
lm2<-lm(bdata$cnt~bdata$atemp,data = bdata)
summary(lm2)
#normal residuals
qqnorm(lm1$residuals)

#holiday or not
lm3<-lm(bdata$cnt~bdata$holiday,data = bdata)
summary(lm3)


#working day or not
lm4<-lm(bdata$cnt~bdata$workingday,data = bdata)
summary(lm4)

#weather and total bikes
lm5<-lm(bdata$cnt~bdata$weathersit,data=bdata)
summary(lm5)


#windspeed and total  count
lm6<-lm(bdata$cnt~bdata$windspeed,data=bdata)
summary(lm6)

#humididty and total count
lm7<-lm(bdata$cnt~bdata$hum,data=bdata)
summary(lm7)

cor.test(bdata$temp,bdata$atemp)

#checking usage on working day by casual/ registered 
lm21<-lm(bdata$registered~bdata$workingday,data=bdata)
summary(lm21)

lm22<-lm(bdata$casual~bdata$workingday,data=bdata)
summary(lm22)


#attached only which are significxant

#multiple regression
#season,feelingtemp, weather situation, windspeed, humidity
lm_final<-lm(bdata$cnt~bdata$season+bdata$atemp+bdata$weathersit+bdata$windspeed+bdata$hum)
summary(lm_final)

par(mfrow=c(1,1))
plot(lm_final,las=1)



