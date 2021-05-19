rdata <- read.csv("happy.csv")
print(rdata)

y<- rdata$`Ladder.score`
x1<-rdata$`Logged.GDP.per.capita`
x2<-rdata$`Social.support`
x3<-rdata$`Healthy.life.expectancy`
x4<-rdata$`Freedom.to.make.life.choices`
x5<-rdata$`Generosity`
x6<-rdata$`Perceptions.of.corruption`

x <- cbind(x1,x2,x3,x4,x5,x6)

frame <- data.frame(y,x1,x2,x3,x4,x5,x6)
print(frame)
pairs(frame)

#checking skewnwss of independent variables
par(mfrow=c(2,2))

boxplot(x1)
plot(density(x1))
polygon(density(x1),col= "orange")

boxplot(x2)
plot(density(x2))
polygon(density(x2),col= "orange")

boxplot(x3)
plot(density(x3))
polygon(density(x3),col= "orange")

boxplot(x4)
plot(density(x4))
polygon(density(x4),col= "orange")

boxplot(x5)
plot(density(x5))
polygon(density(x5),col= "orange")

boxplot(x6)
plot(density(x6))
polygon(density(x6),col= "orange")

library(e1071)
skewness(x1)
skewness(x2)
skewness(x3)
skewness(x4)
skewness(x5)
skewness(x6)

#initial model
model=lm(y~x1+x2+x3+x4+x5+x6)
summary(model)

par(mfrow=c(2,2))
plot(model)
res <- residuals(model)

#correlation plot
par(mfrow=c(1,1))
library(corrplot)
corrplot(cor(frame),method="number")

#vif of model
library(car)
car::vif(model)

#checking outliers
library(olsrr)
ols_plot_cooksd_bar(model)


#removing outliers
frame2 <- frame[-c(13,16,41,53,56,63,75,94,115,120,132,137,147),]
print(frame2)
write.csv(frame2,file = "ridge.csv")

