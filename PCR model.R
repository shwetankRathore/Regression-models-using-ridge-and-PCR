rdata <- read.csv("ridge.csv")

xnew=cbind(rdata$x1,rdata$x2,rdata$x3,rdata$x4,rdata$x5,rdata$x6)

pcr=prcomp(xnew,center = T,scale=T)
plot(pcr,type='l')
summary(pcr)

#using pcr model
model2=lm(rdata$y~PC1+PC2+PC3+PC4,data=data.frame(pcr$x))
summary(model2)
car::vif(model2)

PC1=pcr$x[,1]
PC2=pcr$x[,2]
PC3=pcr$x[,3]
PC4=pcr$x[,4]

#heteroscedasticity
#After PCR
library(MASS)
par(mfrow=c(1,1))
plot(model2)

par(mfrow=c(1,1))
plot(PC1,stdres(model2))
abline(h=0)
plot(PC2,stdres(model2))
abline(h=0)
plot(PC3,stdres(model2))
abline(h=0)
plot(PC4,stdres(model2))
abline(h=0)

#hetroscedasticity checking
#bruesch pagan test
bptest(model2)

#PRESS
PRESS <- function(linear.model) {
  #' calculate the predictive residuals
  pr <- residuals(linear.model)/(1-lm.influence(linear.model)$hat)
  #' calculate the PRESS
  PRESS <- sum(pr^2)
  
  return(PRESS)
}

#R-sq predicted
pred_r_squared <- function(linear.model) {
  #' Use anova() to get the sum of squares for the linear model
  lm.anova <- anova(linear.model)
  #' Calculate the total sum of squares
  tss <- sum(lm.anova$'Sum Sq')
  # Calculate the predictive R^2
  pred.r.squared <- 1-PRESS(linear.model)/(tss)
  
  return(pred.r.squared)
}

pred_r_squared(model2)#81.1%
