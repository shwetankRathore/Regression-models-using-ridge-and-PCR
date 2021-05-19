ndata <- read.csv("ridge.csv")
options(max.print=9999)
#print(ndata)

y_1<- ndata$`y`
x1_1<-ndata$`x1`
x2_1<-ndata$`x2`
x3_1<-ndata$`x3`
x4_1<-ndata$`x4`
x5_1<-ndata$`x5`
x6_1<-ndata$`x6`

newmodel=lm(y_1~x1_1+x2_1+x3_1+x4_1+x5_1+x6_1)
summary(newmodel)
par(mfrow=c(1,1))
plot(newmodel)
res <- residuals(newmodel)
shapiro.test(res)

#vif of model
library(car)
car::vif(newmodel)

newframe <- data.frame(x1_1,x2_1,x3_1,x4_1,x5_1,x6_1)

library(lmridge)
mod = lmridge(y_1~x1_1+x2_1+x3_1+x4_1+x5_1+x6_1,newframe, K = seq(0, 0.15, 0.002))
#print(mod)
par(mfrow=c(1,1))
plot(mod, type = "ridge", abline = FALSE) #stabilizes near 0.03

kest(mod)$HKB #0.02976505
model1=lmridge(y_1~x1_1+x2_1+x3_1+x4_1+x5_1+x6_1,newframe, K=0.02976505)
summary(model1)
library(car)
vif(model1)

#hetroscedasticity
par(mfrow=c(1,1))
plot(y_1,residuals(model1))
abline(h=0)
plot(x1_1,residuals(model1))
abline(h=0)
plot(x2_1,residuals(model1))
abline(h=0)
plot(x3_1,residuals(model1))
abline(h=0)
plot(x4_1,residuals(model1))
abline(h=0)
plot(x5_1,residuals(model1))
abline(h=0)
plot(x6_1,residuals(model1))
abline(h=0)

#Glejser's test
are <- abs(residuals(model1))

summary(lm(are ~ x4_1, data = newframe)) #0.004883 ---highest
summary(lm(are ~ I(1/x4_1), data = newframe)) #0.001402
summary(lm(are ~ sqrt(x4_1), data = newframe))#0.003903
summary(lm(are ~ I(1/sqrt(x4_1)), data = newframe))#0.00213

summary(lm(are ~ x5_1, data = newframe)) #8.104e-08
summary(lm(are ~ I(1/x5_1), data = newframe)) # 0.0008338
#summary(lm(are ~ sqrt(x5_1), data = newframe)) #0.0.01749 ---highest
#summary(lm(are ~ I(1/sqrt(x5_1)), data = newframe))#0.01515

summary(lm(are ~ x6_1, data = newframe)) #0.04368
summary(lm(are ~ I(1/x6_1), data = newframe)) #0.04796
summary(lm(are ~ sqrt(x6_1), newframe)) #0.04748
summary(lm(are ~ I(1/sqrt(x6_1)), data = newframe))#0.05012 ----highest

#Goldfeld-Quandt test
library(lmtest)
gqtest(model1,fraction=1/3,order.by = ~ x4_1,alternative = c("less"))#p-value=0.09882
gqtest(model1,fraction=1/3,order.by = ~ I(1/x5_1),alternative = c("less"))#p-value=0.4269
gqtest(model1,fraction=1/3,order.by = ~ I(1/sqrt(x6_1)),alternative = c("less"))#p value=0.1265

#bruesch pagan test
bptest(model1)

tss=sum((y_1-mean(y_1))^2)
r_sq_pred= function(m) {
  1-sum((press.lmridge(m))^2)/tss
}
r_sq_pred(model1) #80.86%
