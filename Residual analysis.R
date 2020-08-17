library("MASS")
data("trees")
trees

# Predicting Trees Volume Based on its Height and Girth.

## MODEL 1
## Label -> Volume
## Features -> Height


lin_reg = lm(Volume ~ Height, data = trees)
lin_reg

summary(lin_reg)

trees$vol_pred = predict(lin_reg)
trees$residual = trees$Volume - trees$vol_pred

trees

plot(trees$vol_pred, trees$residual, pch=21, bg="red", col="red")
abline(0,0)


hist(trees$residual)

std_res = rstandard(lin_reg)
qqnorm(std_res,
       ylab="Standardized Residuals", 
       xlab="Normal Scores", 
       main="Trees Data")

qqline(std_res)


##--------------------------------------------------------------------------------##

## MODEL 2
## Label -> Volume
## Features -> Girth

library("MASS")
data("trees")
trees


lin_reg = lm(Volume ~ Girth, data = trees)
lin_reg

summary(lin_reg)

trees$vol_pred = predict(lin_reg)
trees$residual = trees$Volume - trees$vol_pred

trees

plot(trees$vol_pred, trees$residual, pch=21, bg="red", col="red")
abline(0,0)


hist(trees$residual)

std_res = rstandard(lin_reg)
qqnorm(std_res,
       ylab="Standardized Residuals", 
       xlab="Normal Scores", 
       main="Trees Data")

qqline(std_res)

##--------------------------------------------------------------------------------##

## MODEL 3
## Label -> Volume
## Features -> Girth + Height


library("MASS")
data("trees")
trees


lin_reg = lm(Volume ~., data = trees)
lin_reg

summary(lin_reg)

trees$vol_pred = predict(lin_reg)
trees$residual = trees$Volume - trees$vol_pred

trees

plot(trees$vol_pred, trees$residual, pch=21, bg="red", col="red")
abline(0,0)


hist(trees$residual)

std_res = rstandard(lin_reg)
qqnorm(std_res,
       ylab="Standardized Residuals", 
       xlab="Normal Scores", 
       main="Trees Data")

qqline(std_res)

#2) Residual are
#Residuals are randomly distributed around regression line
#Residuals follow normal distribution
#Residuals are Homoscedastic.
#Linear model is valid.



