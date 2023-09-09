library(MASS)
############p1############
df <- read.csv('p2data.csv')
head(df)
model <- lm( DeliveryTime ~ CaseNumbers + Distance , 
             data = df )
summary( model )

n <- length( df$Observation )
X <- cbind( rep(1,n) , df$CaseNumbers,df$Distance)
Y <- cbind( df$DeliveryTime )
m <- solve(t(X)%*%X)
b <- m%*%t(X)%*%Y
print(b)
print(model$coefficients)

# 1. Normal Residuals 
e <- Y - X%*%b
print(e)
print(model$residuals)

# 2. Standardized Residuals 
h <- X%*%m%*%t(X)
H <- diag(n) - X%*%m%*%t(X)
MSres <- t(Y)%*%H%*%Y/(n-3)
StandRes <- e/sqrt(MSres[1])
print(StandRes)
print(rstandard(model))

# 3. Studentised Residuals 
StudRes <- e/sqrt( MSres[1]*as.matrix(1 - diag(h)))
print(StudRes)
print(studres(model))

# 4. Press Residuals 
PressRes <- e/as.matrix( 1-diag(h))
print(PressRes)

# 5. R-Student Residulas
rstudent(model)

############p1############
# Fitted Values vs R student 
Yfit = X%*%b 
print(Yfit)
print(model$fitted.values)
plot(Yfit , StudRes , xlab = 'Fitted Values',
     ylab = 'R Student' , 
     main = 'Fitted Values vs R student')

# Normal (QQ) plot of the residuals 
qqnorm(StudRes , main = "QQ plot for R student errors")
qqline(StudRes)

# Partial Regression Plots 
# 1. of x1 
x1model1 <- lm(DeliveryTime~Distance,data = df)
x1model2 <- lm(CaseNumbers ~ Distance , data = df )
plot( x1model1$fitted.values , x1model2$fitted.values , 
      ylab = 'yfit[1]' , xlab = 'xfit[1]' , 
      main = 'Partial Regression plot for X1')
# 1. of x2 
x2model1 <- lm(DeliveryTime~CaseNumbers ,data = df)
x2model2 <- lm(Distance ~ CaseNumbers , data = df )
plot( x2model1$fitted.values , x2model2$fitted.values , 
      ylab = 'yfit[2]' , xlab = 'xfit[2]' , 
      main = 'Partial Regression plot for X2')
