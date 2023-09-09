# importing libraries 
library( MASS)
library(carData)
library(car)
library(dplyr)
#########p1#########

edata <- read.csv('data/Electricity_Data.csv')
head( edata )
X <- edata$X
Y <- edata$Y
plot( X , Y )
mod11 <- lm( Y ~ X , data = edata )
summary(mod11)

qqnorm(mod11$residuals)
qqline(mod11$residuals)

plot( mod11$fitted.values,rstudent(mod11))

# square fit 

tY <- sqrt(Y)
mod12 <- lm( tY ~ X )
summary(mod12) # reduced f statistic 
plot(X , tY )

qqnorm(mod12$residuals)
qqline(mod12$residuals)

plot( mod12$fitted.values , mod12$residuals )
plot( mod12$fitted.values , rstudent(mod12)) # constant variance ! 

# using the box cox method 
b <- boxcox(mod11 , plotit = TRUE )
lambda <- b$x[which.max(b$y)] # -0.545454 ~ square root trasfromation
tY <- (Y^lambda - 1)/lambda 

mod13 <- lm( tY ~ X )
plot( X , tY )
summary(mod13)

qqnorm(mod13$residuals)
qqline(mod13$residuals)

plot(mod13$fitted.values , rstandard(mod13))
plot(mod13$fitted.values , rstudent(mod13)) # fair residual plot 

#########p2#########

wdata <- read.csv('data/Wind_Mill_Data.csv')
head(wdata)
X <- wdata$X 
Y <- wdata$Y 

mod21 <- lm( Y ~ X )
summary(mod21)
plot( X , Y ) # non linear appleal 

qqnorm(mod21$residuals)
qqline(mod21$residuals)

plot(mod21$fitted.values , mod21$residuals) # curved plot incdicates non linearity 

# fit with quadratic term to account for curvature 
  # but the plot(X,Y) suggest saturation like bheaviour 

X2 <- X^2 
mod22 <- lm( Y ~ X + X2 )
summary(mod22) # very high F statistic 

plot(mod22$fitted.values,rstudent(mod22)) # no curvature

# x -> 1/x is a rather better suited to the data 

tX <- 1/X
mod23 <- lm(Y~tX)
plot(Y,tX) # linear relationship  !
summary(mod23) # very high F statistic 

plot(mod23$fitted.values,rstudent(mod23)) # uniform 0 centerd residuals 

# manual box Tidewell transformation 

plot(X,Y) # non linear bheaviour 

alpha = 1 
for( i in 1:10){
  x =  X^alpha
  mod1 = lm(Y~x) 
  b = as.numeric( mod1$coefficients[2] )
  w = x*log(X)
  mod2 = lm(Y~x+w)
  g = as.numeric( mod2$coefficients[3] )
  alpha = g/b + alpha 
  print(alpha)
}

tX <- X^alpha
mod24 <- lm( Y ~  tX  )
summary(mod24) # better F statistic
plot( Y , tX )

plot( mod24$fitted.values , rstudent(mod24)) # desired residual plot 
# box Tidewell transformationc

boxTidwell( Y ~ X )
lambda <- -0.83334 
tX <- X^lambda 

mod25 <- lm( Y ~ tX )
plot( tX , Y ) # linearized graph 

plot(mod25$fitted.values , rstandard(mod25))
plot(mod25$fitted.values , rstudent(mod25))

qqnorm(mod25$residuals)
qqline(mod25$residuals)

#########p3#########

wlsdata <- read.csv('data/Weighted_Least_Squares_Data.csv')
head(wlsdata)
X <- wlsdata$X 
Y <- wlsdata$Y 

mod31 <- lm(Y ~ X )
plot( X , Y ) # non linear 
abline(mod31)

plot(mod31$fitted.values , rstandard(mod31)) 
plot(mod31$fitted.values , rstudent(mod31)) # funnel, non normal 

qqnorm(mod31$residuals)
qqline(mod31$residuals)

# split the data set 

wlsdata <- wlsdata %>% mutate( index = cut( X , 
        breaks =c(1,2,4,6,8,10,11) , 
        labels =c(1,2,3,4,5,6) )) 
grpdata <- wlsdata %>% group_by(as.factor(index))

# mean variance and scatter plot 

sigmadata <- grpdata %>% summarise(meanX = mean(X) , meanY = mean(Y),
                      sdX = sd(X)^2 ,sdY = sd(Y)^2 )
plot( sigmadata$meanX , sigmadata$sdY ) # linear increase in sdY with respect to mX 
i <- 6
subdata <- wlsdata%>%filter(index == i )
plot( subdata$X , subdata$Y , main = i )

# fitting mode to regress over sd y and m x 

mod32 <- lm( sdY ~ meanX , data = sigmadata )
plot(sigmadata$meanX , sigmadata$sdY)
abline(mod32)

sy <- abs( predict(mod32 , newdata = data.frame( meanX = X ) ) ) 
k <- sy^(-1)
tY <- k*Y 
tX <- k*X

mod33 <- lm( tY ~ tX )
summary(mod33)
plot( tX , tY )

transdata <- data.frame('X' = tX , 'Y' = tY , 'pX' = X )
transdata <- transdata %>% mutate( index =  cut( pX , breaks =c(1,2,4,6,8,10,11) , labels =c(1,2,3,4,5,6) ))
transgrp <- transdata %>% group_by(index) %>% summarise( meanX = mean(X) , meanY = mean(Y),sdX = sd(X)^2 ,sdY = sd(Y)^2 )
print(transgrp)

plot( mod33$fitted.values , rstudent(mod33))

#########p4#########

fdata <- read.csv("data/food_sale_data.csv")
head(fdata)
plot( fdata$X , fdata$Y )

fdata <- fdata[order(fdata$X , decreasing = FALSE ) , ]
index = c( rep(1, 3),rep(2,3),rep(3,5),rep(4,6),rep(5,6),rep(6,3),rep(7,4))
fdata %>% mutate( index = index ) 
grp <- fdata %>% group_by(as.factor(index) ) %>% summarise( mX = mean(X) , sY = sd(Y)^2 )

plot( grp$mX , grp$sY ) # linear ! 
mod_ <- lm( sY ~ mX , data = grp )
abline(mod_)
summary(mod_)

w <- predict( mod_ , data.frame( mX = fdata$X ))
w <- w^(-1)

fdata <- fdata %>% mutate( tX = w*X , tY = w*Y )
plot( fdata$tX , fdata$tY )
mod__ <- lm( tY ~ tX , data = fdata )
abline( mod__ )
summary(mod__)

plot( mod__$fitted.values,rstudent(mod__)) # normal except for a few ouliars 

