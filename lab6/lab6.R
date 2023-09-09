library(MASS)
library(car)
library(carData)
########p1########

dtdata <- read.csv("Delivery_Time.csv")
head(dtdata)

Y <- matrix( dtdata$DeliveryTime , ncol = 1)
n <- length(Y) 
p <- 3
X <- matrix(c(rep(1,n),dtdata$CaseNumbers,dtdata$Distance),ncol=3,nrow=n)

# leverage points 

H <- X%*%solve(t(X)%*%X)%*%t(X)

leverage_obs <- NULL 
for( i in 1:n){
  if( H[i,i] > p/n ){
    leverage_obs <- cbind( leverage_obs , i )
  }
}
print(as.vector( leverage_obs) ) 

mod11 <- lm(DeliveryTime ~ CaseNumbers + Distance , data = dtdata )
summary(mod11)
leveragePlots(mod11)

# cooks distance 

cd <- cooks.distance(mod11)
f <- qf( 0.5 , p , n-p )
cdout <- NULL 
for( i in 1:n ){ 
  if( cd[i] > f ){ 
    cdout <- cbind( cdout , i )
    }  
}
print(cdout)

# DFBETAS 

dfbetas <- as.matrix( dfbetas( mod11 ))
dfbout <- NULL 
for( i in 1:n ){ 
  for( j in 1:p ){ 
    if( dfbetas[i,j] > 2/sqrt(n)){
      dfbout <- cbind( dfbout , i )
    }  
  }
}
print(as.vector( dfbout) ) 

# DFFITS 

dffits <- dffits(mod11)
dffout <- NULL 
for( i in 1:n){
  if( dffits[i] > 2*sqrt(p/n)){
    dffout <- cbind( dffout , i )
  }
}
print(dffout)

# COVRATIO 

covratio <- covratio(mod11)
covout <- NULL 
for( i in 1:n ){ 
  if( covratio[i] > 1 + 3*p/n | covratio[i] < 1 - 3*p/n ){
    covout <- cbind( covout , i )
  }
}
print(covout)
