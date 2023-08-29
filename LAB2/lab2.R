sink("lab2out.txt")
library(MASS)
library(Matrix)
library(matlib)
###p1### 
p1data <- read.csv("p1data.csv")
head(p1data)
plot( p1data$PropellantAge , p1data$ShearStrength , 
      xlab = "Age of Propellant" , 
      ylab = "Shear Strength",
      main = "Age vs Shear Strength scatter plot")
# strong statistical dependance of shear strength on the age! 
n <- length( p1data$Observation)
ones <- rep( 1 , n)
p <- 2 
X <- matrix( c( ones , p1data$PropellantAge) , 
             nrow = n , ncol = 2 , byrow = FALSE )
Y <- matrix( p1data$ShearStrength , nrow = n , ncol = 1 )
p1beta <- solve( t(X)%*%X )%*%t(X)%*%Y 
print( p1beta )
p1model <- lm( p1data$ShearStrength ~ p1data$PropellantAge )
summary( p1model )
abline( p1model )


###p3### 
H <- diag(n) - X%*%solve(t(X)%*%X)%*%t(X)
sigma <-( t(Y)%*%H%*%Y )/(n-p)
summary(p1model)
print(sqrt(sigma))
sigmamat <- matrix( rep(sigma , p*p) , p )
varbeta <- diag( sigmamat*solve( t(X)%*%X ) )
t <- p1beta * ( varbeta^(-1))
print( t )


###p2### 
p2data <- read.csv("p2data.csv")
p2data <- subset( p2data , select = -Observation)
plot( p2data )
n <- length(p2data$DeliveryTime)
ones <- rep( 1 , n )
p <- 3 
X <- matrix( c( ones , p2data$CaseNumbers , p2data$Distance) , 
             nrow = n , ncol = p , byrow =  FALSE )
Y <- matrix( p2data$DeliveryTime , nrow = n , ncol = 1 , byrow = FALSE )
p2beta <- solve( t(X)%*%X )%*%t(X)%*%Y 
p2model <- lm( p2data$DeliveryTime ~ p2data$CaseNumbers + p2data$Distance)
summary(p2model)
print( p2beta )


###p4### 
z1 <- rnorm(5000,mean=0,sd=1)
z2 <- rnorm(5000,mean=0,sd=1)
z3 <- rnorm(5000,mean=0,sd=1)
chi <-z1^2 + z2^2 + z3^2 
hist( chi )
print( mean( chi ))
print(mean(chi) - 3)
print( sd( chi )^2)
print(  sd(chi)^2 - 6 )

###p5### 
X <- matrix( runif(40,1,10) , nrow = 8 , ncol = 5 )
P <- X %*% solve( t(X)%*%X ) %*% t(X)
round( P%*%P - P , 14 )
u <- rep( 0 , 8 ) 
I <- diag( 8 )
Y <-  mvrnorm( n = 5000 , mu = u , Sigma = I  ) 
u <- diag( Y%*%P%*%t(Y) )
hist( u )
print( mean( u ))
print( sd( u )^2 )
print( (mean(u) - 5)*20  ) 
print( (sd(u)^2 - 10)*10 )


###p6### 
X <- matrix( round( runif( 40 , 0 , 100)) , nrow = 8 , ncol = 5 )
PX1 <- X %*% solve( t(X)%*%X ) %*% t(X)
PX2 <- diag(8) - PX1 
Y <-  mvrnorm( n = 5000 , mu = rep( 0 , 8 )  , Sigma = diag( 8 ) ) 
f2 <- R(PX2)
u1 <- diag( Y%*%PX1%*%t(Y) )/R(PX1)
u2 <- diag( Y%*%PX2%*%t(Y) )/R(PX2)
f <- u2*( u1^(-1) )
hist( f )
d1 <- 3
d2 <- 5
tmean <- d2/( d2 -2 )
tvar <- ( 2 * (d2^2) * ( d1 + d2 - 2 ))/( d1* (d2-2)^2 * (d2 - 4))
amean <- mean( f )
avar <- sd(f)^2  
print( amean )
print( tmean )
print( avar )
print( tvar )
print(((amean - tmean)/tmean)*100)
print(((avar-tvar)/tvar)*100)
print( f2 )