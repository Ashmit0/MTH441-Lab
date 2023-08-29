sink(file = "lab3out.txt" , type = c("output" , "message"))
library(car)
library(MASS)
library(olsrr)
library(Matrix)
library(carData)
######p1######
# using the "linear hypothesis" function
array <- c(-1,1,-1,1,0,0,0) 
array2 <- c(-1,-1,1,1,0,1,2)
df <- data.frame( y = c(1,4,8,9,3,8,9) , 
                  x1 = array,
                  x2 = array2 ,
                  x3 = array^2 )
model <- lm( y ~ x1 + x2 + x3 , data = df )
summary( model )
A <- matrix( c(1,0,0,0,0,1,-1,0) , 
               nrow = 2 , ncol = 4 , byrow = TRUE )
linearHypothesis( model , 
                  hypothesis.matrix = A )
# using the explicit formulas for the f distribution 
ones <- rep( 1 , length( df$y ))
X <- matrix( 
  c( ones , array , array2 , array^2) , 
  nrow = length(df$y) , ncol = 4 , 
  byrow = FALSE )
Y <- matrix( c(1,4,8,9,3,8,9) , 
            nrow = length(df$y) , 
            ncol = 1 )
b1 <-solve(t(X)%*%X)%*%t(X)%*%Y
c <- matrix( c(0,0) , ncol = 1,nrow =2 )
m1 <- A%*%b1 - c 
h <- diag(dim(X)[1]) - X%*%solve(t(X)%*%X)%*%t(X)
fstat <- ((t(m1)%*%solve(A%*%solve(t(X)%*%X)%*%t(A))%*%m1)/rankMatrix(A))/((t(Y)%*%h%*%Y)/rankMatrix(h))
print( c("calculated F statistic: ",fstat[1]) ) # f statistic value 
print( 1 - pf( fstat[1] , rankMatrix(A) , rankMatrix(h))) # correspomding p value 
# t statistic for the data 
sigma <-as.vector( (t(Y)%*%h%*%Y)/(length(Y) - 4) ) 
v <- sigma*as.vector(diag(solve(t(X)%*%X)))
tstat <- b1/sqrt(v)
print(tstat)
summary(model)
######p2######
#a)LSE under the null hypothysis: b1=b2=b3=0
a <- matrix( c(0,1,0,0,0,0,1,0,0,0,0,1), 
            nrow = 3 , ncol = 4 , byrow = TRUE )
c <- matrix(c( 0 ,0,0) , nrow =3 , ncol = 1 )
b <-solve(t(X)%*%X)%*%t(X)%*%Y # unrestricted beta 
bRes <- b+solve(t(X)%*%X)%*%t(a)%*%solve(a%*%solve(t(X)%*%X)%*%t(a))%*%(c-a%*%b)
bRes <- round( bRes , digits = 8 )
#b) restricted and unristricted RSS
RSS <- t(Y - X%*%b)%*%(Y-X%*%b)
RSSres <- t(Y-X%*%bRes)%*%(Y-X%*%bRes)
#c,d) F statistic and the pvalue 
fstat <- ((RSSres - RSS)*rankMatrix(h))/((RSS)*rankMatrix(a))
fstat <- fstat[1]
print(fstat)
print(1-pf(fstat,rankMatrix(a),rankMatrix(h)))
summary(model)
######p3######
#lack of fit test
df <- data.frame( x = c(1,1,2,3.3,3.3,4,4,4,4.7,5,5.6,5.6,5.6,6,6,6.7,6.9),
                  y = c(10.84,9.30,16.35,22.88,24.35,24.56,25.86,29.16,24.59,22.25,25.90,27.20,25.61,25.45,26.56,21.03,21.46))
model <- lm( y ~ x , data = df )
lofmodel <- ols_pure_error_anova( model )
print(lofmodel)
######p4######
df <- read.csv('p1data.csv')
model <- lm( ShearStrength ~ PropellantAge , data = df )
summary(model)
alpha = 0.05 
X <- matrix( c(rep(1,length(df$Observation)) , df$PropellantAge) , nrow = length(df$Observation) , 
             ncol = 2 , byrow = FALSE )
Y <- matrix( df$ShearStrength , nrow = length(df$Observation) , 
             ncol = 1. )
b <- solve(t(X)%*%X)%*%t(X)%*%Y 
#a) Bonferroni method 
k <- 2 
n <- length(Y)
tcr <- qt( p = alpha/(2*k) , df = length(Y) - 2 , lower.tail = FALSE )
sigma <- as.vector(t(Y)%*%(diag(n)-X%*%solve(t(X)%*%X)%*%t(X))%*%Y)/( n - 2)
v<- as.vector(diag(solve(t(X)%*%X)))
tstat <- b/(sqrt(sigma*v))
summary(model)
print(tstat)
del <- tcr*sqrt(sigma*v)
print( b + del )
print(b - del )
print(c('Confidance interval for b0 = (2519.79245 , 2735.85227)'))
print(c('Confidance interval for b0 = (-44.21747 , -30.08971)'))
#b) Maximum Modulus t-Intervals
a1 <- matrix( c(1,0) , nrow = 2, ncol = 1 )
a2 <- matrix( c(0,1) , nrow = 2 , ncol = 1 )
m <- solve(t(X)%*%X)
p <- t(a2)%*%m%*%a1/sqrt(t(a1)%*%m%*%a1%*%t(a2)%*%m%*%a2) # correlation term 
print( c('coorelation term: ' , -p ) ) 
ucr <- 2.411 # from max modulous table in seber's book 
del0 <- sqrt(ucr*sigma*(t(a1)%*%m%*%a1))
del1 <- sqrt(ucr*sigma*(t(a2)%*%m%*%a2))
print(c('confidance interval for b0: (2559.216 , 2696.428)'))
print(c('confidance interval for b0: (-41.63962, -32.66756)'))
#c) Scheffe's S-Method
fcr <- qf(p = alpha , df1 = 2 , df2 = n -2 , )
del0 <- sqrt(2*fcr*sigma*(t(a1)%*%m%*%a1))
print(c('confidance interval for b0: (2574.782 , 2680.863)'))
del1 <- sqrt(2*fcr*sigma*(t(a2)%*%m%*%a2))
print(c('confidance interval for b1: (-40.62181 , -33.68537)'))
