#QUESTION 1
#(a) Doolittle Algorithm
is.square.matrix <- function( x )
{
  ### determines if the given matrix is a square matrix
  ### arguments
  ### x = a matrix object
  ###
  if ( !is.matrix( x ) )
    stop( "argument x is not a matrix" )
  return( nrow(x) == ncol(x) )
}


lu.decomposition <- function( x )
{
  ### This function performs an LU decomposition of the given square matrix argument
  ### the results are returned in a list of named components.  The Doolittle decomposition
  ### method is used to obtain the lower and upper triangular matrices
  ###
  ### arguments
  ### x = a square numeric matrix
  ###
  if ( !is.square.matrix( x ) )
    stop( "argument x is not a square matrix" )
  if ( !is.numeric( x ) )
    stop( "argument x is not numeric" )
  n <- nrow( x )
  L <- matrix( 0, nrow=n, ncol=n )
  U <- matrix( 0, nrow=n, ncol=n )
  diag( L ) <- rep( 1, n )
  for ( i in 1:n ) {
    ip1 <- i + 1
    im1 <- i - 1
    for ( j in 1:n ) {
      U[i,j] <- x[i,j]
      if ( im1 > 0 ) {
        for ( k in 1:im1 ) {
          U[i,j] <- U[i,j] - L[i,k] * U[k,j]
        }
      }
    }
    if ( ip1 <= n ) {
      for ( j in ip1:n ) {
        L[j,i] <- x[j,i]
        if ( im1 > 0 ) {
          for ( k in 1:im1 ) {
            L[j,i] <- L[j,i] - L[j,k] * U[k,i]
          }
        }
        if ( U[i,i] == 0 )
          stop( "argument x is a singular matrix" )
        L[j,i] <- L[j,i] / U[i,i]
      }    
    }
  }
  result <- list( L=L, U=U )
  return( result )
}

#Examples of doolittle algorithm in play
#example 1
{A <- matrix( c ( 1, 2, 2, 1 ), nrow=2, byrow=TRUE)
luA <- lu.decomposition(A)
L <- luA$L
U <- luA$U
print( L )
print( U )
print( L %*% U )
print( A )}

#example 2
{B <- matrix( c( 2, -3, 1, 1, 1, -1, -1, 1, -1 ), nrow=3, byrow=TRUE )
luB <- lu.decomposition( B )
L <- luB$L
U <- luB$U
print( L )
print( U )
print( L %*% U )
print( B )}

#example 3
{C <- matrix( c( -2.175, 1, 0, 1, -2.150, 1, 0, 1, -2.125 ), nrow=3, byrow=TRUE )
luC <- lu.decomposition( C )
L <- luC$L
U <- luC$U
print( L ) #gives Lower Triangular matrix
print( U ) #gives Upper Triangular matrix
print( L %*% U )
print( C )}

#{proof; 
e <- c(-1.625, 0.5, 1.625);
solve(C,e)  #[1]  0.5520137 -0.4243701 -0.9644095 
#}


#QUESTION 2
{#box muller
mullerbt <- function(){
  n <- 10^4
  samples <- matrix(ncol=2,nrow=n)
  for (i in 1:n){
    u1 <- runif(1)
    u2 <- runif(1)
    R <- sqrt(-2*log(1-u1))
    theta <- 2*pi*u2
    X <- R*cos(theta)
    Y <- R*sin(theta)
    samples[i,1] <- X
    samples[i,2] <- Y
  }
  norm01 <- rnorm(n)
  label <- rep(c("x","y","normal"),n)
  value <- c(samples[,1],samples[,2],norm01)
  df <- data.frame(value,label)
  library(ggplot2)
  plt <- ggplot(df, aes(x=value, color=label, fill=label)) + geom_histogram(aes(y=..density..), bins = 60, position="identity", alpha=0.3) + labs(x="Value", y="Density") + theme_bw()
  print(plt)
}
mullerbt()

#alternatively, generating normal
# import the library to test the normality of the distribution
library(nortest)

size = 100000

u = runif(size)
v = runif(size)

x=rep(0,size)
y=rep(0,size)

for (i in 1:size){
  x[i] = sqrt(-2*log(u[i]))*cos(2*pi*v[i])
  y[i] = sqrt(-2*log(u[i]))*sin(2*pi*v[i])
}

#a test for normality
lillie.test(c(x,y))

#plot the estimation of the density
plot(density(c(x,y)))
}

#QUESTION 3
{
  
  finite.diff <- function( func, .x, indices=1:length(.x), stepSize=sqrt( .Machine$double.eps ), ... ) 
    {
     # if we evaluate at values close to 0, we need a different step size
    stepSizeVec <- pmax( abs(.x), 1 ) * stepSize
    
    fx <- func( .x, ... )
    approx.gradf.index <- function(i, .x, func, fx, stepSizeVec, ...) {
      x_prime <- .x
      x_prime[i] <- .x[i] + stepSizeVec[i]
      stepSizeVec[i] <- x_prime[i] - .x[i]
      fx_prime <- func( x_prime, ... )
      return( ( fx_prime - fx )/stepSizeVec[i] )
    }
    grad_fx <- sapply (indices, approx.gradf.index, .x=.x, func=func, fx=fx, stepSizeVec=stepSizeVec, ... )
    
    return( grad_fx )
  }
}

# example with correct gradient
{f <- function( x, a ) {
  return( sum( ( x - a )^2 ) )
}

f_grad <- function( x, a ) {
  return( 2*( x - a ) )
}

check.derivatives( .x=1:10, func=f, func_grad=f_grad,
                   check_derivatives_print='none', a=runif(10) )

# example with incorrect gradient
f_grad <- function( x, a ) {
  return( 2*( x - a ) + c(0,.1,rep(0,8)) )
}

check.derivatives( .x=1:10, func=f, func_grad=f_grad,
                   check_derivatives_print='errors', a=runif(10) )

# example with incorrect gradient of vector-valued function
g <- function( x, a ) {
  return( c( sum(x-a), sum( (x-a)^2 ) ) )
}

g_grad <- function( x, a ) {
  return( rbind( rep(1,length(x)) + c(0,.01,rep(0,8)), 2*(x-a) + c(0,.1,rep(0,8)) ) )
}

check.derivatives( .x=1:10, func=g, func_grad=g_grad,
                   check_derivatives_print='all', a=runif(10) )}

###QUESTION 4
{
A <- matrix(c(1, 0, 0,
              0.6, 1, 0,
              0.5, -0.7, 1), nrow=3, byrow=TRUE)
b <- c(2-0.5, -1, 2)
}