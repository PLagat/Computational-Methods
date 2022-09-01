
#Simulation of Brownian Motion
n = 1000 #no of observed sample paths
t = 1 #time horizon, daily
t.s = seq(0,t,length = n+1) #time step
dt = t/n #change in time
Bt = sqrt(dt)*cumsum(rnorm((n+1), mean=0, sd=1))
plot(t.s, Bt, type="l",col="blue",
     xlab= "Time", ylab="Simulated Brownian Motion")

abline(h=0,lty=2,col="red") #to get a straight line


#simulation of Geometric Brownian Motion
gbm = function(n,t,S0,mu,sigma, nRepl){
  t=1 #time horizon
  t.s = seq(0,t,length=n+1) #time step
  dt = t/n #change in time
  Bt = sqrt(dt)*cumsum(rnorm((n+1),0,1))
  St = S0*exp((mu-sigma^2/2)*t.s + sigma*Bt) #the exact solution
  St #display result of GBM
}
nRepl <- 10 #no of replications
paths = gbm(n=50, S0=1,mu=2,sigma=1,nRepl=10)
plot(paths, type="l", col= cols, lty = 1,
     xlab="Time", main = "Simulation of GBM sample paths")


#Euler-Maruyama scheme
time <- seq(from=0, to=t, by=dt) #time moments in which we simulate the process
length(time) #it should be N+1
S0 <- 1
X_EM <- numeric(n+1) #vector of zeros, N+1 elements
X_EM[1] <- S0 #first element of X_EM is X0. with the for loop we find the other N elements

for(i in 1:n+1){
  X_EM[i] <- X_EM[i-1] + mu*X_EM[i-1]*dt + sigma*X_EM[i-1]*dW[i-1]
}

#plot X against time
plot(time, X_EM, type = "l", main = "GBM path with Euler-Maruyama scheme", 
     xlab = expression("t"[i]), ylab = expression("W"[t[i]]))

#plot W against time
matplot(time, cbind(X_analytic, X_EM), type = "l", main = "GBM", 
        xlab = expression("t"[i]), ylab = expression("X"[t[i]]))













