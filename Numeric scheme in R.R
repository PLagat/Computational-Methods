GeometricBrownian<-function()
{
  paths<-10 #no of replications
  t<-1      #time horizon/count
  n<-1000   #no of observed sample paths
  dt<-t/n    #change in time
  mu<-2
  sigma<-1
  sample<-matrix(0,nrow=(n+1),ncol=paths)
  for(i in 1:paths) {
    sample[1,i]<-100
    for(j in 2:(n+1)) {
      sample[j,i]<-sample[j-1,i]*exp(dt*(mu-((sigma)^2)/2)+((dt)^.5)*rnorm(1)*sigma) #Expression for Geometric Brownian Motion
    }
  }	
 
  matplot(sample,main="Sample paths for Geometric Brownian Motion",xlab="Time",ylab="Path",type="l")
}
GeometricBrownian() 


#Euler Maruyama Scheme
t.s = seq(0,t,length=n+1) #time step



#Milstein scheme
milstein = function(mu,sigma,X0)
  for (j in range(N))
    X += mu*X*dt + sigma*X*dB[j] + 0.5*sigma**2 * X * (dB[j] ** 2 - dt)
Xmil.append(X)