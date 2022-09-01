#GROUP 1 #CLASS ACTIVITY 1
#ADM NO.	NAME
#115264	ROTICH MAESTRO KIPKE
#120228	NG'AYU SHANI MUMBI
#123776	OLAKA BRANDON ODHIAMBO
#122608	GITONGA BRIAN MURIMI
#121034	LAGAT PATRICIA JEPKOGEI
#120642	WAMBUA ELIZABETH MWIKALI
#119224	NJOROGE HARRISON NJENGA

# (I)Exponential distribution ??=2

#Exponential dist pdf - f(x)=??e^(-2x)
#"                      "     cdf - y=1-e^(-2x)
#y=1-e^(-2x)

#Make x the subject of the formula;

#e^(-2x)=1-y
#ln(e^(-2x) )=ln(1-y)
#-2x=ln(1-y)
#x=-1/2 ln(1-y)
#To generate random numbers then, since  X ~ N(0,sigma^2)

y <- runif(5,0,1)

a<-log(1-y)
b<- -0.5*a
b 
 #[1] 0.02388901 0.07015448 1.06413259
 #[4] 0.91543362 1.01301758


# (II) Poisson Distribution, ??=2

pois_inv_trans_regular = function(n, lambda){
  X = rep(0, n) # generate n samples
  for(m in 1:n){
    U = runif(1)
    i = 0; F = exp(-lambda) # initialize
    while(U >= F){
      i = i+1; F = F + dpois(i,lambda) # F=F+pi
    }
    X[m] = i
  }
  X
}
### test the code for ??=2
set.seed(0); X = pois_inv_trans_regular(n=10000,lambda=2); c(mean(X),var(X))
# [1]  2.007000 2.046756


# (III) Discrete function with pdf f(x) ={(0.10 if x=1)

# THE PROBABILITIES WAS ADDING UP TO 1.23 (PLEASE ADVISE)

#Solution Discovered (NOT inverse transform method)
P= c(0.10,0.80,0.33);
X = c(1,2,3);
counter = 1;
r = runif(1, min = 0, max = 1);
while(r > P[counter])
  counter =counter + 1;
end
X[counter] # 2

