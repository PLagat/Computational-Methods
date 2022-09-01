#QUESTION 3
##Finding expected value of I=𝐸[exp (√𝑈)] where U is uniformly distributed
##M values = 10^2,10^3,10^4,10^5
##for M =10^2
set.seed(1)
M1=10^2
U1= runif(M1)
head(U1)
Y_1=exp(sqrt(U1))
head(Y_1)
Im.1 =(sum(Y_1)/M1)
Im.1

set.seed(1)
M2=10^3
U2= runif(M2)
head(U2)
Y_2=exp(sqrt(U2))
head(Y_2)
Im.2 =(sum(Y_2)/M2)
Im.2

set.seed(1)
M3=10^4
U3= runif(M3)
head(U3)
Y_3=exp(sqrt(U3))
head(Y_3)
Im.3 =(sum(Y_3)/M3)
Im.3

set.seed(1)
M4=10^5
U4= runif(M4)
head(U4)
Y_4=exp(sqrt(U4))
head(Y_4)
Im.4 =(sum(Y_4)/M4)
Im.4


#VARIANCE
b=1
M1=10^2
variance= (b^2/M1)*var(Y_1)
variance

M2=10^3
variance2= (b^2/M2)*var(Y_2)
variance2

M3=10^4
variance3= (b^2/M3)*var(Y_3)
variance3

M4=10^5
variance4= (b^2/M4)*var(Y_4)
variance4
 
#confidence interval
##standard deviations
s1= sqrt(variance)
s1
s2=sqrt(variance2)
s2
s3=sqrt(variance3)
s3
s4=sqrt(variance4)
s4
## at 95% = (-1.96, +1.96)
###at M1=10^2
CI.1=c((Im.1-(1.96*(s1/sqrt(M1)))),(Im.1+(1.96*(s1/sqrt(M1)))))
CI.1

###at M2=10^3
CI.2=c((Im.2-(1.96*(s2/sqrt(M2)))),(Im.2+(1.96*(s2/sqrt(M2)))))
CI.2

###at M3=10^4
CI.3=c((Im.3-(1.96*(s3/sqrt(M3)))),(Im.3+(1.96*(s3/sqrt(M3)))))
CI.3

###at M4=10^5
CI.4=c((Im.4-(1.96*(s4/sqrt(M4)))),(Im.4+(1.96*(s4/sqrt(M4)))))
CI.4


#ANTITHETIC VARIATE 

##M values = 10^2,10^3,10^4,10^5
##for M =10^2
set.seed(1)
M1=10^2
U1.a= runif(M1)
head(U1.a)
Y_1.a=(exp(sqrt(U1.a))+exp(sqrt(1-U1.a)))/2
head(Y_1.a)
Im.1hat=(sum(Y_1.a)/M1)
Im.1hat

set.seed(1)
M2=10^3
U2.a= runif(M2)
head(U2.a)
Y_2.a=(exp(sqrt(U2.a))+exp(sqrt(1-U2.a)))/2
head(Y_2.a)
Im.2hat =(sum(Y_2.a)/M2)
Im.2hat

set.seed(1)
M3=10^4
U3.a= runif(M3)
head(U3.a)
Y_3.a=(exp(sqrt(U3.a))+exp(sqrt(1-U3.a)))/2
head(Y_3.a)
Im.3hat=(sum(Y_3.a)/M3)
Im.3hat

set.seed(1)
M4=10^5
U4.a= runif(M4)
head(U4.a)
Y_4.a=(exp(sqrt(U4.a))+exp(sqrt(1-U4.a)))/2
head(Y_4.a)
Im.4hat =(sum(Y_4.a)/M4)
Im.4hat


#VARIANCE
b=1
M1=10^2
variance.a= (b^2/M1)*var(Y_1.a)
variance

M2=10^3
variance2.a= (b^2/M2)*var(Y_2.a)
variance2.a

M3=10^4
variance3.a= (b^2/M3)*var(Y_3.a)
variance3.a

M4=10^5
variance4.a= (b^2/M4)*var(Y_4.a)
variance4.a

#confidence interval
##standard deviations
s1.a= sqrt(variance.a)
s1.a
s2.a=sqrt(variance2.a)
s2.a
s3.a=sqrt(variance3.a)
s3.a
s4.a=sqrt(variance4.a)
s4.a
## at 95% = (-1.96, +1.96)
###at M1=10^2
CI.1.a=c((Im.1hat-(1.96*(s1.a/sqrt(M1)))),(Im.1hat+(1.96*(s1.a/sqrt(M1)))))
CI.1.a

###at M2=10^3
CI.2.a=c((Im.2hat-(1.96*(s2.a/sqrt(M2)))),(Im.2hat+(1.96*(s2.a/sqrt(M2)))))
CI.2.a

###at M3=10^4
CI.3.a=c((Im.3hat-(1.96*(s3.a/sqrt(M3)))),(Im.3hat+(1.96*(s3.a/sqrt(M3)))))
CI.3.a

###at M4=10^5
CI.4.a=c((Im.4hat-(1.96*(s4.a/sqrt(M4)))),(Im.4hat+(1.96*(s4.a/sqrt(M4)))))
CI.4.a

#RATIO OF LENGTHS 
##Length
L1=CI.1[2]-CI.1[1]
L1

L2=CI.2[2]-CI.2[1]
L2

L3=CI.3[2]-CI.3[1]
L3

L4=CI.4[2]-CI.4[1]
L4

L1.a=CI.1.a[2]-CI.1.a[1]
L1.a

L2.a=CI.2.a[2]-CI.2.a[1]
L2.a

L3.a=CI.3.a[2]-CI.3.a[1]
L3.a

L4.a=CI.3.a[2]-CI.3.a[1]
L4.a

#Ratio 
ratio1= (L1/L1.a)
ratio1
ratio2= (L2/L2.a)
ratio2
ratio3= (L3/L3.a)
ratio3
ratio4= (L4/L4.a)
ratio4


tab <- matrix(c(Im.1,Im.1hat ,CI.1[1],CI.1[2], CI.1.a[1], CI.1.a[2],ratio1, Im.2,Im.2hat,CI.2[1],CI.2[2] ,CI.2.a[1], CI.2.a[2],ratio2,Im.3,Im.3hat,CI.3[1],CI.3[2],CI.3.a[1],CI.3.a[2], ratio3,Im.4,Im.4hat ,CI.4[1],CI.4[2] ,CI.4.a[1],CI.4.a[2], ratio4), ncol=7, byrow=TRUE)
colnames(tab) <- c('Im','Im.hat','95% CI-Im-lower','95% CI-Im-upper','95% CI-Imhat-lower','95% CI-Imhat-upper','Ratio of length')
rownames(tab) <- c('M=10^2','M=10^3','M=10^4','M=10^5')
tab <- as.table(tab)
tab









