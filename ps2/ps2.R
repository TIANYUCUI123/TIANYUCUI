#PROBLEM1#

#clear up everything#
rm(list=ls())
#generate a uniform distribution with range 1:3#
set.seed(123)
X1<-runif(10000, min=1, max=3) 
View(X1)
#generate a gamma distribution with shape 3 and scale2#
X2<-rgamma(10000, 3, rate = 0.5)
View(X2)
#generate a binomial distribution with prob=0.3#
X3<-rbinom(10000,1, prob=0.3)
View (X3)
#generate a normal distribution with mean 2 and sd1#
eps<-rnorm(10000, mean = 2, sd = 1)
#Create Y which is the linear combination of X#
Y=0.5+1.2*X1-0.9*X2+0.1*X3+eps
View(Y)
#Create ydummy variable#
y<-as.numeric(Y>mean(Y))


#PROBLEM2#
#Calculate the correlation between Y and X1#
cor<-cor(Y,X1)
DISTANCE=cor - 1.2
#run the regression of Y and X variables
reg1<-lm(formula = Y ~ X1+X2+X3 )
summary(reg1)
#calculate the coefficient of Y on X1, X2, X3#
coefficients(reg1) 
#do the OLS calculation to compare with R#
#Step 1: generate the matrix of X,Y#
X<-as.matrix(cbind(1,X1,X2,X3))
Y<-as.matrix(Y)
#Step 2: do the calculation (X'X)^-1 X'Y#
beta<- solve(t(X)%*%X)%*%t(X)%*%Y
#calculate the standard errors using the standard formulas of OLS#
#Step1: calculate the residuals variance#
VARe <- as.numeric(t(Y-X%*%beta)%*%(Y-X%*%beta)/(10000-4) )
#Step2 : calculate the standard error of the beta coefficient#
std<- diag(sqrt(solve(t(X)%*% X)*VARe))


#do the bootstrap with replication 49 and 499 respectively#
#take 49 samples with replacement from sample x of size 10000

std.boot<-NA
datanew<- cbind(X,Y)
for (i  in 1:49){
  bootdata <- datanew[sample(nrow(datanew), 10000, replace = TRUE),] 
  beta<- solve(t(datanew[,c(1:4)])%*%datanew[,c(1:4)])%*%t(datanew[,c(1:4)])%*%datanew[,5]
  VARe <- as.numeric(t(datanew[,5]-datanew[,c(1:4)]%*%beta)%*%(datanew[,5]-datanew[,c(1:4)]%*%beta)/(10000-4) )
  std<- diag(sqrt(solve(t(datanew[,c(1:4)])%*% datanew[,c(1:4)])*VARe))
  std.boot<-rbind(std.boot,std)
  
}
std.boot<-std.boot[-1,]
View (std.boot)
#take 499 samples with replacement from sample x of size 10000
std.boot<-NA
  for (i  in 1:499){
    bootdata <- datanew[sample(nrow(datanew), 10000, replace = TRUE),] 
    beta<- solve(t(datanew[,c(1:4)])%*%datanew[,c(1:4)])%*%t(datanew[,c(1:4)])%*%datanew[,5]
    VARe <- as.numeric(t(datanew[,5]-datanew[,c(1:4)]%*%beta)%*%(datanew[,5]-datanew[,c(1:4)]%*%beta)/(10000-4) )
    std<- diag(sqrt(solve(t(datanew[,c(1:4)])%*% datanew[,c(1:4)])*VARe))
    std.boot<-rbind(std.boot,std)
    
  }
std.boot<-std.boot[-1,]
View (std.boot)

#PROBLEM3#
#write a function that returns the liklelihood of the probit#
#generate a uniform distribution with range 1:3#
X0<-rep(1,10000)
X1<-runif(10000, min=1, max=3) 
X2<-rgamma(10000, 3, rate = 0.5)
X3<-rbinom(10000,1, prob=0.3)
eps<-rnorm(10000, mean = 2, sd = 1)
Y=0.5+1.2*X1-0.9*X2+0.1*X3+eps
y<-as.numeric(Y>mean(Y))
X<-as.matrix(cbind(X0,X1,X2,X3))
View(X)

#write a likelihood function#
probit.nll <- function (mlebeta,X,y) {
  # linear predictor
  eta <- X %*% mlebeta
  # probability
  p <- pnorm(eta)
  # negative log-likelihood(we can get the maximize likelihood function thereby)
  loglk <-(-sum((1 - y) * log(1 - p) + y * log(p)))
}
probit<-probit.nll (c(0,0,0,0),X,y)
print(probit)

#Implement the steepest ascent optimization algorithm to maximize that likelihood#
#gradient function in r#
probit.gr<- function (mlebeta,X,y) {
  Phi = pnorm(X %*% mlebeta) # Phi is Cumulative probability
  phi = dnorm(X %*% mlebeta) # phi is Probability Density
  n = length(y)           # sample size
  k = length(mlebeta)         # number of coefficients
  g = t(matrix(rep(phi/Phi,k),nrow=n)*X) %*% y - t(matrix(rep(phi/(1-Phi),k),nrow=n)*X) %*% (1-y)
  g = -g
   return(g)
} 

probit.gr  (c(0,0,0,0),X,y)
# set up an intial value of the mlebeta
mlebeta<-lm(Y~X1+X2+X3)$coefficient
#define the old beta#
beta<-mlebeta
#define alpha #
alpha<-0.00000003
#define the new beta#
newbeta<-beta-alpha*probit.gr(beta,X,y)
l1<-probit.nll(beta,X,y)
l2<-probit.nll(newbeta,X,y)
diff<-abs(l2-l1)
while (diff> 0.01){
  l1<-probit.nll(beta,X,y)
  probit.gr<- function (beta,X,y) 
  newbeta<-beta-alpha*probit.gr
  l2<-probit.nll(newbeta,X,y)
  diff<-abs(l2-l1)
  beta<-newbeta
  
}
View (newbeta)
glm(y ~ X1 + X2 +X3 ,family=binomial(link="probit"))$coefficients
#PROBLEM4#
####################################probit#####################################
#write the optimization of the probit model#
X<-as.matrix(cbind(1,X1,X2,X3))
Y<-as.matrix(Y)
y<-as.numeric(Y>mean(Y))
# initial values
inits <-  lm(Y ~ X1 + X2 + X3 )$coef
# MLE estimation of probit model#
probit <- optim(inits, probit.nll,probit.gr, method = "BFGS", hessian = TRUE)
probitparameter<-probit$par
# checking with R's built-in function
glm(y ~ X1 + X2 +X3 ,family=binomial(link="probit"))$coefficients
#####################################logit#####################################
X<-as.matrix(cbind(1,X1,X2,X3))
Y<-as.matrix(Y)
y<-as.numeric(Y>mean(Y))
# Likelihood of the logit model#
logit.loglk <- function(mlebeta, X, y){
  loglk <- sum(y * plogis(X%*%mlebeta, log.p=TRUE) + (1-y) * plogis(-(X%*%mlebeta), log.p=TRUE))
   return(-loglk)
}
#optimization problem#
mlebeta <-c(-0.1, -0.3, 0.001, 0.01) # arbitrary starting parameters
optimLogit = optim(mlebeta, logit.loglk,X = X, y = y, method = 'BFGS', hessian=TRUE)
logitparameter<-optimLogit$par
View(logitparameter)
optimLogit$par
# checking with R's built-in function
glm(y ~ X1 + X2 +X3 ,family=binomial(link="logit"))$coefficients
#############################linear probability model###########################
X<-as.matrix(cbind(1,X1,X2,X3))
Y<-as.matrix(Y)
y<-as.numeric(Y>mean(Y))
linearparameter<- lm(y ~ X1 + X2 + X3 )$coefficient
############################Combine the results together########################
parameter<-rbind(probitparameter,logitparameter,linearparameter)
View(parameter)

#PROBLEM5#

# Marginal effects (ME) calculation in probit model
phi<-dnorm(X %*% mlebeta)
meprobit<-matrix(0,nrow = 10000,ncol = 4)
mlebeta<-as.matrix(mlebeta)
View(mlebeta)
for (i in 1:4) {
  meprobit[,i] <-phi %*% mlebeta[i,]
  
}

#Marginal effect (ME) calculation in logit model
philogis<-dlogis(X %*% mlebeta)
melogis<-matrix(0,nrow = 10000,ncol = 4)
mlebeta<-as.matrix(mlebeta)
for (i in 1:4) {
  melogis[,i] <-phi %*% mlebeta[i,]
}
View(melogis)

#compute the standard deviation of probit model using the delta method#
install.packages("numDeriv")
library(numDeriv)
Xmean<-apply(X,2,mean)
View(Xmean)
View(mlebeta)
maginX<-function(mlebeta,X = Xmean){
  phi<-dnorm(X %*% t(mlebeta))%*% mlebeta
}
jac<-jacobian(maginX,mlebeta)
glmprobit<-glm(y ~ X1 + X2 +X3 ,family=binomial(link="probit"))
variance<-vcov(glm(y ~ X1 + X2 +X3 ,family=binomial(link="probit")))
jvariance<-jac%*%variance%*%t(jac)
std<-diag(sqrt(jvariance))
View(std)
#compute the standard deviation of logit model using the delta method#
Xmean<-apply(X,2,mean)
View(Xmean)
View(mlebeta)
maginX<-function(mlebeta,X = Xmean){
  phi<-dlogis(X %*% t(mlebeta))%*% mlebeta
}
jac<-jacobian(maginX,mlebeta)
glmprobit<-glm(y ~ X1 + X2 +X3 ,family=binomial(link="logit"))
variance<-vcov(glm(y ~ X1 + X2 +X3 ,family=binomial(link="logit")))
jvariance<-jac%*%variance%*%t(jac)
std<-diag(sqrt(jvariance))
View(std)
#compute the standard deviation using the bootstrap of probit model #
abootprobit<-NA
data<-meprobit
for (i  in 1:499){
  bootdata <- meprobit[sample(nrow(meprobit), 10000, replace = TRUE),] 
  averagex<- c(mean(bootdata[,1]),mean(bootdata[,2]),mean(bootdata[,3]),mean(bootdata[,4]))
  abootprobit<-rbind(abootprobit,averagex)
  
}

abootprobit<-abootprobit[-1,]
View (abootprobit)
stdabootprobit<-c(sd(abootprobit[,1]),sd(abootprobit[,2]),sd(abootprobit[,3]),sd(abootprobit[,4]))
View (stdabootprobit)

#compute the standard deviation using the bootstrap of logit model#
abootlogit<-NA
data<-melogis
for (i  in 1:499){
  bootdata <- melogis[sample(nrow(melogis), 10000, replace = TRUE),] 
  averagex<- c(mean(bootdata[,1]),mean(bootdata[,2]),mean(bootdata[,3]),mean(bootdata[,4]))
  abootlogit<-rbind(abootlogit,averagex)
  
}
abootlogit<-abootlogit[-1,]
View (abootlogit)
stdabootmelogis<-c(sd(abootlogit[,1]),sd(abootlogit[,2]),sd(abootlogit[,3]),sd(abootlogit[,4]))
View (stdabootmelogis)

