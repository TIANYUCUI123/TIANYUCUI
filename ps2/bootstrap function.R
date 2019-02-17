#generate bootstrap function#

set.seed(1000)
datanew<- cbind(X,Y)
parameter<- ncol(X)
stdboot<-function(X,Y,k,n){
  for (k in 1: k) {   
    datanew<- as.matrix(cbind(X,Y))
    boot <- datanew[sample(nrow(datanew), n, replace = TRUE),] #sample each row with replacement, and repeat for 49 times#}
     beta<- solve(t(boot[,c(1:4)])%*%boot[,c(1:4)])%*%t(boot[,c(1:4)])%*%boot[,5] 
     VARe <- as.numeric(t(boot[,5]-boot[,c(1:4)]%*%beta)%*%(boot[,5]-boot[,c(1:4)]%*%beta)/(n-4) )
     std[k,]<- t(diag(sqrt(solve(t(boot[,c(1:4)])%*% boot[,c(1:4)])*VARe))) 
   }
   return(std)
}
X<-as.matrix(cbind(1,X1,X2,X3))
Y<-as.matrix(Y)
stdboot(X,Y,49,10000)
View(stdboot(X,Y,49,10000))
boot<-# check the validity of the function#
  
std<-function(boot,k,n){
    for (k in 1:k)
      beta<- solve(t(boot[,c(1:4)])%*%boot[,c(1:4)])%*%t(boot[,c(1:4)])%*%boot[,5] 
    VARe <- as.numeric(t(boot[,5]-boot[,c(1:4)]%*%beta)%*%(boot[,5]-boot[,c(1:4)]%*%beta)/(n-4) )
    std[k,]<- t(diag(sqrt(solve(t(boot[,c(1:4)])%*% boot[,c(1:4)])*VARe)))
  }
