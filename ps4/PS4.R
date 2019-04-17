#exercise 1 : importiing data#
# clear console# 
cat("\014") 
# clear workspace#
rm(list=ls())
set.seed(1)
library(readxl)
Koop_Tobias <- read_excel("C:/Users/cuiti/Master Study/Second Semester/econometrics/TIANYUCUI/ps4/Koop-Tobias.xlsx")
data<-Koop_Tobias#change the data name
install.packages("plm")
library(plm)  
dataPanel <- pdata.frame(data, index=c("PERSONID"))
freq<-as.vector(table(data$PERSONID))
class(freq)
#the table list of frequency for each individual, which represent the time 
pid<-sample(unique(data$PERSONID),5)
#find 5 individuals and their matched information#
x<-data[data$PERSONID%in%pid, 1:4]
View(x)

#exercise 2 random effect model#
library(nlme)
model<- glm(LOGWAGE~EDUC+POTEXPER, data = data)
gls<- model$coefficients
modelcheck<- plm(LOGWAGE~EDUC+POTEXPER,data= dataPanel, model = "random")#since I already manipulate data in exercise 1, I can just input the data#
plm<- modelcheck$coefficients
coef_result<- rbind(gls,plm)
coef_result
#In fact, there is a slightly different on education and experience coefficient#

#exercise 3 fixed effect model#
dataorder<-data[order(data$PERSONID),]
choice<-c("PERSONID", "LOGWAGE", "EDUC","POTEXPER")
dataorder <- dataorder[choice]
#within estimation#
LOGWAGEMEAN<- aggregate(dataorder[,2],list(dataorder$PERSONID),mean)
#The mean of logwage for each individual#
EDUCMEAN<-aggregate(dataorder[, 3], list(dataorder$PERSONID), mean)
# The mean of education for each individual#
POTEXPERMEAN<-aggregate(dataorder[,4],list(dataorder$PERSONID),mean)
# mean of potential experience for each indivdiual#
MEAN<-cbind(LOGWAGEMEAN,EDUCMEAN,POTEXPERMEAN)
names(MEAN) <- c("PERSONID", "LOGWAGEMEAN","DROP1","EDUCMEAN","DROP2","POTEXPERMEAN")
mean<-subset(MEAN,select = c(PERSONID,LOGWAGEMEAN,EDUCMEAN,POTEXPERMEAN))
# We finish arrange the mean of each needed variable#
withindata<-merge(mean,dataorder,by="PERSONID")
# arrange the data so that match with the rows and columns for initial database#
View(withindata)
n<-nrow(withindata)
regdata<-matrix(0,nrow=n,ncol=4)
for ( i in 2:4) { 
  regdata[,i]=withindata[,i+3]-withindata[,i]
}
regdata<-data.frame(regdata[,-1])
#Since the first column is personid difference, which will be zero, so we can rule out the first column#
colnames(regdata)<-c("withinwage","withineduc","withinpotexp")
reg1<-lm(withinwage~withineduc+withinpotexp,data = regdata)
withincoefficient<- reg1$coefficients[2:3]
modelcheck2<- plm(LOGWAGE~EDUC+POTEXPER,data= dataPanel, model = "within")
modelcoefficient<-modelcheck2$coefficients
result1<-rbind(withincoefficient,modelcoefficient)
print(result1)

#Between estimation#
reg2<-lm(LOGWAGEMEAN~EDUCMEAN+POTEXPERMEAN,data=mean)
betweencoefficient<-reg2$coefficients
modelcheck3<-plm(LOGWAGE~EDUC+POTEXPER,data= dataPanel, model = "between")
modelcoefficient<-modelcheck3$coefficients
result2<-rbind(betweencoefficient,modelcoefficient)
print(result2)

#first difference estimation#
dataorder<-data[order(data$PERSONID),]
choice<-c("PERSONID", "LOGWAGE", "EDUC","POTEXPER")
dataorder <- as.matrix(dataorder[choice])
#reupdate the database to help do the regression#
v<-matrix(0,nrow=n,ncol=ncol(dataorder))
#generate a new database to help do the regression#
for(i in 2:nrow(dataorder)){
  v[i,]<- dataorder[i,] - dataorder[i-1,]
}
#first difference data#
colnames(v)<-c("PERSONID","WAGE","EDUC","POTEXP")
v<-data.frame(v)
v<-v[ which( ! v$PERSONID ==1) , ]
#for those personid difference that is not zero, we should eliminate since it represent the first difference of initial role of each individual#
v <- v[-1,]
nrow(v)
print(nrow(v))
print(nrow(dataorder)-length(unique(data$PERSONID)))
#check whether this method to do the arrangement is valid or not#
reg3<-lm(WAGE~EDUC+POTEXP,data = v)
fdcoefficient<- reg3$coefficients
modelcheck4<-plm(LOGWAGE~EDUC+POTEXPER,data= dataPanel, model = "fd")
modelcoefficient<-modelcheck4$coefficients
result3<-rbind(fdcoefficient,modelcoefficient)
print(result3)

############another method that use  the diff function#######################################
dataorder<-data.frame(dataorder)
diffwage <- unlist(by(dataorder$LOGWAGE , list(dataorder$PERSONID) , function(i) c(NA,diff(i))))
diffwage<- na.omit(diffwage)
diffedu <- unlist(by(dataorder$EDUC , list(dataorder$PERSONID) , function(i) c(NA,diff(i))))
diffedu<- na.omit(diffedu)
diffpotexper<-- unlist(by(dataorder$POTEXPER , list(dataorder$PERSONID) , function(i) c(NA,diff(i))))
diffpotexper<- na.omit(diffpotexper)
reg4<-lm(diffwage~diffedu+diffpotexper)
fdcoefficient<- reg3$coefficients
modelcheck4<-plm(LOGWAGE~EDUC+POTEXPER,data= dataPanel, model = "fd")
modelcoefficient<-modelcheck4$coefficients
result4<-rbind(fdcoefficient,modelcoefficient)
print(result4)


#exercise 4 understanding fixed effects
set.seed(1)
data<-Koop_Tobias
pid<-sample(unique(data$PERSONID),100)
data4<-data[data$PERSONID%in%pid,]
freq<-table(data4$PERSONID)
sum(freq)
# sample for 100 individual information#
beta<-rep(0.1,103)
ols_func <- function (beta,y=y,x1=x1,x2=x2) {
  fepar<-beta[1:100]#fixed effect parameter#
  beta1<-beta[101]#education coefficient#
  beta2<-beta[102]#potential experience coefficient#
  s<-beta[103]# standard error#
  index<-as.numeric(freq)
  alpha<-rep(fepar,index)#generate a indivdiual fixed effect vector, which length should be equal to sum of frequency for 100 individual#
  v<- (y-alpha-beta1*x1-beta2*x2)/ s
  p<-dnorm(v)
  lg<-log(p)
  loglsum<-sum(lg)
  return(-loglsum)
  
}  # finish the loglikelihood#

beta[1:102]<-rnorm(102)
beta[103]<- 1
y<-data4$LOGWAGE
x1<-data4$EDUC
x2<-data4$POTEXPER
optim(beta,ols_func,y=y,x1=x1,x2=x2)
#####start the ols regression model to find estimated indivdiual fixed effect####
reg<-lm(y~x1+x2+factor(data4$PERSONID)-1)
coefficient<-reg$coefficients
ynew<-coefficient[3:102]
data4new<-data4[!duplicated(data4$PERSONID),]
#####set up the regression of individual fixed effect on time invariant######
reg2<-lm(ynew~ABILITY+MOTHERED+FATHERED+BRKNHOME+SIBLINGS,data = data4new)
coefficientnew<-reg2$coefficients
print(coefficientnew)
#######compute the standard error#
modelcheck2<- plm(LOGWAGE~EDUC+POTEXPER,data= dataPanel, model = "within")
modelcheck2$vcov
modelcheck3<-plm(LOGWAGE~EDUC+POTEXPER,data= dataPanel, model = "between")
modelcheck3$vcov[2:3,2:3]
modelcheck4<-plm(LOGWAGE~EDUC+POTEXPER,data= dataPanel, model = "fd")
modelcheck4$vcov[2:3,2:3]
#we figure out that the standard error of the first difference and between estimator are the same#<-data[order(data$PERSONID),]
X1<-c( "EDUC","POTEXPER")
X <- as.matrix(data[X1])# find the X matrix#
XX<-solve(t(X)%*% X)
residual<-modelcheck2$residuals # find the residule of the within estimation#
A<-t(X) %*% diag(residual)^2%*% X
# we search online that use the huber white standard error to revise the standard ols se
new_std<-XX %*% A %*% XX
print(diag(sqrt(new_std)))
# install the deplyr  package
install.packages("dplyr")
library(dplyr)
betaboot<-matrix(0,nrow=49,ncol = 6)
for (k  in 1:49){
    pid<-sample(unique(data$PERSONID),100)
   
    boot_sample<-data.frame()
    for (n in 1:100){
      boot_sample<- rbind(boot_sample,filter(data,PERSONID==pid[n]))
    }
  
#fixed_effect_model_alpha#
alpha<-lm(LOGWAGE~-1+EDUC+POTEXPER+factor(PERSONID),data = boot_sample)$coefficients
alpha<-alpha[-c(1,2)]
# subsetting the invariant variables#
invariant<-c("PERSONID", "ABILITY","MOTHERED","FATHERED","BRKNHOME","SIBLINGS")
boot_invariant <- as.data.frame(boot_sample[invariant])
boot_invariant<-unique(boot_invariant)

# run  the fixed effect regression#
betaboot[k,]<-lm(alpha~boot_invariant$ABILITY+boot_invariant$MOTHERED+boot_invariant$FATHERED+boot_invariant$BRKNHOME+boot_invariant$SIBLINGS)$coefficients
}
sdboot<-apply(betaboot, 2, sd)
print(sdboot)
