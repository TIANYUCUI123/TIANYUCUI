# clear console# 
cat("\014") 
# clear workspace#
rm(list=ls())
#install the bayesm package
install.packages("bayesm")
#start the package
library(bayesm)
#record the data
data(margarine)
data1<- margarine$choicePrice
data2<- margarine$demos
data<-merge(data1,data2,by="hhid")
#average and dispersion of product characteristics#
apply(data1[,3:12],2,mean)
apply(data1[,3:12],2,sd)
#market share by product characteristics#
freq<-table(data1$choice)
View(freq)
marketshare<- freq/4470
View(marketshare)
#market share by brand#
freq[1]
marketshareppk<-sum(freq[1],freq[7])/4470
marketsharepbb<-freq[2]/4470
marketsharepfi<-sum(freq[3],freq[8])/4470
marketsharephse<-sum(freq[4],freq[10])/4470
marketsharepgen<-freq[5]/4470
marketshareplmp<-freq[6]/4470
marketsharepss<-freq[9]/4470
brandshare<-cbind(marketshareppk,marketsharepbb,marketsharepfi,marketsharephse,marketsharepgen,marketshareplmp,marketsharepss)
View(brandshare)
#market share by forms#
stk<-sum(freq[1:6])/4470
tub<-sum(freq[7:10])/4470
formshare<-cbind(stk,tub)
View(formshare)
#mapping with the observed attributes#
Freqs <-table(data$Income, data$choice)
Freqs[1,]
class(Freqs)
Sum<-table(data$Income)
Sum[1]
prob<- matrix(0,nrow=14,ncol=10) 
for ( i in 1:14){ 
  prob[i,]<-Freqs[i,]/Sum[i]
  
}
View(prob)
rownames(prob)<-c("Income2.5","Income7.5","Income12.5","Income17.5","Income22.5","Income27.5","Income32.5","Income37.5","Income42.5","Income47.5","Income55","Income67.5","Income87.5","Income130")
colnames(prob) <- colnames(data1[,3:12])
View(prob)
#problem 2#

