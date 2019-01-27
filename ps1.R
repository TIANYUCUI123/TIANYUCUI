# clear console# 
cat("\014") 
# clear workspace#
rm(list=ls())
# set working directory #
setwd("C:/Users/cuiti/Master Study/Second Semester/econometrics/TIANYUCUI")
# import the database for question 1#
library(readr)
datstu <- read.csv("C:/Users/cuiti/Master Study/Second Semester/econometrics/dat/datstu.csv",na.string=c("","NA"))
# question1.1:observation of student#
nrow(datstu)

# question1.2:number of schools #

#select the schoolcode varaibles#
Nschool<-c("schoolcode1","schoolcode2","schoolcode3","schoolcode4","schoolcode5","schoolcode6")
School <- datstu[,5:10]
vec1 <- c(as.matrix(School))
Nschool<-unique(vec1)
#replace Nschool by dropping NA in the data set#
Nschool <- Nschool[ !is.na(Nschool ) ]
length(Nschool)

# question1.3:number of programs#
Nprogram<-c("choicepgm1","choicepgm2","choicepgm3","choicepgm4","choicepgm5","choicepgm6")
Program<-datstu[Nprogram]
vec2 <- c(as.matrix(Program))
Nprogram<-unique(vec2)
Nprogram<-Nprogram[ !is.na( Nprogram)]
length(Nprogram)

# question 1.4: number of choices#
Choice1<- cbind(vec1,vec2)
Choice1<-unique(Choice1)
Choice1<-na.omit(Choice1)
nrow(Choice1)

#question 1.5:missing test score#
Score <- c("score")
Score<-datstu[Score]
Score<- as.vector(Score)
#number of rows in overal database#
nrow(Score)
#number of rows that omit the missing values#
Scorefull<-na.omit(Score)
nrow(Scorefull)
#the difference should be the number of missing values#
nrow(Score)-nrow(Scorefull)

#question 1.6 apply to the same schools-meaning1#
School1<-apply(datstu[,5:10],1,function(x) length(x[!is.na(x)])-length(unique(x[!is.na(x)])))
length(School1[School1!=0])
#question 1.6 apply to the same schools-meaning2#
School2<-datstu[,c(5:10)]
vec5<-c(as.matrix(School2))
ID<-rep(1:340823,6)
ID<-as.vector(ID)
School3<-cbind(ID,vec5)
View(School3)
X<-unique(School3[,c('ID','vec5')])
table(X[,2])

#question 1.7 apply to less than 6 choices#

Nprogram<-datstu[,c(11:16)]
nrow(Nprogram)
#omit the rows that have no value#
Program <- na.omit(Nprogram)
View(Program)
nrow(Program)
c1<-nrow(Nprogram)-nrow(Program)
Nschool1<-datstu[,c(5:10)]
nrow(Nschool1)
school1<-na.omit(Nschool1)
nrow(school1)
c2<-nrow(Nschool1)-nrow(school1)
max(c1,c2)


#question2.1#
library(readr)
datsss <- read.csv("C:/Users/cuiti/Master Study/Second Semester/econometrics/dat/datsss.csv",na.string=c("","NA"))
datjss<- read.csv("C:/Users/cuiti/Master Study/Second Semester/econometrics/dat/datjss.csv",na.string=c("","NA"))
datsss<- datsss[!is.na(datsss$schoolname), ]
datsss<- datsss[!duplicated(datsss[,c('schoolcode')]),]
datsss$X <- NULL
#merge with the choice#
sss <- merge(Choice1, datsss, by.x = "vec1", by.y = "schoolcode")
View(sss)

#delete invalid ranking with na and 99#
datstu <- read.csv("C:/Users/cuiti/Master Study/Second Semester/econometrics/dat/datstu.csv",na.string=c("","NA"))
datstu<- na.omit(datstu, cols="rankplace")
datstu<-datstu[!datstu$rankplace == "99", ]
#match the ranking with the schoolcode#
admit_school=c()
for (i in 1:dim(datstu)[1]) {
  admit_school[i]=datstu[i,(datstu$rankplace+4)[i]]
}
datstu=cbind(datstu,admit_school)
datstu[1:nrow(da)]