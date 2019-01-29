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


#question2#
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
colnames(datstu)[colnames(datstu)=="admit_school"] <- "schoolcode"

#match rank with the program code#
admit_program=c()
for (i in 1:dim(datstu)[1]) {
  admit_program[i]=as.character(datstu[i,(datstu$rankplace[i]+10)])
}
datstu=cbind(datstu,admit_program)
colnames(datstu)[colnames(datstu)=="admit_program"] <- "program"

#find the minimum score, average score,and size#
stu<-c("X","score","agey","male","schoolcode","program")
stu<-datstu[stu]
stu<-stu[with(stu, order(schoolcode, program)),]
cutoff<-tapply(stu$score, list(stu$schoolcode, stu$program), min)
quality<-tapply(stu$score,list(stu$schoolcode,stu$program),mean)
size<-tapply(stu$score,list(stu$schoolcode,stu$program),length)
#raarrange database to prepare for merge#
cutoff <- data.frame(schoolcode=rep(row.names(cutoff),ncol(cutoff)),
                j=rep(colnames(cutoff),each=nrow(cutoff)),
                cutoff=as.vector(cutoff))
quality <- data.frame(schoolcode=rep(row.names(quality),ncol(quality)),
                     j=rep(colnames(quality),each=nrow(quality)),
                     quality=as.vector(quality))
size <- data.frame(schoolcode=rep(row.names(size),ncol(size)),
                     j=rep(colnames(size),each=nrow(size)),
                     size=as.vector(size))
ss2<-cbind(cutoff,quality,size)
ss2 <- ss2[, !duplicated(colnames(ss2))]
#merge the ss2 and  sss#
sss$vec1<- with(sss, paste0(vec1, sep=",",vec2))
ss2$schoolcode<- with(ss2, paste0(schoolcode,sep=",", j))
sss<-as.data.frame(sss)
ss3<-as.data.frame(ss2)
data2<- merge(ss2, sss, by.x = "schoolcode", by.y = "vec1",all=TRUE)
data2 <- data2[ -c(2,6) ]
colnames(data2)[colnames(data2)=="schoolcode"] <- "schoolprogram"
data2 <- na.omit(data2)
write.csv(data2,"question2table.csv")
#question3#
# In this question, I treat the distance as the length between high school that students are admitted and junior high school#
data3<-merge(datstu,datjss,by="jssdistrict")
colnames(data3)[colnames(data3)=="X.x"]<-"studentID"
colnames(data3)[colnames(data3)=="point_x"]<-"jsslong"
colnames(data3)[colnames(data3)=="point_y"]<-"jsslat"
data3$X.y <- NULL
datsss<-subset(datsss, !duplicated(schoolcode))
datstu<-merge(data3,datsss,by ="schoolcode")
datstu$distance<-NA
datstu$distance<-sqrt((69.172*(datstu$ssslong.y-datstu$jsslong)*cos(datstu$jsslat/57.3))^2+(69.172*(datstu$ssslat.y-datstu$jsslat))^2)

#question4#
datstu <- read.csv("C:/Users/cuiti/Master Study/Second Semester/econometrics/dat/datstu.csv",na.string=c("","NA"))
datstu<-subset(datstu, !duplicated(score))
datstu<- datstu[!is.na(datstu$score), ]
c<-c("score","schoolcode1","choicepgm1")
rankchoice1<-datstu[c]
scoremin1<-min(rankchoice1$score)
scoremean1<-mean(rankchoice1$score)
d<-c("score","schoolcode2","choicepgm2")
rankchoice2<-datstu[d]
scoremin2<-min(rankchoice2$score)
scoremean2<-mean(rankchoice2$score)
f<-c("score","schoolcode3","choicepgm3")
rankchoice3<-datstu[f]
scoremin3<-min(rankchoice3$score)
scoremean3<-mean(rankchoice3$score)
g<-c("score","schoolcode4","choicepgm4")
rankchoice4<-datstu[g]
scoremin4<-min(rankchoice4$score)
scoremean4<-mean(rankchoice4$score)
h<-c("score","schoolcode5","choicepgm5")
rankchoice5<-datstu[h]
scoremin5<-min(rankchoice5$score)
scoremean5<-mean(rankchoice5$score)
j<-c("score","schoolcode6","choicepgm6")
rankchoice6<-datstu[j]
scoremin6<-min(rankchoice6$score)
scoremean6<-mean(rankchoice6$score)




#question5#
