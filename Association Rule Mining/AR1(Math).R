install.packages("arules")
library(arules)

setwd("C:/Users/mxs15/Desktop/2016-Summer-Courses/Advanced Statistic/Assignment1")

d1=read.table("student-mat.csv",sep=";",header=TRUE)

#1.G1-G2-G3
dat1=data.frame(d1[,31:33])
summary(dat1)
#change grades into letter grades
dat1[["G1"]]=cut(dat1[["G1"]],c(0,10,15,20),labels = c("C","B","A"))
dat1[["G2"]]=cut(dat1[["G2"]],c(-1,10,15,20),labels = c("C","B","A"))
dat1[["G3"]]=cut(dat1[["G3"]],c(-1,10,15,20),labels = c("C","B","A"))
#mining rules
rules1.1=apriori(dat1, parameter = list(supp=0.05,conf=0.7))
inspect(rules1.1)
#filter rules has lift larger than 2
rules1.1.1=subset(rules1.1,subset = lift>2)
inspect(rules1.1.1)

#2.Reason-Higher-StudyTime-AvgGrade
dat2=data.frame(d1$reason,d1$higher,d1$studytime,AvgGrade=rowMeans(d1[,31:33]))
dat2
summary(dat2)
dat2[["AvgGrade"]]=cut(dat2[["AvgGrade"]],c(0,10,15,20),labels = c("C","B","A"))
names(dat2)=c("Reason","Higher","StudyTime","AvgGrade")
dat2$StudyTime=as.factor(dat2$StudyTime)
rules2=apriori(dat2,parameter = list(supp=0.01,conf=0.7))
inspect(rules2)
rules2.2=subset(rules2,subset=lift>2)
inspect(rules2.2)

#3.Sex-Age-StudyTime-Absence
dat3=data.frame(d1[,"sex"],d1[,"age"],d1[,"studytime"],d1[,"absences"])
names(dat3)=c("Sex","Age","StudyTime","Absences")
summary(dat3)
dat3$Age=cut(dat3$Age,c(0,17,26),labels = c("Teen","Adult"))
dat3$StudyTime=as.factor(dat3$StudyTime)
dat3$Absences=cut(dat3$Absences,c(-1,5,20,80),labels=c("low","high","very high"))
rules3=apriori(dat3,parameter = list(supp=0.03,conf=0.6))
inspect(rules3)
rules3.1=subset(rules3,subset=lift>1.5)
inspect(rules3.1)

#Sex-Age-AvgGrade
#dat4=data.frame(d1$sex,d1$age,AvgGrade=rowMeans(d1[,31:33]))
#dat4[["AvgGrade"]]=cut(dat4[["AvgGrade"]],c(0,10,15,20),labels = c("C","B","A"))
#summary(dat4)
#names(dat4)=c("Sex","Age","AvgGrade")
#dat4$Age=cut(dat4$Age,c(0,17,26),labels = c("Teen","Adult"))
#rules4=apriori(dat4,parameter = list(supp=0.05,conf=0.6))
#inspect(rules4)

#4.StudyTime-Internet-AvgGrade
dat4=data.frame(d1$studytime,d1$internet,AvgGrade=rowMeans(d1[,31:33]))
names(dat4)=c("StudyTime","Internet","AvgGrade")
dat4[["AvgGrade"]]=cut(dat4[["AvgGrade"]],c(0,10,15,20),labels = c("C","B","A"))
dat4$StudyTime=as.factor(dat4$StudyTime)
summary(dat4)
rules4=apriori(dat4,parameter = list(supp=0.05,conf=0.6))
inspect(rules4)
rules4.1=subset(rules4,subset=lift>1.2)
inspect(rules4.1)

#5.Fedu-Medu-Guardian
dat5=data.frame(d1$Fedu,d1$Medu,d1$guardian)
names(dat5)=c("Fedu","Medu","Guardian")
dat5$Fedu=as.factor(dat5$Fedu)
dat5$Medu=as.factor(dat5$Medu)
rules5=apriori(dat5,parameter = list(supp=0.05,conf=0.7))
inspect(rules5)
rules5.1=subset(rules5,subset=lift>1.15)
inspect(rules5.1)

#6.Sex-Age-Rel-Goout
dat6=data.frame(d1$sex,d1$age,d1$romantic,d1$freetime)
names(dat6)=c("Sex","Age","Romantic","FreeTime")
dat6$Age=cut(dat6$Age,c(0,17,26),labels = c("teen","adult"))
summary(dat6)
dat6$FreeTime=as.factor(dat6$FreeTime)
rules6=apriori(dat6,parameter = list(supp=0.03,conf=0.7))
inspect(rules6)
rules6.1=subset(rules6,subset=lift>1.5)
inspect(rules6.1)

#7.Sex-Age-TotalAlcho-Health
dat7=data.frame(d1$sex,d1$age,d1$health,Alcho=rowSums(d1[,27:28]))
summary(dat7)
names(dat7)=c("Sex","Age","Health","Alcho")
dat7$Age=cut(dat7$Age,c(0,17,26),labels = c("teen","adult"))
dat7$Alcho=cut(dat7$Alcho,c(0,4,7,11), labels = c("low","normal","high"))
dat7$Health=cut(dat7$Health,c(0,2,4,6),labels = c("bad","good","perfect"))
rules7=apriori(dat7,parameter = list(supp=0.05,conf=0.7))
inspect(rules7)
rules7.1=subset(rules7,subset=lift>1.2)
inspect(rules7.1)

#8.Medu,Fedu,AvgGrades
dat8=data.frame(d1$Medu,d1$Fedu,AvgGrades=rowMeans(d1[,31:33]))
summary(dat8)
names(dat8)=c("Medu","Fedu","AvgGrades")
dat8$AvgGrades=cut(dat8$AvgGrades,c(0,10,15,20),labels = c("C","B","A"))
dat8$Medu=cut(dat8$Medu,c(-1,2,5),labels = c("low","high"))
dat8$Fedu=cut(dat8$Fedu,c(-1,2,5),labels = c("low","high"))
rules8=apriori(dat8,parameter = list(supp=0.05,conf=0.7))
inspect(rules8)

#9.PEdu,AvgGrades
dat9=data.frame(d1$famrel,PEdu=rowSums(d1[,7:8]),AvgGrades=rowMeans(d1[,31:33]))
summary(dat9)
names(dat9)=c("FamRel","PEdu","AvgGrades")
dat9$AvgGrades=cut(dat9$AvgGrades,c(0,10,15,20),labels = c("C","B","A"))
dat9$PEdu=cut(dat9$PEdu,c(0,3,6,9),labels = c("low","middle","high"))
dat9$FamRel=cut(dat9$FamRel,c(0,2,4,6),labels = c("bad","good","perfect"))
rules9=apriori(dat9,parameter = list(supp=0.03,conf=0.7))
inspect(rules9)
