install.packages('arules')
install.packages('arulesViz')
library(arules)
library(arulesViz)

attrition_data<-read.csv("employee_attrition.csv",sep = ",",header = T)

View(attrition_data)

str(attrition_data)

summary(attrition_data)

attrition_data$EmployeeCount<-NULL
attrition_data$Over18<-NULL
attrition_data$StandardHours<-NULL
attrition_data$EmployeeNumber<-NULL
attrition_data$NumCompaniesWorked<-NULL
attrition_data$DistanceFromHome<-NULL
attrition_data$TrainingTimesLastYear<-NULL
attrition_data$StockOptionLevel<-NULL


attrition_data$JobSatisfaction <- as.factor(attrition_data$JobSatisfaction)
attrition_data$RelationshipSatisfaction <- as.factor(attrition_data$RelationshipSatisfaction)
attrition_data$PerformanceRating <- as.factor(attrition_data$PerformanceRating)
attrition_data$JobLevel <- as.factor(attrition_data$JobLevel)
attrition_data$HourlyRate <- as.factor(attrition_data$HourlyRate)
attrition_data$StockOptionLevel <- as.factor(attrition_data$StockOptionLevel)
attrition_data$Gender <- as.factor(attrition_data$Gender)
attrition_data$PercentSalaryHike <- as.factor(attrition_data$PercentSalaryHike)
attrition_data$YearsSinceLastPromotion <- as.factor(attrition_data$YearsSinceLastPromotion)
attrition_data$DailyRate<-as.factor(attrition_data$DailyRate)
attrition_data$MonthlyIncome <- as.factor(attrition_data$MonthlyIncome)
attrition_data$MonthlyRate <- as.factor(attrition_data$MonthlyRate)
attrition_data$YearsInCurrentRole<-as.factor(attrition_data$YearsInCurrentRole)
attrition_data$YearsWithCurrManager<-as.factor(attrition_data$YearsWithCurrManager)

summary(attrition_data)

job_level<-as.character(attrition_data$JobLevel)
job_level[is.na(job_level)]<-'Not Known'
attrition_data$JobLevel<-as.factor(job_level)

percent_hike<-as.character(attrition_data$PercentSalaryHike)
percent_hike[is.na(percent_hike)]<-'Not Known'
attrition_data$PercentSalaryHike<-as.factor(percent_hike)

performance<-as.character(attrition_data$PerformanceRating)
performance[is.na(performance)]<-'Not Known'
attrition_data$PerformanceRating<-as.factor(performance)

relation<-as.character(attrition_data$RelationshipSatisfaction)
relation[is.na(relation)]<-'Not Known'
attrition_data$RelationshipSatisfaction<-as.factor(relation)

tot<-as.character(attrition_data$TotalWorkingYears)
tot[is.na(tot)]<-'Not Known'
attrition_data$TotalWorkingYears<-as.factor(tot)

years<-as.character(attrition_data$YearsSinceLastPromotion)
years[is.na(years)]<-'Not Known'
attrition_data$YearsSinceLastPromotion<-as.factor(years)

summary(attrition_data)

# Column 'Age'
hist(attrition_data$Age,xlab = 'Age',main = 'Histogram of Age',col = blues9)
attrition_data$Age_Group<-attrition_data$Age
attrition_data$Age_Group<-ifelse((attrition_data$Age>=18 & attrition_data$Age<=32),'Young',attrition_data$Age_Group)
attrition_data$Age_Group<-ifelse((attrition_data$Age>32 & attrition_data$Age<=47),'Medium',attrition_data$Age_Group)
attrition_data$Age_Group<-ifelse((attrition_data$Age>47 & attrition_data$Age<=60),'Old',attrition_data$Age_Group)
attrition_data$Age_Group<-as.factor(attrition_data$Age_Group)
attrition_data$Age<-NULL

# Column 'Environment Satisfaction'
attrition_data$Env_Group<-attrition_data$EnvironmentSatisfaction
attrition_data$Env_Group<- ifelse((attrition_data$Env_Group)>=3,"High","Low")
attrition_data$Env_Group<-as.factor(attrition_data$Env_Group)
attrition_data$EnvironmentSatisfaction<-NULL

# Column 'Years at Company'
attrition_data$Years_Group<-attrition_data$YearsAtCompany
attrition_data$Years_Group<- ifelse((attrition_data$Years_Group)>=10,"High","Low")
attrition_data$Years_Group<-as.factor(attrition_data$Years_Group)
attrition_data$YearsAtCompany<-NULL

# Column 'Gender'
table(attrition_data$Gender,attrition_data$Attrition)

# Column 'Education'
attrition_data$Edu_Group<-attrition_data$Education
attrition_data$Edu_Group<- ifelse((attrition_data$Edu_Group)>=3,"High","Low")
attrition_data$Edu_Group<-as.factor(attrition_data$Edu_Group)
attrition_data$Education<-NULL
table(attrition_data$Edu_Group,attrition_data$Attrition)
# The attrition = Yes count for highly educated people is less when compared to others.

# Column 'Work Life Balance'
attrition_data$WorkLife_Group<-attrition_data$WorkLifeBalance
attrition_data$WorkLife_Group<- ifelse((attrition_data$WorkLife_Group)>=3,"High","Low")
attrition_data$WorkLife_Group<-as.factor(attrition_data$WorkLife_Group)
attrition_data$WorkLifeBalance<-NULL

# Column 'Business Travel'
table(attrition_data$Attrition,attrition_data$BusinessTravel)
# People who do not travel have the lowest attrition percentage.
# Attrition Rate for people who do not travel= 10%
# Attrition Rate for people who frequently travel= 24.22%
# Attrition Rate for people who rarely travel= 14.18%

# Column 'Job Involvement'
table(attrition_data$JobInvolvement,attrition_data$Attrition)
attrition_data$JobInvolvement_Group<-attrition_data$JobInvolvement
attrition_data$JobInvolvement_Group<- ifelse((attrition_data$JobInvolvement_Group)>=3,"High","Low")
attrition_data$JobInvolvement_Group<-as.factor(attrition_data$JobInvolvement_Group)
attrition_data$JobInvolvement<-NULL
# People with more job involvement have a less attrition rate.


write.csv(attrition_data,file = "Final Attrition.csv")
rules_1<-apriori(attrition_data)
rules_1 <- sort (rules_1, by="confidence",decreasing=TRUE)
inspect(head(rules_1,5) )
plot(rules_1,jitter=0)


rules_2 <- apriori(attrition_data, parameter = list(supp = 0.5, conf = 0.5))
rules_2 <- sort (rules_2, by="confidence",decreasing=TRUE)
inspect(head(rules_2,5) )
plot(rules_2,jitter=0)


rules_3 <- apriori(attrition_data, parameter=list (supp=0.1,conf = 0.5, maxtime = 10), appearance = list (default="lhs",rhs=c("Attrition=Yes","Attrition=No")),control = list(verbose = F))
rules_3 <- sort (rules_3, by="confidence",decreasing=TRUE)
inspect(head(rules_3,5))
plot(rules_3,jitter=0)

View(attrition_data[,1])
