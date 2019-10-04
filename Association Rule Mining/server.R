
library(shiny)
library(arules)
library(arulesViz)
library(rsconnect)

rsconnect::setAccountInfo(name='agarge',
                          token='F35BA5C69EA903D017E3DD8253BFBABB',
                          secret='<SECRET>')




shinyServer(
  function(input,output,session)
  {
    
    output$myPlot <- renderPlot({
      
      emp <- read.csv("employee_attrition.csv",sep="," ,header=TRUE,stringsAsFactors = FALSE)
      emp <- data.frame(emp)
      del <- c(9,10,27,22)
      emp <- emp[,-del]
      clean_emp <- na.omit(emp)
      del1 <- c(13,19,21,24,25,26,28,29,30,31)
      clean_emp <- clean_emp[,-del1]
      
      giveLevel<-function(x)
      {
        BucketsClean <- replicate(length(x),"Doctor")
        BucketsClean[x==1] <- "Below College"
        BucketsClean[x==2] <- "College"
        BucketsClean[x==3] <- "Bachelor"
        BucketsClean[x==4] <- "Master"
        return (BucketsClean)
      }
      
      giveLevel1<-function(x)
      {
        BucketsClean <- replicate(length(x),"Low")
        BucketsClean[x==2] <- "Medium"
        BucketsClean[x==3] <- "High"
        BucketsClean[x==4] <- "Very High"
        return (BucketsClean)
      }
      
      giveLevel2<-function(x)
      {
        BucketsClean <- replicate(length(x),"Seniors")
        BucketsClean[x>18 & x<38] <- "Young"
        BucketsClean[x>37 & x<54] <- "Adults"
        
        return (BucketsClean)
      }
      
      giveLevel3<-function(x)
      {
        BucketsClean <- replicate(length(x),"Low")
        BucketsClean[x>=500 & x<=1100] <- "Medium"
        BucketsClean[x>1100] <- "High"
        return (BucketsClean)
      }
      
      giveLevel4<-function(x)
      {
        BucketsClean <- replicate(length(x),"Near")
        BucketsClean[x>=12 & x<=18] <- "Average"
        BucketsClean[x>=18] <- "Far"
        return (BucketsClean)
      }
      
      giveLevel5<-function(x)
      {
        BucketsClean <- replicate(length(x),"Outstanding")
        BucketsClean[x==1] <- "Low"
        BucketsClean[x==2] <- "Good"
        BucketsClean[x==3] <- "Excellent"
        return (BucketsClean)
      }
      
      
      
      giveLevel6<-function(x)
      {
        BucketsClean <- replicate(length(x),"Very High")
        BucketsClean[x==1] <- "Bad"
        BucketsClean[x==2] <- "Medium"
        BucketsClean[x==3] <- "High"
        return (BucketsClean)
      }
      
      giveLevel7<-function(x)
      {
        BucketsClean <- replicate(length(x),"High")
        BucketsClean[x<50] <- "Low"
        BucketsClean[x>50 & x<80] <- "Average"
        return (BucketsClean)
      }
      
      giveLevel8<-function(x)
      {
        BucketsClean <- replicate(length(x),"High")
        BucketsClean[x<10000] <- "Low"
        BucketsClean[x>10000 & x<20000] <- "Average"
        return (BucketsClean)
      }
      
      
      giveLevel9<-function(x)
      {
        BucketsClean <- replicate(length(x),"High")
        BucketsClean[x<3000] <- "Low"
        BucketsClean[x>3000 & x<7000] <- "Average"
        return (BucketsClean)
      }
      
      clean_emp$Education <- giveLevel(clean_emp$Education) 
      clean_emp$EnvironmentSatisfaction <- giveLevel1(clean_emp$EnvironmentSatisfaction)
      clean_emp$JobSatisfaction <- giveLevel1(clean_emp$JobSatisfaction)
      clean_emp$RelationshipSatisfaction <- giveLevel1(clean_emp$RelationshipSatisfaction)
      clean_emp$JobInvolvement <- giveLevel1(clean_emp$JobInvolvement)
      clean_emp$Age <- giveLevel2(clean_emp$Age)
      clean_emp$DailyRate <- giveLevel3(clean_emp$DailyRate)
      clean_emp$DistanceFromHome <- giveLevel4(clean_emp$DistanceFromHome)                          
      clean_emp$PerformanceRating <- giveLevel5(clean_emp$PerformanceRating)
      clean_emp$WorkLifeBalance <- giveLevel6(clean_emp$WorkLifeBalance)
      clean_emp$HourlyRate <- giveLevel7(clean_emp$HourlyRate)
      clean_emp$MonthlyRate <- giveLevel8(clean_emp$MonthlyRate)
      clean_emp$MonthlyIncome <- giveLevel9(clean_emp$MonthlyIncome)
      
      
      clean_emp$Age <- as.factor(clean_emp$Age)
      clean_emp$Attrition <- as.factor(clean_emp$Attrition)
      clean_emp$BusinessTravel <- as.factor(clean_emp$BusinessTravel)
      clean_emp$DailyRate <- as.factor(clean_emp$DailyRate)
      clean_emp$Department <- as.factor(clean_emp$Department)
      clean_emp$DistanceFromHome <- as.factor(clean_emp$DistanceFromHome)
      clean_emp$Education <- as.factor(clean_emp$Education)              
      clean_emp$EducationField <- as.factor(clean_emp$EducationField)     
      clean_emp$EnvironmentSatisfaction <- as.factor(clean_emp$EnvironmentSatisfaction)
      clean_emp$Gender <- as.factor(clean_emp$Gender)        
      clean_emp$HourlyRate <- as.factor(clean_emp$HourlyRate)           
      clean_emp$JobInvolvement <- as.factor(clean_emp$JobInvolvement)     
      clean_emp$JobRole <- as.factor(clean_emp$JobRole)           
      clean_emp$JobSatisfaction <- as.factor(clean_emp$JobSatisfaction)       
      clean_emp$MaritalStatus <- as.factor(clean_emp$MaritalStatus)   
      clean_emp$MonthlyIncome <- as.factor(clean_emp$MonthlyIncome)         
      clean_emp$MonthlyRate <- as.factor(clean_emp$MonthlyRate)         
      clean_emp$OverTime <- as.factor(clean_emp$OverTime)
      clean_emp$PerformanceRating <- as.factor(clean_emp$PerformanceRating)
      clean_emp$RelationshipSatisfaction <- as.factor(clean_emp$RelationshipSatisfaction)
      clean_emp$WorkLifeBalance <- as.factor(clean_emp$WorkLifeBalance)
      
      
      s <- input$support
      c <- input$confidence
      
      
      rules_1 <- apriori(clean_emp, parameter = list(supp = s , conf = c))
      rules_1 <- sort (rules_1, by="confidence",decreasing=TRUE)
      inspect(rules_1)
      plot(rules_1,jitter=0)
  
      
    })
    
  }
)

