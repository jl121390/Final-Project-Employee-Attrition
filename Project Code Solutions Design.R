#Project Code Solution Design
library(readxl)
library(carData)
library(car)
library(caret)
library(bootstrap)

histogram(EmployeeAttritionData$AttritionInd)


EmployeeAttritionData <- read.csv("C:/Users/Administrator/Documents/WA_Fn-UseC_-HR-Employee-Attrition.csv")
str(EmployeeAttritionData)

EmployeeAttritionData$AttritionInd <- rep(0, 1470)
for(i in 1:1470) {if (EmployeeAttritionData$Attrition[i] %in% c("Yes")) EmployeeAttritionData$AttritionInd[i] <- 1}

EmployeeAttritionData$TravelIndicator <- rep(0, 1470)
for(i in 1:1470) {if (EmployeeAttritionData$BusinessTravel[i] %in% c("Non-Travel")) EmployeeAttritionData$TravelIndicator[i] <- 0
if (EmployeeAttritionData$BusinessTravel[i] %in% c("Travel_Rarely")) EmployeeAttritionData$TravelIndicator[i] <- 1
if (EmployeeAttritionData$BusinessTravel[i] %in% c("Travel_Frequently")) EmployeeAttritionData$TravelIndicator[i] <- 2}


EmployeeAttritionData$DepartmentIndicator <- rep(0, 1470)
for(i in 1:1470) {if (EmployeeAttritionData$Department[i] %in% c("Sales")) EmployeeAttritionData$DepartmentIndicator[i] <- 1
if (EmployeeAttritionData$Department[i] %in% c("Research & Development")) EmployeeAttritionData$DepartmentIndicator[i] <- 2
if (EmployeeAttritionData$Department[i] %in% c("Human Resources")) EmployeeAttritionData$DepartmentIndicator[i] <- 3}


EmployeeAttritionData$EducationFieldIndicator <- rep(0, 1470)
for(i in 1:1470) {if (EmployeeAttritionData$EducationField[i] %in% c("Life Sciences")) EmployeeAttritionData$EducationFieldIndicator[i] <- 6
if (EmployeeAttritionData$EducationField[i] %in% c("Other")) EmployeeAttritionData$EducationFieldIndicator[i] <- 5
if (EmployeeAttritionData$EducationField[i] %in% c("Medical")) EmployeeAttritionData$EducationFieldIndicator[i] <- 4
if (EmployeeAttritionData$EducationField[i] %in% c("Marketing")) EmployeeAttritionData$EducationFieldIndicator[i] <- 3
if (EmployeeAttritionData$EducationField[i] %in% c("Human Resources")) EmployeeAttritionData$EducationFieldIndicator[i] <- 2
if (EmployeeAttritionData$EducationField[i] %in% c("Technical Degree")) EmployeeAttritionData$EducationFieldIndicator[i] <- 1}


EmployeeAttritionData$JobRoleIndicator <- rep(0, 1470)
for(i in 1:1470) {if (EmployeeAttritionData$JobRole[i] %in% c("Sales Executive")) EmployeeAttritionData$JobRoleIndicator[i] <- 9
if (EmployeeAttritionData$JobRole[i] %in% c("Research Scientist")) EmployeeAttritionData$JobRoleIndicator[i] <- 8
if (EmployeeAttritionData$JobRole[i] %in% c("Manufacturing Director")) EmployeeAttritionData$JobRoleIndicator[i] <- 7
if (EmployeeAttritionData$JobRole[i] %in% c("Healthcare Representative")) EmployeeAttritionData$JobRoleIndicator[i] <- 6
if (EmployeeAttritionData$JobRole[i] %in% c("Manager")) EmployeeAttritionData$JobRoleIndicator[i] <- 5
if (EmployeeAttritionData$JobRole[i] %in% c("Sales Representative")) EmployeeAttritionData$JobRoleIndicator[i] <- 4
if (EmployeeAttritionData$JobRole[i] %in% c("Research Director")) EmployeeAttritionData$JobRoleIndicator[i] <- 3
if (EmployeeAttritionData$JobRole[i] %in% c("Human Resources")) EmployeeAttritionData$JobRoleIndicator[i] <- 2
if (EmployeeAttritionData$JobRole[i] %in% c("Laboratory Technician")) EmployeeAttritionData$JobRoleIndicator[i] <- 1}

EmployeeAttritionData$GenderInd <- rep(0, 1470)
for(i in 1:1470) {if (EmployeeAttritionData$Gender[i] %in% c("Female")) EmployeeAttritionData$GenderInd[i] <- 1
if (EmployeeAttritionData$Gender[i] %in% c("Male")) EmployeeAttritionData$GenderInd[i] <- 2}

EmployeeAttritionData$MaritalStatusInd <- rep(0, 1470)
for(i in 1:1470) {if (EmployeeAttritionData$MaritalStatus[i] %in% c("Single")) EmployeeAttritionData$MaritalStatusInd[i] <- 1
if (EmployeeAttritionData$MaritalStatus[i] %in% c("Divorced")) EmployeeAttritionData$MaritalStatusInd[i] <- 2
if (EmployeeAttritionData$MaritalStatus[i] %in% c("Married")) EmployeeAttritionData$MaritalStatusInd[i] <- 3}


EmployeeAttritionData$OvertimeInd <- rep(0, 1470)
for(i in 1:1470) {if (EmployeeAttritionData$OverTime[i] %in% c("Yes")) EmployeeAttritionData$OvertimeInd[i] <- 1}



unique(EmployeeAttritionData$Department)
unique(EmployeeAttritionData$EducationField)
unique(EmployeeAttritionData$JobRole)
unique(EmployeeAttritionData$Gender)
unique(EmployeeAttritionData$MaritalStatus)
unique(EmployeeAttritionData$OverTime)

#MonthlyIncome, Monthly Rate, Hourly Rate and  and Daily Rate all say the same thing so we will just keep monthly rate
#Employee count is always one, over18 is always Yes and standard hours is always 80 so we will remove them as well
#We will also remove all the of the character data and replace them with indicators
str(EmployeeAttritionData)
ncol(EmployeeAttritionData)
EmployeeAttritionData <- subset(EmployeeAttritionData, select = -c(2,3,4,5,8,9,12,13,16,18,20,22,23,27))

data.frame(colnames(EmployeeAttritionData)) 

summary(EmployeeAttritionData)




######Data analysis

scatterplotMatrix(EmployeeAttritionData[,c(2,9,10,11,12,13,14,15,16,17,18)],pch=19)

library(psych)


pairs.panels(EmployeeAttritionData[,c(2,9,10,11,12,13,14,15,16,17,18)])


cor(EmployeeAttritionData$AttritionInd,EmployeeAttritionData[,c(11,13,14,15)])

str(EmployeeAttritionData)

head(EmployeeAttritionData)


cor(EmployeeAttritionData$JobSatisfaction,EmployeeAttritionData$AttritionInd)
#We can see a clear negative correlation between job satisfcation and attrition, as more satisfied employees are less likely to leave

cor(EmployeeAttritionData$TravelIndicator,EmployeeAttritionData$AttritionInd)
#We also notice a positive correlation between travel and attrition, as employees who are required to travel more frequently are more likely to leave

head(EmployeeAttritionData)

ncol(EmployeeAttritionData)

heatmap(cor(EmployeeAttritionData), Rowv = NA, Colv = NA)

library(gplots)
heatmap.2(cor(EmployeeAttritionData[,c(22,3,4,5,6,7,8,9,10)]), Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          cellnote = round(cor(EmployeeAttritionData),2), 
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))


heatmap.2(cor(EmployeeAttritionData[,c(22,11,12,13,14,15,16,17,18,19)]), Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          cellnote = round(cor(EmployeeAttritionData),2), 
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))

heatmap.2(cor(EmployeeAttritionData[,c(22,20,21,23,24,25,26)]), Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          cellnote = round(cor(EmployeeAttritionData),2), 
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))

heatmap.2(cor(EmployeeAttritionData[,c(22,27,28,29)]), Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          cellnote = round(cor(EmployeeAttritionData),2), 
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))


#########Neural Network application########

library(neuralnet)
library(nnet)
library(caret)

data.frame(colnames(train.index)) 


set.seed(1)  # set seed for reproducing the partition
train.index <- sample(c(1:1470), 600)
selected.var <- c(22,10, 8,26)
train.df <- EmployeeAttritionData[train.index, selected.var]
valid.df <- EmployeeAttritionData[-train.index, selected.var]


trainData <- (EmployeeAttritionData$GenderInd,EmployeeAttritionData$JobSatisfaction)

EmployeeAttritionData$AttritionInd

EmployeeAttritionData$Leave <- EmployeeAttritionData$AttritionInd==1
EmployeeAttritionData$Stay <- EmployeeAttritionData$AttritionInd==0

#Companiesworked,#yearsatcompany,yearsincurrentrole,maritalstatus

EmployeeAttritionData$JobSatisfaction

library(dplyr)
EmployeeAttritionData$ID <- 1:nrow(EmployeeAttritionData)
train <- EmployeeAttritionData %>% dplyr::sample_frac(.75)
validation  <- dplyr::anti_join(EmployeeAttritionData, train, by = 'ID')

nn <- neuralnet(Leave ~ GenderInd+JobSatisfaction+NumCompaniesWorked + OvertimeInd, linear.output = F ,data = train, hidden = 2)

nn$weights
prediction(nn)
plot(nn, rep="best")

library(caret)
predict <- neuralnet::compute(nn,data.frame(validation$NumCompaniesWorked,validation$JobSatisfaction,validation$GenderInd,validation$OvertimeInd))
predicted.class = ifelse(predict$net.result[,1]>.1,1,0)

table(predicted.class,validation$AttritionInd)

#########Neural Network application########


####Naive Bayes Algorithmo#######

library(e1071)

library(dplyr)

install.packages("survey")
library(survey)

EmployeeAttritionData

str(EmployeeAttritionData)

data.frame(colnames(EmployeeAttritionData)) 



selected.var <- c(22,10, 8,26)
train.index <- sample(c(1:dim(EmployeeAttritionData)[1]), dim(EmployeeAttritionData)[1]*0.5)
train.df <- EmployeeAttritionData[train.index, selected.var]
valid.df <- EmployeeAttritionData[-train.index, selected.var]


train.df$AttritionInd <- sample( c(1,0),size = nrow(train.df), replace = TRUE, prob = c(1/2,1/2) )

#GenderInd+JobSatisfaction+NumCompaniesWorked + OvertimeInd

delays.nb <- naiveBayes(as.factor(train.df$AttritionInd) ~., data = train.df)



library(caret)
# training
pred.class <- predict(delays.nb, newdata = train.df)
confusionMatrix(pred.class, as.factor(train.df$AttritionInd) )

#.8960 specificity on people who will likely leave, when including  Gender, Number of companies worked, job satisfaction and gender

levels(pred.class)


# validation
pred.class <- predict(delays.nb, newdata = valid.df)
options(scipen=999, digits = 0)
confusionMatrix (pred.class, as.factor(valid.df$AttritionInd))



library(forecast)
valid.df <- data.frame(valid.df)
NROW(forecast(pred.class))
NROW(valid.df$AttritionInd)
pred.class <- as.numeric(pred.class)
valid.df$AttritionInd <- as.numeric(valid.df$AttritionInd)
accuracy(pred.class,valid.df$AttritionInd)
options(scipen=999, digits = 10)




#######Random forest#######
install.packages("randomForest")
library(randomForest)


#selected.var <- c(22,10, 8,26)
train.index <- sample(c(1:dim(EmployeeAttritionData)[1]), dim(EmployeeAttritionData)[1]*0.5)
train.df <- EmployeeAttritionData[train.index, ]
valid.df <- EmployeeAttritionData[-train.index, ]

train.df$AttritionInd <- sample( c(1,0),size = nrow(train.df), replace = TRUE, prob = c(1/2,1/2) )


train.df$Leave <- train.df$AttritionInd==1
train.df$Stay <- train.df$AttritionInd==0
valid.df$Leave <- valid.df$AttritionInd==1
valid.df$Stay <- valid.df$AttritionInd==0


EmployeeAttritionData[,c(1,2,3)]

## random forest
rf <- randomForest(as.factor(train.df$Leave) ~ train.df$DistanceFromHome+train.df$Education+train.df$EnvironmentSatisfaction+train.df$JobInvolvement+train.df$JobLevel+train.df$JobSatisfaction+train.df$MonthlyIncome+train.df$NumCompaniesWorked+train.df$PercentSalaryHike+train.df$PerformanceRating+train.df$RelationshipSatisfaction
                   +train.df$StockOptionLevel+train.df$TotalWorkingYears+train.df$TrainingTimesLastYear+train.df$WorkLifeBalance+train.df$YearsAtCompany+train.df$YearsInCurrentRole+train.df$YearsSinceLastPromotion+train.df$YearsWithCurrManager+train.df$TravelIndicator+train.df$DepartmentIndicator+train.df$DepartmentIndicator+train.df$GenderInd+train.df$MaritalStatusInd+train.df$OvertimeInd
                   , v = train.df, ntree = 1000, 
                   mtry = 10, nodesize = 5, importance = TRUE)  



rf <- randomForest(as.factor(train.df$Leave) ~ 
train.df$YearsInCurrentRole+train.df$YearsSinceLastPromotion+train.df$YearsAtCompany
+train.df$YearsWithCurrManager+train.df$MonthlyIncome+train.df$TotalWorkingYears, v = train.df, ntree = 500, 
 mtry = 4, nodesize = 5, importance = TRUE)  



## variable importance plot
varImpPlot(rf, type = 1)

NROW(valid.df)
## confusion matrix
rf.pred <- predict(rf, valid.df$Leave)
confusionMatrix(rf.pred,as.factor(valid.df$Leave))
