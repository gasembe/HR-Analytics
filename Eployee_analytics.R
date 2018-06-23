library(tidyverse)
library(MASS)
library(car)
library(cowplot)


############################################################################################
# DATA LOAD
############################################################################################

# Load data general_data.csv
empData <- read.csv("general_data.csv", header = TRUE) #4410 observation of 24 variables # 4410 observation of 24 variables

# Load manager_survey_data.csv
empMgrSurvey <- read.csv("manager_survey_data.csv", header = TRUE, stringsAsFactors = FALSE) #4410 obsrvations of 3 variables

# Load employee_survey_data.csv
empSurvey <- read.csv("employee_survey_data.csv", header = TRUE, stringsAsFactors = FALSE) #4410 observations of 4 variables 

# Load in_time.csv
inTime <- read.csv("in_time.csv", header = TRUE, stringsAsFactors = FALSE) # 4410 observations of 262 variables

# Load out_time.csv
outTime <- read.csv("out_time.csv", header = TRUE, stringsAsFactors = FALSE) # 4410 observations of 262 variables

#############################################################################################
# DATA PREPARATION AND EDA
#############################################################################################

## structure understanding empData (general_data.csv) 

str(empData)
summary(empData)
# NumCompaniesWorked and TotalWorkingYears contains 19 and 9 missing values respectively
# Missing Value Treatment
# will replace them with their respective medians - which are closer to the means

empData$TotalWorkingYears <- ifelse(is.na(empData$TotalWorkingYears), median(empData$TotalWorkingYears, na.rm=T), empData$TotalWorkingYears)

empData$NumCompaniesWorked <- ifelse(is.na(empData$NumCompaniesWorked), median(empData$NumCompaniesWorked, na.rm = T), empData$NumCompaniesWorked)

## structure understanding empData (employee_survey_data.csv) 
str(empSurvey)
summary(empSurvey)

# EnvironmentSatisfaction ,JobSatisfaction and  WorkLifeBalance have 25 ,20  and 38 missing values/na 
# we replace them with the median, have chosen to imputate missing values with meadian because it is close to the mean as per the summary above
# and the mean of the variable in both cases is decimal wheres all the values of the 3 variables are whole number integers. 
empSurvey$EnvironmentSatisfaction <- ifelse(is.na(empSurvey$EnvironmentSatisfaction), median(empSurvey$EnvironmentSatisfaction, na.rm = T), empSurvey$EnvironmentSatisfaction)

empSurvey$JobSatisfaction <- ifelse(is.na(empSurvey$JobSatisfaction), median(empSurvey$JobSatisfaction, na.rm = T), empSurvey$JobSatisfaction )

empSurvey$WorkLifeBalance <- ifelse(is.na(empSurvey$WorkLifeBalance), median(empSurvey$WorkLifeBalance, na.rm = T), empSurvey$WorkLifeBalance)

## structure understanding empData (manager_survey_data.csv) 
str(empMgrSurvey)
summary(empMgrSurvey)


# Manager survey is a complete data set with no missing values, as such no data cleanin operation is 
# required on the data set

str(inTime)
summary(inTime)

# OBSERVATION
# Intime data set can be thought of as a register of the time the employee reported to work on a particular date
# Some columns are 100% na, we assume these to be holidays when employees were not required to report to work
# Some records have na values for certain days for specific employee, We assume this to be the days the employee did not report to work

#Required operations
# intime has some columns with 100% missing values these columns will be dropped
# convert the dataframe from wide format to long format
# convert the output dataframe date value to standard R time

# Drop columns with 70% NAs
colSums(is.na(inTime))
inTime <- inTime[,colMeans(is.na(inTime)) <= 0.30] # 12 columns dropped 

#The first column on inTime data file has no column name, we assume that this is the employeeId and name it as such
colnames(inTime)[1] <- "EmployeeID"
#convert from wide to long
intimeLong <- gather(inTime, key = DateKey, value = TimeIn , 2:250)

# convert Timein  variable to standard R date time
intimeLong$TimeIn <- as.POSIXct(intimeLong$TimeIn, format = "%Y-%m-%d %H:%M:%S" )

#Out_Time clensing
# remove 70 % or more na columns
colSums(is.na(outTime))
outTime <- outTime[,colMeans(is.na(outTime)) <= 0.30] # 12 columns dropped

#The first column on OutTime data file has no column name, we assume that this is the employeeId and name it as such

colnames(outTime)[1] <- "EmployeeID"
outTimeLong <- gather(outTime, key = DateKey, value = TimeOut, 2:250)

#Convert timeOut to standard dateTime variable
outTimeLong$TimeOut <- as.POSIXct(outTimeLong$TimeOut, format = "%Y-%m-%d %H:%M:%S")

# merge the two data frames inTimelong and outTimelong to calculate hour worked per day per employee

empTime <- merge(intimeLong, outTimeLong, by = c("EmployeeID", "DateKey"),
                 by.y = c("EmployeeID", "DateKey"))


#Because inTime and OutTime are indicative times when the employee reported and left work on a given dy we
# create a derived metric hoursWorked for each day by substracting timeIn from Time out 

empTime$HoursWorked  <-  empTime$TimeOut - empTime$TimeIn


# We consider days the emloyee was off as those records with na, We therefore create a dataframe of absentism to
# derive a new metric  of the number of days the employee did not report to work

empAbsentism <- filter(empTime, is.na(empTime$HoursWorked))


#calculate the number of days each employee has been off duty
empAbsentismGrp <- group_by(empAbsentism, EmployeeID)
empAbsentismSummary <- summarise(empAbsentismGrp, DaysOff = n())


# calculate average time each employee work everyday 
avgWorkTimeGrp <- group_by(empTime, EmployeeID)
empAvgWorkTimeSummary <- summarise(avgWorkTimeGrp, MeanHours = mean(as.numeric(HoursWorked), na.rm = TRUE))

# Merge final dataset 
empTimeFinal <- merge(empAvgWorkTimeSummary, empAbsentismSummary, by = "EmployeeID")


empDataFinal <- empData %>%  merge(empTimeFinal,by = "EmployeeID") %>% merge(empSurvey, by="EmployeeID") %>% merge(empMgrSurvey,by="EmployeeID")
empDataFinal$MeanHours <- format(round(empDataFinal$MeanHours,0))

empDataFinal$MeanHours <- as.numeric(empDataFinal$MeanHours)

#Create new derived metric StandardHourDiff, which is the difference between standardHours and calculated menHours per employee
empDataFinal$StandardHoursDiff <- empDataFinal$MeanHours - empDataFinal$StandardHours

# check for outliers
sapply(empDataFinal[,c("MonthlyIncome","PerformanceRating" )], 
       function(x) quantile(x,seq(0,1,.01),na.rm = T)) 
# No outliers detected

# Master dataset for EDA
str(empDataFinal)  


#############################################################################################
# DATA UNDERSTANDING - EDA
#############################################################################################
library(cowplot)

# we carry out univariate and bivariate analysis by ploting a few charts to get a general undrstanding of the data and try draw some initial hypothesis
# of the variables that are stronger indicator of employee attrition


# Continous variable that are  highest  contributors to employee attrition
## Grid 1 - univariate analysis - continous variable
plot_grid(
          ggplot(empDataFinal, aes(x = empDataFinal$Age )) + geom_bar(),
          ggplot(empDataFinal, aes(x = empDataFinal$Education, fill = empDataFinal$Attrition)) + geom_bar(position = "fill"),
          ggplot(empDataFinal, aes(x = empDataFinal$EmployeeCount, fill = empDataFinal$Attrition)) + geom_bar(),
          ggplot(empDataFinal, aes(x = empDataFinal$JobLevel, fill = empDataFinal$Attrition)) + geom_bar(position = "fill"),
          ggplot(empDataFinal, aes(x = empDataFinal$MonthlyIncome, fill = empDataFinal$Attrition)) + geom_histogram(bins = 20),
          ggplot(empDataFinal, aes(x = empDataFinal$NumCompaniesWorked, fill = empDataFinal$Attrition)) + geom_bar()
          
)
# Employees in job level 2 and 4 are slightly likely to leave the company
# Distribution of employees by age in the company assumes a standard normal distribution


## Grid 2 - univariate analysis - continous variable
plot_grid(
  ggplot(empDataFinal, aes(x = empDataFinal$NumCompaniesWorked, fill = empDataFinal$Attrition)) + geom_bar( position = "fill"),
  ggplot(empDataFinal, aes(x = empDataFinal$PercentSalaryHike, fill = empDataFinal$Attrition)) + geom_bar(position = "fill"),
  ggplot(empDataFinal, aes(x = empDataFinal$StockOptionLevel, fill = empDataFinal$Attrition)) + geom_bar(position = "fill"),
  ggplot(empDataFinal, aes(x = empDataFinal$TotalWorkingYears)) + geom_histogram(bins = 40),
  ggplot(empDataFinal, aes(x = empDataFinal$TrainingTimesLastYear, fill = empDataFinal$Attrition)) + geom_bar(position = "fill"),
  ggplot(empDataFinal, aes(x = empDataFinal$YearsAtCompany)) + geom_histogram(bins = 40)
  
)
# There is more employee attrition among employees with 
      ## Highest salary hike 25%, 
      ## employees who have served more than 5 years are likely to leave the company
      ## There are more staff with 10,6 and 1 years of working experience in the company
      ## Most employees have served less than 10 years in the company
      ## Employees who 0 training last year are more likely to leave than those who received atleast one training

## Grid 3 - univariate analysis - continous variable
plot_grid(
  ggplot(empDataFinal, aes(x = empDataFinal$YearsSinceLastPromotion, fill = empDataFinal$Attrition)) + geom_bar( position = "fill"),
  ggplot(empDataFinal, aes(x = empDataFinal$MeanHours, fill = empDataFinal$Attrition)) + geom_bar(position = "fill"),
  ggplot(empDataFinal, aes(x = empDataFinal$DaysOff, fill = empDataFinal$Attrition)) + geom_bar(position = "fill"),
  ggplot(empDataFinal, aes(x = empDataFinal$EnvironmentSatisfaction, fill = empDataFinal$Attrition)) + geom_bar(position = "fill"),
  ggplot(empDataFinal, aes(x = empDataFinal$JobSatisfaction, fill = empDataFinal$Attrition)) + geom_bar(position = "fill"),
  ggplot(empDataFinal, aes(x = empDataFinal$JobInvolvement, fill = empDataFinal$Attrition)) + geom_bar(position = "fill"),
  ggplot(empDataFinal, aes(x = empDataFinal$PerformanceRating, fill = empDataFinal$Attrition)) + geom_bar(position = "fill")
  
)

  # employees who work more than 8 hours are likely to leave the company
  # employees who have taken less off days more likely to leave the company
  # employees with 1 rating for EnviromentSatisfaction, JobSatisfaction and JobInvolvement have a higher chance of leaving.

  

## Grid 4 - Univariate analysis - categorical variables

plot_grid(
  ggplot(empDataFinal, aes(x = empDataFinal$Department, fill = empDataFinal$Attrition)) + geom_bar( position = "fill"),
  ggplot(empDataFinal, aes(x = empDataFinal$BusinessTravel, fill = empDataFinal$Attrition)) + geom_bar(position = "fill"),
  ggplot(empDataFinal, aes(x = empDataFinal$EducationField, fill = empDataFinal$Attrition)) + geom_bar(position = "fill"),
  ggplot(empDataFinal, aes(x = empDataFinal$Gender, fill=empDataFinal$Attrition)) + geom_bar(position = "fill"),
  ggplot(empDataFinal, aes(x = empDataFinal$JobRole, fill = empDataFinal$Attrition)) + geom_bar(position = "fill"),
  ggplot(empDataFinal, aes(x = empDataFinal$MaritalStatus, fill= empDataFinal$Attrition)) + geom_bar(position = "fill")
)
## Employee in human resources department are more likely to leave than other departments
## Employees who Travel frequently are most likely to leave and least likely are those not-travel
## human resource field employeees have a higher probability of leaving, Technical degree are least likely to leave
## Gender is not a good predictor of attrition, all have equal likelihood
## Research director has higher turnover than other roles
## Attrition is higher among single employees, Divorcees are least likely to leave

# TO DO BOX PLOTS - BIVARIATE ANALYSIS




############################################################################################
# MODELING
############################################################################################

# Model Selection
#-----------------------------------------------------
# For this task we will choose a logistics regression as it involves prediction of a binary dependent variable of an employee 
# leaving the company or not


# Variable transformation
#------------------------------------------------------
# The final dataset has a total of 5 categorical variables with more than two levels, namely 
# BusinessTravel, Department, EducationField , JobRole, MaritalStatus. In addition there are two variables with 2 factor levels (Attrition, Gender)
# and one 1 level factor variable by name Over18
# We create dummies for variable with more than two factor levels, convert two factor levels variables to 0s and 1s and
# purge from the dataset 1 factor level variables. The remaining variables are numeric they will  requires normalization by means of scaling.

#------------------------------------------------------
# Dummies - Categorical Variables transformation
#-------------------------------------------------------

# create dataframe of the categorical variables
empCategorical <- subset(empDataFinal, select = c(BusinessTravel, Department, EducationField , JobRole, MaritalStatus))


# Create dummies
empDummies<- data.frame(sapply(empCategorical, 
                            function(x) data.frame(model.matrix(~x-1,data =empCategorical))[,-1]))

# Attach dummies back to the main data frame
empDataFinal1 <- subset(empDataFinal, select = -c(EmployeeID, BusinessTravel, Department, EducationField , JobRole, MaritalStatus))
empDataFinal1 <- cbind(empDataFinal1, empDummies)

#Convert Gender to numeric
empDataFinal1$Gender <- ifelse(empDataFinal1$Gender == "Male",1,0) 

# purge over18, EmployeeCount, StandardHours, these variables are of uniform values across all records in the data set and as such
# not ideal for model building
empDataFinal1  <- subset(empDataFinal1, select = -c(Over18, EmployeeCount, StandardHours))

# convert target variable attrition from Y/NO TO 1/O
empDataFinal1$Attrition <- ifelse(empDataFinal$Attrition == "Yes",1,0)

str(empDataFinal1)
#--------------------------------------------------------
# Scaling - Numerical variables normalization
#---------------------------------------------------------

empDataFinal1$Age <- scale(empDataFinal1$Age)                 
empDataFinal1$DistanceFromHome <- scale(empDataFinal1$DistanceFromHome)      
empDataFinal1$MonthlyIncome <- scale(empDataFinal1$MonthlyIncome)         
empDataFinal1$TotalWorkingYears <- scale(empDataFinal1$TotalWorkingYears)  
empDataFinal1$DaysOff <- scale(empDataFinal1$DaysOff)
empDataFinal1$Education <- scale(empDataFinal1$Education)
empDataFinal1$JobLevel <- scale(empDataFinal1$JobLevel)
empDataFinal1$NumCompaniesWorked <- scale(empDataFinal1$NumCompaniesWorked)
empDataFinal1$TrainingTimesLastYear <- scale(empDataFinal1$TrainingTimesLastYear)
empDataFinal1$YearsAtCompany <- scale(empDataFinal1$YearsAtCompany)
empDataFinal1$YearsSinceLastPromotion <- scale(empDataFinal1$YearsSinceLastPromotion)
empDataFinal1$YearsWithCurrManager <- scale(empDataFinal1$YearsWithCurrManager)
empDataFinal1$MeanHours <- scale(empDataFinal1$MeanHours)
empDataFinal1$EnvironmentSatisfaction <- scale(empDataFinal1$EnvironmentSatisfaction)
empDataFinal1$JobSatisfaction <- scale(empDataFinal1$JobSatisfaction)
empDataFinal1$WorkLifeBalance <- scale(empDataFinal1$WorkLifeBalance)
empDataFinal1$JobInvolvement <- scale(empDataFinal1$JobInvolvement)
empDataFinal1$PerformanceRating <- scale(empDataFinal1$PerformanceRating)
empDataFinal1$StockOptionLevel <- scale(empDataFinal1$StockOptionLevel)
empDataFinal1$PercentSalaryHike <- scale(empDataFinal1$PercentSalaryHike)
empDataFinal1$StandardHoursDiff <- scale(empDataFinal1$StandardHoursDiff)

# Final dataset
View(empDataFinal1)

#-------------------------------------------------------------------------------------------
# MODEL BUILDING
#-------------------------------------------------------------------------------------------

# Split dataset into training and training and testing set
set.seed(100)
splitIndices <- sample(1:nrow(empDataFinal1),  0.7*nrow(empDataFinal1))
splitIndices

trainSet <- empDataFinal1[splitIndices,]
testSet <- empDataFinal1[-splitIndices,]

# First Model
empModel <- glm(Attrition ~ ., data = trainSet, family = "binomial")
summary(empModel) #AIC 2131.1 , Null deviance 2691.5, Residual deviance 2049.1

# StepWise selection
empModel1 <- stepAIC(empModel, direction = "both")
summary(empModel1)

#  Evaluate multicollinearlity with vif
vif(empModel1)
# MaritalStatus.xMarried has the highest combination of vif at 	2.126386 and p-value	0.067759, we remove it 
# on our next run

empModel2 <- glm(formula = Attrition ~ Age + Education + NumCompaniesWorked + StockOptionLevel + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + MeanHours + EnvironmentSatisfaction + 
                  JobSatisfaction + WorkLifeBalance + JobInvolvement + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + EducationField.xMarketing + EducationField.xOther + 
                  EducationField.xTechnical.Degree + JobRole.xLaboratory.Technician + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive +  MaritalStatus.xSingle, family = "binomial", data = trainSet)

summary(empModel2)

# JobRole.xManufacturing.Director , JobRole.xLaboratory.Technician ,EducationField.xOther ,EducationField.xMarketing, JobInvolvement 
# are insignificant (high p-value)

#vif on empModel2
vif(empModel2)   #JobRole.xManufacturing.Director  has highest p-value we remove it from the next run

empModel3 <- glm(formula = Attrition ~ Age + Education + NumCompaniesWorked + StockOptionLevel + 
                   TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                   YearsWithCurrManager + MeanHours + EnvironmentSatisfaction + 
                   JobSatisfaction + WorkLifeBalance + JobInvolvement + BusinessTravel.xTravel_Frequently + 
                   BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                   Department.xSales + EducationField.xMarketing + EducationField.xOther + 
                   EducationField.xTechnical.Degree + JobRole.xLaboratory.Technician + 
                    JobRole.xResearch.Director + JobRole.xResearch.Scientist + JobRole.xSales.Executive +  MaritalStatus.xSingle, family = "binomial", data = trainSet)

summary(empModel3) # EducationField.xMarketing, EducationField.xOther, JobInvolvement are insignificant (highest p - value)

#vif on empModel3
vif(empModel3) # EducationField.xOther has the p-value, we remove it from the next run

#empModel4
empModel4 <- glm(formula = Attrition ~ Age + Education + NumCompaniesWorked + StockOptionLevel + 
                   TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                   YearsWithCurrManager + MeanHours + EnvironmentSatisfaction + 
                   JobSatisfaction + WorkLifeBalance + JobInvolvement + BusinessTravel.xTravel_Frequently + 
                   BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                   Department.xSales + EducationField.xMarketing +  
                   EducationField.xTechnical.Degree + JobRole.xLaboratory.Technician + 
                   JobRole.xResearch.Director + JobRole.xResearch.Scientist + JobRole.xSales.Executive +  MaritalStatus.xSingle, family = "binomial", data = trainSet)

summary(empModel4) #EducationField.xMarketing is insignificant

#vif on empModel4
vif(empModel4) # EducationField.xMarketing though with a lower vif value has a higher p-value, we drop it

# empModel5
empModel5 <- glm(formula = Attrition ~ Age + Education + NumCompaniesWorked + StockOptionLevel + 
                   TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                   YearsWithCurrManager + MeanHours + EnvironmentSatisfaction + 
                   JobSatisfaction + WorkLifeBalance + JobInvolvement + BusinessTravel.xTravel_Frequently + 
                   BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                   Department.xSales  +  EducationField.xTechnical.Degree + JobRole.xLaboratory.Technician + 
                   JobRole.xResearch.Director + JobRole.xResearch.Scientist + JobRole.xSales.Executive +  MaritalStatus.xSingle, family = "binomial", data = trainSet)

summary(empModel5) # All remaining variables are significant with lower p-value. We end here and adopt 
#empModel as our final model

vif(empModel5)

################################################################################################
# MODEL EVALUATION
###############################################################################################

# Predict employee attrition on test dataset
testSet_Pred <- predict(empModel5, type = "response",
                        newdata = testSet[,-2])

# We check the summary
summary(testSet_Pred)

#Add predicted values to test dataset
testSet$Prediction <- testSet_Pred

View(testSet)

# Lets take a probability cutoff of 40% to calculate our model accuracy
pred_attrition <- factor(ifelse(testSet_Pred >= 0.25, "Yes","No"))
actual_attrition <- factor(ifelse(testSet$Attrition ==1, "Yes", "No"))

# confusion matrix
table(actual_attrition, pred_attrition)

# calculate model sensitivity (True positive rate)

#library(caret)
confMatrix <- confusionMatrix(pred_attrition, actual_attrition, positive = "Yes")
confMatrix



################################################################################################
# MODEL PRESENTATION
################################################################################################

