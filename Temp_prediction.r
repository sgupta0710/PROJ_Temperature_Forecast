#Temparature forecast
#Using this dataset, can we predict next day's (01-01-2020) maximum temperature. 
#For refernce to our prediction, 
#We have the dataset which contains the temperature data from last 1 year 

########Loading Libraries##########
# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("randomForest", repos = "http://cran.us.r-project.org")


library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(dplyr)
library(randomForest)


#Load Data
#This dataset is from  
#https://www.ncep.noaa.gov/

dates<- read.csv('temps_new.csv' , stringsAsFactors = TRUE)
tail(dates)
str(dates)

#Here we observe that we have 348 entries whereas in a year we have 365 days (in 2019).
#That means we have less data as expected, but since this is not a big number,missing data will not have large effect.
#Also, this data is from a very trusted source we can say that the data quality is good.

#the data has 9 columns with 8 features and one target - "actual"

#Exploratory Data Analysis
#Data Preperation
#Data pre-processing
#1. One-hot coding
#2. split data - into features and labels
#3. Convert to arrays
#4. Split data into training and tests sets


#Identify anomalies in each column of the dataset using Summary
summary(dates)

#Just by looking at the summary of the data, it becomes difficult to find out any anomalities. But by using the graphs, any anomalities looks clearly.

#lets convert the dates in date format
years <- dates$year
months <- dates$month
day <- dates$day

#lets convert them to date format now

date <- dates %>%
  mutate(date = make_date(year, month, day))
date

#lets start pre-processing the data
#1. one hot coding - using dummyvars of caret package
#one-hot coding - takes input as categorical data and converts them to the numerical data without any ordering

library(caret)
dmy <- dummyVars(" ~ .", data = dates)
newdates <- data.frame(predict(dmy, newdata = dates))
newdates

glimpse(newdates)
str(newdates)

#Visualization
plot(dates$actual, type = "l", ylab = "Temperature", xlab = " ", main = "Max Temp")
plot(dates$temp_1, type = "l", ylab = "Temperature", xlab = " ", main= "Previous Day Max Temp")
plot(dates$temp_2, type = "l", ylab = 'Temperature', xlab = 'Date',  main= "Two Days Prior Max Temp")
plot(dates$friend, type = "l", ylab = "Temperature", main= "Friend Estimate")




#2. Split dataframe into features and target
#labels are target
#features are columns used to make predictions

features <- newdates %>% select('year','month','day','week.Fri','week.Mon','week.Sat','week.Sun','week.Thurs', 'week.Tues' ,'week.Wed' ,'temp_2' ,'temp_1' , 'average' , 'friend')
target <- newdates %>% select('actual')  


#3. Split data into train and test set
set.seed(1, sample.kind = "Rounding")  # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y= newdates$actual, times = 1, p = 0.25, list = FALSE)

train_set <- newdates[-test_index,]
test_set <- newdates[test_index,]

#tail(test_set)

#analyse the train set and test set
summary(train_set)
summary(test_set)

#lets check the number of days in each month in the dataset
newdates %>% group_by(month) %>% summarise(n = n())


#check any NA value in the dataset
anyNA(train_set)
anyNA(test_set)

#fetch all the column names except target column name 'actual' in the list for using them in future
featuresCols <- colnames(newdates[-14])
featuresCols






##MODEL
#lets calculate the base model using the result of which we can see how can we improve our further models
#1. Simple Average Model
base_avg_values_test <- test_set[ , "average"]
base_avg_values_test
base_actual_values_test <- test_set[ , "actual"]
base_actual_values_test


base_err <- abs(base_avg_values_test  - base_actual_values_test)
base_err
#calculate the average base error in degrees
mean_avg <- mean(base_err)
mean_avg   #outcome: 4.51

#calculate MAPE- Mean absolute percentage error
mape_avg = 100 * (mean_avg / base_actual_values_test)



#accuracy
accuracy_avg = 100 - mean(mape_avg)
accuracy_avg


predicted_results <- data_frame(date = "31-12-2019" , method = "Average Model appraoch", Prediction = NA, Error = mean_avg , Accuracy = accuracy_avg)
print.data.frame(predicted_results)

#with this avg we get the forecast with an error of 4.51 degrees, now let us build  model which will give us less error than this base error

#Now lets train our model using RANDOM FOREST
#all the package installations and library loading is done above
set.seed(22, sample.kind = 'Rounding')
temp.rf <- randomForest(actual ~ . , data = train_set, 
                        importance = TRUE, na.action = na.omit)

temp.rf


#lets check the predictions

pred = predict(temp.rf, newdata = test_set)
pred

#now lets calculate the absolute error between the predicted values and the actual value
abs_erre_rf <- abs(pred - base_actual_values_test)

#take the mean of absolute error 
abs_mean_rf <- mean(abs_erre_rf)
abs_mean_rf   #outcome 4.19

#Here we observed that the new model has improved the error from 4.51 to 4.18

#calculate MAPE- Mean absolute percentage error
mape = 100 * (abs_erre_rf / base_actual_values_test)
mape


#accuracy
accuracy = 100 - mean(mape)
accuracy  

predicted_results <- bind_rows(predicted_results,data_frame(date = "31-12-2019" , method = "Random_forest 1", Prediction = pre1, Error = abs_mean_rf , Accuracy = accuracy))
print.data.frame(predicted_results)

#Improve the model
set.seed(1, sample.kind = "Rounding")
temp.rf_new <- randomForest(actual ~ . , data = train_set, mtry=4 ,ntree = 120,
                        importance = TRUE, na.action = na.omit)

temp.rf_new

#lets check the predictions
pred_new = predict(temp.rf_new, newdata = test_set)
pred_new


#now lets calculate the absolute error between the predicted values and the actual value
abs_erre_rf_new <- abs(pred_new - base_actual_values_test)

#take the mean of absolute error 
abs_mean_rf_new <- mean(abs_erre_rf_new)
abs_mean_rf_new   #outcome 4.14

#calculate MAPE- Mean absolute percentage error
mape_new = 100 * (abs_erre_rf_new / base_actual_values_test)
mape_new


#accuracy
accuracy_new = 100 - mean(mape_new)
accuracy_new 

predicted_results <- bind_rows(predicted_results, data_frame(date = "31-12-2019" , method = "Random_forest_improved", Prediction = pre2 , Error = abs_mean_rf_new , Accuracy = accuracy_new ))
print.data.frame(predicted_results)

#Here we observed that the new model has improved the error from 4.19 to 4.14
#similarly we can tune other hyperparameters as well

#Variable importance and feature selection

#By now we have made a good model which is giving us improved error values. Now on observing we can see that there are many features which are not contributing much 
#in the prediction of the temperature. Here we need to identify which are the top fetures which are contributing in the better prediction

importance(temp.rf)


#Here we received two paramenters - %IncMSE & IncNodePurity
#Mean Decrease Accuracy (%IncMSE) - This shows how much our model accuracy decreases if we leave out that variable.The higher number, the more important the feature is.
#IncNodePurity - This is a measure of variable importance based on the Gini impurity index used for the calculating the splits in trees.

#so the two important features are 
#1.temp_1- the maximum temperature the day before 
#2.average - the historical avg max temperature

#the day of the week and the year are not contributing much in predicting the max temperature as it has nothing to do with the temperature

#to improve our model we will only consider these two paramenters and calculate the error

#model with two most important features - here we can remove all other variables which has no/less importance

imp_var <- c('temp_1','average','actual')
imp_var

train_imp <- train_set[ , imp_var]
test_imp <- test_set[ , imp_var]
set.seed(22,sample.kind = "Rounding")
temp.rf_imp <- randomForest(actual ~ . , data = train_imp, mtry=2 ,ntree = 100,
                                          importance = TRUE, na.action = na.omit)
temp.rf_imp

#lets check the predictions
pred_imp = predict(temp.rf_imp, newdata = test_imp)
pred_imp


#now lets calculate the absolute error between the predicted values and the actual value
abs_erre_rf_imp <- abs(pred_imp - base_actual_values_test)

#take the mean of absolute error 
abs_mean_rf_imp <- mean(abs_erre_rf_imp)
abs_mean_rf_imp   #outcome 4.19

#calculate MAPE- Mean absolute percentage error
mape_imp = 100 * (abs_erre_rf_imp / base_actual_values_test)
mape_imp

#accuracy
accuracy_imp = 100 - mean(mape_imp)
accuracy_imp  #93.23%

predicted_results <- bind_rows(predicted_results, data_frame(date = "31-12-2019" , method = "Random_forest_important", Prediction = pre3 , Error = abs_mean_rf_imp , Accuracy = accuracy_imp ))
print.data.frame(predicted_results)

#On taking just the imp variables/features we have seen that the performance is almost similar infact bit improved as it is when all the variables were considered.
#In real life project, if we work on all the variables, model will take more time and memory and will give same result, hence it is 
#advisable that before applying the model, we should identify the most important variables/features which will majorly contribute in the prediction.

#Visualization -final after model
#lets visualize the importance of the features 

varImpPlot(temp.rf, sort = TRUE)


#in histogram
imp1 = as.data.frame(importance(temp.rf))
imp1 = cbind(vars=rownames(imp1), imp1)
testing <- imp1[2]
imp1 = imp1[order(testing),]
imp1$vars = factor(imp1$vars, levels=unique(imp1$vars))
#ggplot(imp1, aes(x=IncMSE, y=vars)) + geom_bar(stat="identity") 

imp1 %>% ggplot(aes(imp1$vars ,imp1$`%IncMSE` )) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ xlab("Variable Name") + ylab("Importance %age")

#lets plot predictions vs actual and find out any outliers if present
actual_data <- date %>% select(date,actual)

#fetch data from test_set
years_t <- test_set$year
months_t <- test_set$month
day_t <- test_set$day

test_set_date <- test_set %>%
  mutate(date = make_date(year, month, day))
test_set_date
#make df
predicted_data <- test_set_date %>% select(date) %>% mutate(predictions = pred_imp)
predicted_data


plot(actual_data,date$date, type="o", col="blue", pch="l", lty=1, ylim=c(0,110), ylab="Max Temperature in F")
points(predicted_data,y=NULL,col="red", pch=16 ) 
legend(x="topleft",
       ncol = 4,
       legend = c("actual",
                  "predicted"
                  ),
       fill = c("blue","red")
  )

#no outliers are present
#Results


