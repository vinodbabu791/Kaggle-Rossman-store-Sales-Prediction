##################################################################################
#                   Prediction of Rossman Store Sales (Strore No.7)

# Problem description:
# https://www.kaggle.com/c/rossmann-store-sales

# About the Dataset:
# You are provided with historical sales data for 1,115 Rossmann stores. The task is to forecast the "Sales" column for the test set. 
# Note that some stores in the dataset were temporarily closed for refurbishment.

# Data Attributes:
# https://www.kaggle.com/c/rossmann-store-sales/data

# Goal:
# To accurately forecast the sales of Store 7 

##################################################################################

##################################################################################
#                       REGRESSION MODEL TO BE USED

## Response variable in this problem is a continuous variable which is dependent on
## time as well as other explanatory attributes. Hence, Time Series Linear regression
## model is used as a regression model here. Frequency of Time Series is weekly and
## CYCLE STARTS FROM MONDAY(1) THROUGH SUNDAY(7)
##################################################################################

##################################################################################
#                       APPROACH

## step 1: importing required libraries
## step 2: loading raw data
## step 3: Data Preprocessing
#### step 3.1: Filter Store No.7 data
#### step 3.2: Correct the data type of columns
#### step 3.3: Sort the data by date
#### step 3.4: Check for NA Values
#### step 3.5: Convert sales data to time series 
#### step 3.6: Stationarity in Time series
#### step 3.7: Check for Outliers in sales
#### step 3.8: Create Data Frame with Sales Time Series and predictor variables
## step 4: Data Visualization
#### step 4.1: TimeSeries Plot
#### step 4.2: TimeSeries Plot(splitted)
#### step 4.3: Decomposition of Timeseries
#### step 4.4: Trend of Sales for each weekday using Month Plot
## step 5: Correlation between Predictors and response variable
#### step 5.1: Correlation Matrix
#### step 5.2: Scatterplot between response variable and potential predictors
## step 6: Splitting entire data to training and testing set
## step 7: Model Fitting
#### step 7.1: Model 1 - with transformed predictors
#### step 7.2: Model 2 - with untransformed predictors
## step 8: Check for multicollinearity in the models
## step 9: Goodness of Fit
#### step 9.1: Create model parameters dataframe with AIC, BIC, R-squared, Adj.R-squared and residual standard error for the models
#### step 9.2: Residual Diagnostics
## step 10: Prediction of Sales
#### step 10.1: Predicting sales for test data
#### step 10.2: Plot of predicted values
#### step 10.3: Accuracy of predicted Values
## step 11: Overall Interpretation

##################################################################################


## step 1: importing required libraries
library(dplyr)
library(fpp)
library(car)
library(MLmetrics)

## step 2: loading raw data
raw_data <- read.csv(file.choose(),header=TRUE)


## step 3: Data Preprocessing

#### step 3.1: Filter Store No.7 data
raw_data_7 <- filter(raw_data,Store==7)
str(raw_data_7)


#### step 3.2: Correct the data type of columns
raw_data_7$DayOfWeek <- as.factor(raw_data_7$DayOfWeek)
raw_data_7$Date <- as.Date(raw_data_7$Date,'%Y-%m-%d')
raw_data_7$Open <- as.factor(raw_data_7$Open)
raw_data_7$Promo <- as.factor(raw_data_7$Promo)
###### a=1=>Public Holiday; b=2=>Easter Holiday; c=3=>Christmas; 0=>none
levels(raw_data_7$StateHoliday) <- c('0','1','2','3')
raw_data_7$SchoolHoliday <- as.factor(raw_data_7$SchoolHoliday)
str(raw_data_7)


#### step 3.3: Sort the data by date
raw_data_7 <- arrange(raw_data_7,Date)
head(raw_data_7)


#### step 3.4: Check for NA Values
if(anyNA(raw_data_7)){
  print('Treat Missing Values...')
}else{
  print('No Missing values...')
}


#### step 3.5: Convert data to time series
sales_ts <- ts(raw_data_7$Sales,start=c(1,2),frequency=7)
head(sales_ts)


#### step 3.6: Stationarity in Time series
par(las=1)
ts.plot(sales_ts,xlab='Week Number',ylab='Sales',
        main='Stationarity of Time Series')
points(cummean(sales_ts)~time(sales_ts),col='red',lwd=0.5)
legend(80,19000,bty='n',col='red',lwd=2,legend='Mean Line')

#### step 3.7: Check for Outliers in sales
boxplot(sales_ts~cycle(sales_ts),
        col=2:8,
        xlab='Week Day',
        ylab='sales',
        main='Trend of Sales for each day of the week',
        las=1)
legend(6.3,19000,legend=weekdays(as.Date('2017-11-19')+1:7),col=2:8,lwd=3,cex=0.8,bty='n')


#### step 3.8: Create Data Frame with Sales Time Series and predictor variables
ross=data.frame(sales_ts=sales_ts,
                DayOfWeek=raw_data_7$DayOfWeek,
                Customers=raw_data_7$Customers,
                Open=raw_data_7$Open,
                Promo=raw_data_7$Promo,
                StateHoliday=raw_data_7$StateHoliday,
                SchoolHoliday=raw_data_7$SchoolHoliday,
                Date=raw_data_7$Date)
str(ross)

## step 4: Data Visualization

#### step 4.1: TimeSeries Plot
par(las=1)
ts.plot(ross$sales_ts,xlab='Week Number',ylab='Sales',
        main='Time Series of Rossman store 7 sales')


#### step 4.2: TimeSeries Plot(splitted)
par(mfrow=c(2,2))
for (i in 1:ceiling(end(ross$sales_ts)[1]/35)){
  ts.plot(window(ross$sales_ts,start=c(((i-1)*35)+1,1),end=c(i*35,7)),xlab='Week Number',ylab='Sales',
          main=paste('Time Series of Rossman store 7 sales: ',((i-1)*35)+1, ' to ', i*35,' weeks'))
}


#### step 4.3: Decomposition of Timeseries
par(mfrow = c(1,1))
plot(decompose(sales_ts))


#### step 4.4: Trend of Sales for each weekday using Month Plot
monthplot(ross$sales_ts,
          las=1,
          main = 'Trend for each day of the week',
          ylab='Sales',
          xlab='Week Day',
          col.base = 2:8,
          lwd.base=3)
legend(6.3,19000,legend=weekdays(as.Date('2017-11-19')+1:7),col=2:8,lwd=3,cex=0.6,bty='n')


## step 5: Correlation between Predictors and response variable

#### step 5.1: Correlation Matrix
attach(ross)
ross_numeric = data.frame(sales_ts = as.numeric(sales_ts),
                          DayOfWeek = as.numeric(as.character(DayOfWeek)),
                          Customers = as.numeric(Customers),
                          Open = as.numeric(as.character(Open)),
                          Promo = as.numeric(as.character(Promo)),
                          StateHoliday = as.numeric(as.character(StateHoliday)),
                          SchoolHoliday = as.numeric(as.character(SchoolHoliday))
)
detach(ross)
str(ross_numeric)
cor(ross_numeric)


#### step 5.2: Scatterplot between response variable and potential predictors
attach(ross_numeric)
par(mfrow = c(3,2))
plot(sales_ts~.,data=ross_numeric,ylab = 'Sales',pch='+',col='darkgreen')
mtext('Correlation of Sales with potential Predictors',side = 3,line=-2,outer=TRUE)
detach(ross_numeric)


## step 6: Splitting entire data to training and testing set
test_data <- data.frame()
if(nrow(test_data)==0) {
  train_tsales <- window(ross$sales_ts,end=c(125,7)) 
  test_tsales <- window(ross$sales_ts,start=c(126,1))
  train_data <- ross[1:length(train_tsales),][-1]
  test_data <- ross[(length(train_tsales)+1):nrow(ross),][-1]
}


## step 7: Model Fitting

#### step 7.1: Model 1 - with transformed predictors
ross_model <- tslm(I(sqrt(train_tsales))~trend+I(sqrt(Customers))+Promo,data=train_data)
summary(ross_model)


#### step 7.2: Model 2 - with untransformed predictors
ross_model_comp <- tslm(train_tsales~trend+Customers+Promo,data=train_data)
summary(ross_model_comp)


## step 8: Check for multicollinearity in the models
vif(ross_model)
vif(ross_model_comp)


## step 9: Goodness of Fit

## step 9.1: Create model parameters dataframe with AIC, BIC, R-squared, Adj.R-squared and residual standard error for the models
model_list <- list(ross_model,ross_model_comp)
Formula=c()
AIC=c()
BIC=c()
r2=c()
Adj_r2=c()
res_std_error=c()
for (i in model_list){
  Formula <- c(Formula,i$call)
  AIC <- c(AIC,AIC(i))
  BIC <- c(BIC,BIC(i))
  r2 <- c(r2,summary(i)$r.squared)
  Adj_r2 <- c(Adj_r2,summary(i)$adj.r.squared)
  res_std_error<-c(res_std_error,summary(i)$sigma)
}
model_parameters <- data.frame(Formula=as.character(Formula),
                               AIC=AIC,
                               BIC=BIC,
                               r2=r2,
                               Adj_r2=Adj_r2,
                               res_std_error=res_std_error)
row.names(model_parameters) <- c('Model 1','Model 2')
print(model_parameters)


#### step 9.2: Residual Diagnostics
cor(ross_model$residuals,ross_model$fitted.values)
par(mfrow=c(1,1))
plot(ross_model$residuals~time(train_tsales),
     xlab='Week Number',
     ylab='Residuals',
     main='Residuals Against Time',
     col='blue')
abline(h=0,lwd=2,col='red')
plot(ross_model$residuals~ross_model$fitted.values,
     xlab= 'Fitted Values',
     ylab='Residuals',
     main='Residuals Against Fitted Values',
     col='blue',
     xlim=c(-1,125))
abline(h=0,lwd=2,col='red')


## step 10: Prediction of Sales

#### step 10.1: Predicting sales for test data
prediction <- forecast(ross_model,newdata=test_data)
predict_signs <- sign(data.frame(prediction))
predicted_vals <- data.frame(round(data.frame(prediction)^2,4)*predict_signs)
predicted_data <- data.frame(Forecasted_Sales = predicted_vals[,1] ,
                             Actual_Sales = test_tsales,test_data)
head(predicted_data)

#### step 10.2: Plot of predicted values
par(mfrow=c(1,1))
plot(forecast(ross_model,newdata=test_data),
     xlab='Week Number',
     ylab='Sales',
     main='Forecast of Sales of store 7')
plot(predicted_data$Forecasted_Sales~time(test_tsales),
     type='l',
     xy.labels = FALSE,
     xlab='Predicted Week Numbers',
     ylab='Forecasted Sales',
     main='Prediction Intervals for forcasted values',
     col='blue',
     ylim=c(-100,16000),
     lwd=2
)
points(predicted_vals$Lo.95~time(test_tsales),type='l',lwd=2,lty=1,col='lightblue')
points(predicted_vals$Hi.95~time(test_tsales),type='l',lwd=2,lty=1,col='lightblue')
legend(129,17000,bty='n',
       legend = c('Point Forecast','95% Prediction Interval'),
       lty=1,lwd=2,col=c('blue','lightblue'),cex=0.8)


#### step 10.3: Accuracy of predicted Values
pred_with_sales <- filter(predicted_data,Open != 0)
accuracy(pred_with_sales$Forecasted_Sales,as.numeric(pred_with_sales$Actual_Sales))
RMSPE(pred_with_sales$Forecasted_Sales,as.numeric(pred_with_sales$Actual_Sales))

## step 11: Overall Interpretation
#### Model 1(ross_model) with transformed variable has the minimum error values, maximum Adjusted
#### R-squared values. Hence this model works best for this dataset
