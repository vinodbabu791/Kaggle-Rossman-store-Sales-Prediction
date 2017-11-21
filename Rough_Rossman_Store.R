## Rossman Stores ###
library(car)
train = read.csv(file.choose(),header = TRUE,stringsAsFactors = FALSE) 
test = read.csv(file.choose(),header=TRUE,stringsAsFactors = FALSE)

head(train)

train_7 <- train[train$Store== 7,]
if (anyNA(train_7)){
  print ('Treat Missing Values.....')
}else {
  print ('No Missing Values....')
}

cor(train_7[-c(1,3,8)])
pairs(train_7[-c(1,3,8)])


for (i in names(train_7[-c(1, 3, 8)])) {
  print(paste(
    'Correlation between Sales and ',
    i,
    ' is ',
    round(cor(train_7['Sales'], train_7[i]),5)
  ))
}

### Data Preprocessing

## Convert to time series
train_7$Date <- as.Date(train_7$Date,'%Y-%m-%d')
str(train_7)
library(dplyr)
train_7 <- arrange(train_7,Date)
train_ts <- ts(train_7['Sales'],start = c(1,3),frequency=7)

train_final_df <- data.frame(Customers = train_7$Customers,
                         DayOfWeek=train_7$DayOfWeek,
                         Open=train_7$Open,
                         Promo=train_7$Promo,
                         StateHoliday=train_7$StateHoliday,
                         SchoolHoliday=train_7$SchoolHoliday
                         )
levels(train_final_df$StateHoliday) <-  c('0','1','2','3')
train_final_df$StateHoliday <- as.integer(as.character(train_final_df$StateHoliday))
str(train_final_df)
head(train_final_df)
head(train_7)
ts.plot(train_ts[1:140])

library(fpp)
seasonplot(train_ts)
monthplot(train_ts)
cor(train_final_df)

#split data to training and testing set

m_train_ts <- window(train_ts,start=1,end=c(125,7))
m_test_ts <- window(train_ts,start = c(126,1))
train_iv <- train_final_df[1:873,]
test_iv <- train_final_df[874:nrow(train_final_df),]


model1 <- tslm(m_train_ts~trend+Customers+Promo,data=train_iv)
model2 <- tslm(m_train_ts~trend+Customers+Promo,data=train_iv)
model3 <- tslm(I(sqrt(m_train_ts))~trend+I(sqrt(Customers))+Promo,data=train_iv)
plot(log(as.numeric(m_train_ts+1)),log(train_iv$Customers),xlim=c(8,10))
cor(log(as.numeric(m_train_ts+1)),log(train_iv$Customers+1))
cor()
#model2 <- tslm(I(m_train_ts-(-151.79141))~0+Customers+Promo,data=train_iv)
#model3 <- tslm(m_train_ts~I(train_iv$Customers- mean(train_iv$Customers))+train_iv$Promo)
summary(model1)
vif(model1)
plot(model1)
plot(m_train_ts)
lines(time(m_train_ts),predict(model1),type = 'l',col = 'red')
predict(model2)

plot(log(m_train_ts))
