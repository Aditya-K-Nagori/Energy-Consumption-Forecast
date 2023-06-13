setwd("F://Time Series 4th Sem//Project")
df= read.csv("Energy.csv")
colnames(df) <- c("Date", "Hour", "Consumption (MWH)")
df$`Consumption (MWH)` <- gsub(",", "", df$`Consumption (MWH)`)
DateTime <- as.POSIXct(paste(df$Date, df$Hour), format="%d.%m.%Y %H:%M")
# add the datetime column to your original data frame
df <- cbind(df, DateTime)
df <- subset(df,select = -c(Date, Hour))
# plot the time series using the datetime column
plot(df$DateTime, df$`Consumption (MWH)`, type = 'l',
     xlab = "Date Time", ylab = ("Consumption"),main ="Time Series")

# Load necessary packages
library(ggplot2)
library(lubridate)
library(dplyr)
df$`Consumption (MWH)` <- as.numeric(df$`Consumption (MWH)`)

# Convert datetime column to a datetime object
df$DateTime <- ymd_hms(df$DateTime)

# Create daily and monthly aggregates of consumption
daily_df <- df %>%
  group_by(Date = as.Date(floor_date(DateTime, "day"))) %>%
  summarize(Total_Consumption = sum(`Consumption (MWH)`))

monthly_df <- df %>%
  group_by(Date = floor_date(DateTime, "month")) %>%
  summarize(Total_Consumption = sum(`Consumption (MWH)`))

# Create plot of daily consumption
ggplot(daily_df, aes(x = Date, y = Total_Consumption)) +
  geom_line() +
  labs(x = "Date", y = "Total Consumption (MWH)",
       title = "Daily Electricity Consumption",
       subtitle = "Dec 2015 - June 2020")

# Create plot of monthly consumption
ggplot(monthly_df, aes(x = Date, y = Total_Consumption)) +
  geom_line() +
  labs(x = "Month", y = "Total Consumption (MWH)",
       title = "Monthly Electricity Consumption",
       subtitle = "Dec 2015 - June 2020")


df_daily <- df %>% 
  group_by(date(DateTime)) %>% 
  summarise(`Consumption (MWH)` = sum(`Consumption (MWH)`)) 

ggplot(df_daily, aes(x = "Daily", y = `Consumption (MWH)`)) +
  geom_boxplot() +
  labs(title = "Daily Consumption Distribution", x = "", y = "Consumption (MWH)")

# Create a histogram of hourly consumption
ggplot(df, aes(x = hour(DateTime), y = `Consumption (MWH)`)) +
  geom_histogram(stat = "identity", bins = 24) +
  labs(title = "Hourly Consumption Distribution", x = "Hour of Day", y = "Count")

acf(df$`Consumption (MWH)`)
pacf(df$`Consumption (MWH)`)


# Convert DateTime column to a datetime object
df$DateTime <- as.POSIXct(df$DateTime)


#Daily 

#STL with Multiple Seasonal Periods
library(lubridate)
library(dplyr)
library(forecast)
library(ggplot2)
library(scales)
summary(df)
df.ts = ts(df$`Consumption (MWH)`, frequency = 24)
autoplot(df.ts)
acf(df.ts)
pacf(df.ts)
plot(decompose(head(df.ts,2000)))

train_data<-df$`Consumption (MWH)` %>% ts(freq= 24)
train_data %>% 
  tail(24*7*4) %>% 
  decompose() %>% 
  autoplot()
msts_cons<-df$`Consumption (MWH)` %>% msts( seasonal.periods = c(24, 24*7))
msts_cons  %>% head(  24 *7 *4 ) %>% mstl() %>% autoplot()
msts_train <- head(msts_cons, length(msts_cons) - 24*7)
msts_test <- tail(msts_cons,  24*7)

#Multiple Seasonality Models.
#subset to more recent period
msts_train <- tail(msts_train, 24*7*4*3)
autoplot(msts_train)
stlm_model <- msts_train %>%
  stlm(lambda = 0) %>% 
  forecast(h = 24*7) 
plot(stlm_model)

summary(stlm_model)

#TBATS Models
tbats_mod <- msts_train %>%
  log() %>% 
  tbats(use.box.cox = FALSE, 
        use.trend = TRUE, 
        use.damped.trend = TRUE)
tbats_model <-  forecast(tbats_mod,h=24*7) 
plot(tbats_model)   
summary(tbats_model)
result<-rbind(accuracy((stlm_model$mean) , msts_test), 
              accuracy(as.vector(exp(tbats_model$mean)) , msts_test)) 
rownames(result) <- c("stlm_model","tbats_model")
result

accuracyData <- data.frame(datetime= df$DateTime %>% tail(24*7),
                           actual = as.vector(msts_test) ,
                           stlmForecast = as.vector(stlm_model$mean) ,
                           tbatsForecast = as.vector(exp(tbats_model$mean)))

accuracyData %>% 
  ggplot() +
  geom_line(aes(x = (df$DateTime %>% tail(24*7)), 
                y = (df$`Consumption (MWH)` %>% tail(24*7)), colour = "actual"))+
  geom_line(aes(x = (df$DateTime %>% tail(24*7)), 
                y = stlm_model$mean, colour = "stlm"))+
  geom_line(aes(x = (df$DateTime %>% tail(24*7)),
                y = exp(tbats_model$mean),   colour = "tbats "))+ 
  scale_y_continuous(labels = comma)+
  labs(
    title = "Hourly Energy Consumption in Megawatts",
    x = "Date",
    y = "",
    colour = ""
  )

#regression model
fit <- lm(`Consumption (MWH)` ~ DateTime, data = (head(df,39288)))
pred <- predict(fit, newdata = tail(df,24*7))
attach(mtcars)
par(mfrow=c(2,2))
plot(fit)

ggplot() +
  geom_line(aes(x = (df$DateTime %>% tail(24*7)), 
                y = (df$`Consumption (MWH)` %>% tail(24*7)), colour = "actual"))+
  geom_line(aes(x = (df$DateTime %>% tail(24*7)), y =(pred),   
                colour = "regression "))+ 
  scale_y_continuous(labels = comma)+
  labs(
    title = "Hourly Energy Consumption in Megawatts",
    x = "Date",
    y = "",
    colour = ""
  )

#smoothing

ets= ets(head(df$`Consumption (MWH)`,39288))
autoplot(ets)
pred_ets= forecast(ets,24*7)

ggplot() +
  geom_line(aes(x = (df$DateTime %>% tail(24*7)), 
                y = (df$`Consumption (MWH)` %>% tail(24*7)), colour = "actual"))+
  geom_line(aes(x = (df$DateTime %>% tail(24*7)), y =(pred_ets$mean),   
                colour = "ETS"))+ 
  scale_y_continuous(labels = comma)+
  labs(
    title = "Hourly Energy Consumption in Megawatts",
    x = "Date",
    y = "",
    colour = ""
  )


#simple_naive
forecast_naive <- c(NA, df$`Consumption (MWH)`[-length(df$`Consumption (MWH)`)])
ggplot() +
  geom_line(aes(x = (df$DateTime %>% tail(24*7)), 
                y = (df$`Consumption (MWH)` %>% tail(24*7)), colour = "actual"))+
  geom_line(aes(x = (df$DateTime %>% tail(24*7)), y =(tail(forecast_naive,24*7)),   
                colour = "Naive "))+ 
  scale_y_continuous(labels = comma)+
  labs(
    title = "Hourly Energy Consumption in Megawatts",
    x = "Date",
    y = "",
    colour = ""
  )

#Holtwinters
test_hw = ts(tail(df$`Consumption (MWH)`,24*7), frequency = 24)
train_hw = ts(head(df$`Consumption (MWH)`,39288), frequency = 24)
model_hw = HoltWinters(train_hw)
pred_hw = forecast(model_hw, 24*7)
ggplot() +
  geom_line(aes(x = (df$DateTime %>% tail(24*7)), 
                y = (df$`Consumption (MWH)` %>% tail(24*7)), colour = "actual"))+
  geom_line(aes(x = (df$DateTime %>% tail(24*7)), y =(pred_hw$mean),   
                colour = "Holt Winters "))+ 
  scale_y_continuous(labels = comma)+
  labs(
    title = "Hourly Energy Consumption in Megawatts",
    x = "Date",
    y = "",
    colour = ""
  )
p = as.data.frame(pred_hw$mean)
o=as.data.frame(tail(df$`Consumption (MWH)`,24*7))
mae(p$x,o$`tail(df$\`Consumption (MWH)\`, 24 * 7)`)
rmse(p$x,o$`tail(df$\`Consumption (MWH)\`, 24 * 7)`)
summary(pred_hw)


# arima
fit_arima <- (head(df$`Consumption (MWH)`,39288))%>%auto.arima()
prid_arima = predict(fit_arima, 24*7)
summary(fit_arima)

ggplot() +
  geom_line(aes(x = (df$DateTime %>% tail(24*7)), 
                y = (df$`Consumption (MWH)` %>% tail(24*7)), colour = "actual"))+
  geom_line(aes(x = (df$DateTime %>% tail(24*7)), y =(prid_arima$pred),   
                colour = "arima"))+ 
  scale_y_continuous(labels = comma)+
  labs(
    title = "Hourly Energy Consumption in Megawatts",
    x = "Date",
    y = "",
    colour = ""
  )

#neuralnet
library(neuralnet)
model <- nnetar(train_hw, repeats = 20, p=11, P = 1, size =7)
summary(model$model[[1]])#weights first train
summary(model$model[[2]])#weights second train
pred_nn = forecast(model,24*7)
ggplot() +
  geom_line(aes(x = (df$DateTime %>% tail(24*7)), 
                y = (df$`Consumption (MWH)` %>% tail(24*7)), colour = "actual"))+
  geom_line(aes(x = (df$DateTime %>% tail(24*7)), y =(pred_nn$mean),   
                colour = "Neural"))+ 
  scale_y_continuous(labels = comma)+
  labs(
    title = "Hourly Energy Consumption in Megawatts",
    x = "Date",
    y = "",
    colour = ""
  )
summary(pred_nn)

#Daily
# Aggregate data by 24 hours and group by whole day sum
agg_data <- aggregate(df$`Consumption (MWH)`, list(Date = as.Date(df$DateTime)), sum)

# Plot aggregated data
plot(agg_data$Date, agg_data$x, type = "l", main = "Aggregated Consumption", 
     xlab = "Date", ylab = "Consumption (MWH)")
colnames(agg_data) <- c("Date","Consumption (MWH)")

acf(agg_data$`Consumption (MWH)`)
pacf(agg_data$`Consumption (MWH)`)

# Load required libraries
library(forecast)
library(ggplot2)

# Convert Date column to date format
agg_data$Date <- as.Date(agg_data$Date)

# Split data into training and testing sets
train <- agg_data[1:round(nrow(agg_data)*0.8), ]
test <- agg_data[(round(nrow(agg_data)*0.8) + 1):nrow(agg_data), ]

# Fit regression model to training data
fit <- lm(`Consumption (MWH)` ~ Date, data = train)

# Make predictions on testing data
pred <- predict(fit, newdata = test)

# Plot predicted vs actual values
plot_data <- data.frame(Date = test$Date, Actual = test$`Consumption (MWH)`, Predicted = pred)
ggplot(plot_data, aes(x = Date)) +
  geom_line(aes(y = Actual, colour = "Actual")) +
  geom_line(aes(y = Predicted, colour = "Predicted")) +
  scale_colour_manual(name = "Values", values = c("Actual" = "blue", "Predicted" = "red")) +
  labs(title = "Regression Forecasting Results")

# Compute accuracy metrics
accuracy(pred, test$Consumption)

#Exponentil Smoothing
# Convert Date column to date format
agg_data$Date <- as.Date(agg_data$Date)
# Split data into training and testing sets
train <- agg_data[1:round(nrow(agg_data)*0.8), ]
test <- agg_data[(round(nrow(agg_data)*0.8) + 1):nrow(agg_data), ]

# Fit exponential smoothing model to training data
fit <- ets(train$`Consumption (MWH)`)

# Make predictions on testing data
pred <- forecast(fit, h = nrow(test))$mean
# Plot predicted vs actual values
plot_data <- data.frame(Date = test$Date, Actual = test$`Consumption (MWH)`, Predicted = pred)
ggplot(plot_data, aes(x = Date)) +
  geom_line(aes(y = Actual, colour = "Actual")) +
  geom_line(aes(y = Predicted, colour = "Predicted")) +
  scale_colour_manual(name = "Values", values = c("Actual" = "blue", "Predicted" = "red")) +
  labs(title = "Exponential Smoothing Forecasting Results")
# Compute accuracy metrics
accuracy(pred, test$`Consumption (MWH)`)

#smoothing
# Convert Date column to date format
agg_data$Date <- as.Date(agg_data$Date)

# Split data into training and testing sets
train <- agg_data[1:round(nrow(agg_data)*0.8), ]
test <- agg_data[(round(nrow(agg_data)*0.8) + 1):nrow(agg_data), ]

# Fit smoothing model to training data
fit <- smooth(train$`Consumption (MWH)`)

# Make predictions on testing data
pred <- forecast(fit, h = nrow(test))$mean

# Plot predicted vs actual values
plot_data <- data.frame(Date = test$Date, Actual = test$`Consumption (MWH)`, Predicted = pred)
ggplot(plot_data, aes(x = Date)) +
  geom_line(aes(y = Actual, colour = "Actual")) +
  geom_line(aes(y = Predicted, colour = "Predicted")) +
  scale_colour_manual(name = "Values", values = c("Actual" = "blue", "Predicted" = "red")) +
  labs(title = "Smoothing Forecasting Results")

# Compute accuracy metrics
accuracy(pred, test$`Consumption (MWH)`)

library(ggplot2)

# Convert the 'Date' column to a date object
agg_data$Date <- as.Date(agg_data$Date)
# Create a time series object
library(caret)
library(data.table)
library(zoo)
train <- head(agg_data,1637)
test <- tail(agg_data,7)
# Create LOCF predictions
lo_cf <- tail(train$`Consumption (MWH)`, 1)
lo_cf_preds <- rep(lo_cf, nrow(test))
ggplot() +
  geom_line(aes(x = (agg_data$Date %>% tail(7)), 
                y = (agg_data$`Consumption (MWH)` %>% tail(7)), colour = "actual"))+
  geom_line(aes(x = (agg_data$Date %>% tail(7)),
                y = lo_cf_preds,   colour = "tbats "))+ 
  scale_y_continuous(labels = comma)+
  labs(
    title = "Daily Energy Consumption in Megawatts",
    x = "Date",
    y = "",
    colour = ""
  )

# Create MA predictions (using a 7-day window)
ma_preds <- rollapply(train$`Consumption (MWH)`,
                      7, mean, align = "right", partial = TRUE)
ma_preds <- rep(tail(ma_preds, 1), nrow(test))
MAPE <- function(actual, predicted) {
  mean(abs((actual - predicted) / actual)) * 100
}
# Compute RMSE and MAPE for each model
library(metrics)
models <- c("LOCF", "MA")
rmse <- c(RMSE(test$`Consumption (MWH)`, lo_cf_preds),
          RMSE(test$`Consumption (MWH)`, ma_preds))
mape <- c(MAPE(test$`Consumption (MWH)`, lo_cf_preds),
          MAPE(test$`Consumption (MWH)`, ma_preds))
results <- data.frame(Model = models, RMSE = rmse, MAPE = mape)
ggplot() +
  geom_line(aes(x = (agg_data$Date %>% tail(7)), 
                y = (agg_data$`Consumption (MWH)` %>% tail(7))
                , colour = "actual"))+
  geom_line(aes(x = (agg_data$Date %>% tail(7)),
                y = ma_preds,   colour = "tbats "))+ 
  scale_y_continuous(labels = comma)+
  labs(
    title = "Daily Energy Consumption in Megawatts",
    x = "Date",
    y = "",
    colour = ""
  )


df_matrix <- as.matrix(agg_data[, -1])
rownames(df_matrix) <- agg_data[, 1]
par(mfrow=c(1,1))

# convert the matrix to a time series object
df.ts <- ts(df_matrix, start = c(2015, 365), frequency = 365)
acf(df.ts) #strong autocorrelation present
plot(df.ts, main = "Energy Consumption")

# select a single column from df.ts
df_univariate <- df.ts[,1]

# decompose the time series using stl()
df_stl <- stl(df_univariate, s.window = "periodic")
#trend, season and level present
plot((df_stl), main = "Energy Consumption")

#partition the data
df.valid.ts = ts(tail(agg_data$`Consumption (MWH)`,7), frequency = 365)
df.train.ts = ts(head(agg_data$`Consumption (MWH)`,1637), frequency = 365)
#Regression based model
train.lm.season = tslm(df.train.ts~season)
train.lm.trend.season = tslm(df.train.ts ~ trend + I(trend ^2) + season)
train.trend.season.pred = forecast(train.lm.trend.season, h=7, level = 0)

ggplot() +
  geom_line(aes(x = (agg_data$Date %>% tail(7)), 
                y = (agg_data$`Consumption (MWH)` %>% tail(7))
                , colour = "actual"))+
  geom_line(aes(x = (agg_data$Date %>% tail(7)),
                y = train.trend.season.pred$mean,   colour = "TSLM "))+ 
  scale_y_continuous(labels = comma)+
  labs(
    title = "Daily Energy Consumption in Megawatts",
    x = "Date",
    y = "",
    colour = ""
  )
accuracy(train.lm.trend.season)

#holt winter's
df.data.hw = HoltWinters(df.train.ts, beta=FALSE)
autoplot(forecast(df.data.hw)) + xlab("Time") + ylab("Consumption") +
  ggtitle("Forecast of Demand using Holt-Winter's Exponential Smoothing")

df.data.hw.forecast = forecast(df.data.hw,7)

acf(df.data.hw.forecast$residuals, na.action = na.pass)

ggplot() +
  geom_line(aes(x = (agg_data$Date %>% tail(7)), 
                y = (agg_data$`Consumption (MWH)` %>% tail(7))
                , colour = "actual"))+
  geom_line(aes(x = (agg_data$Date %>% tail(7)),
                y = df.data.hw.forecast$mean,   colour = "HoltWinters "))+ 
  scale_y_continuous(labels = comma)+
  labs(
    title = "Daily Energy Consumption in Megawatts",
    x = "Date",
    y = "",
    colour = ""
  )
p = as.data.frame(df.data.hw.forecast$mean)
o=as.data.frame(tail(agg_data$`Consumption (MWH)`,7))
mae(p$x,o$`tail(agg_data$\`Consumption (MWH)\`, 7)`)
rmse(p$x,o$`tail(agg_data$\`Consumption (MWH)\`, 7)`)


#ARIMA model
df.data.res.arima <- Arima(train.lm.trend.season$residuals, order = c(1,1,2))
df.data.res.arima.pred <- forecast(df.data.res.arima, h = 7)
summary(df.data.res.arima)
tsdiag(df.data.res.arima)
accuracy(df.data.res.arima)
df.data.arima <- auto.arima(df.train.ts)
summary(df.data.arima)
tsdiag(df.data.arima)


#SARIMA
df.data.sarima=arima(df.train.ts,order=c(1,1,2),seasonal=list(order=c(1,2,2),period=7))
df.data.res.sarima.pred <- forecast(df.data.sarima, h = 7)
accuracy(df.data.sarima)
m2=arima(df.train.ts,order=c(1,1,2)
         ,seasonal=list(order=c(1,2,2),period=1))
m2
par(mar=c(1,1,1,1))



#Duol Sesonlity
fit <- tbats(df.train.ts, seasonal.periods = c(365, 7))
# Make forecasts
h <- length(df.valid.ts)
fc <- forecast(fit, h = h)
ggplot() +
  geom_line(aes(x = (agg_data$Date %>% tail(7)), 
                y = (agg_data$`Consumption (MWH)` %>% tail(7))
                , colour = "actual"))+
  geom_line(aes(x = (agg_data$Date %>% tail(7)),
                y = fc$mean,   colour = "Dual Seasonality"))+ 
  scale_y_continuous(labels = comma)+
  labs(
    title = "Daily Energy Consumption in Megawatts",
    x = "Date",
    y = "",
    colour = ""
  )
# Compute accuracy measures
accuracy(fit)
summary(fc)

#auto arima
arima_model <- auto.arima(df.train.ts)
summary(arima_model)
forecast_values <- forecast(arima_model, h = length(df.valid.ts))
# Combine the actual and forecasted values into a single data frame
plot_data <- data.frame(actual = test_data, predicted = forecast_values$mean)

# Create a line plot
ggplot() +
  geom_line(aes(x = (agg_data$Date %>% tail(7)), 
                y = (agg_data$`Consumption (MWH)` %>% tail(7))
                , colour = "actual"))+
  geom_line(aes(x = (agg_data$Date %>% tail(7)),
                y = forecast_values$mean,   colour = "auto arima"))+ 
  scale_y_continuous(labels = comma)+
  labs(
    title = "Daily Energy Consumption in Megawatts",
    x = "Date",
    y = "",
    colour = ""
  )



# Load required library
library(ggplot2)
# Convert data frame to time series object
ts_data <- ts(agg_data$`Consumption (MWH)`, start = c(2015, 365), frequency = 365)
# Decompose the time series
decomp <- decompose(ts_data)
# Plot the decomposed components
autoplot(decomp)
train <- df.train.ts
test <- df.valid.ts
# Check the length of the training and testing sets
length(train)
length(test)
# Load required library
library(forecast)
# Generate forecasts for the testing set using the training set components
fc <- predict(decomp$x, h = 7)
# Create a line plot
ggplot() +
  geom_line(aes(x = (agg_data$Date %>% tail(7)), 
                y = (agg_data$`Consumption (MWH)` %>% tail(7))
                , colour = "actual"))+
  geom_line(aes(x = (agg_data$Date %>% tail(7)),
                y = fc$mean,   colour = "Decompose"))+ 
  scale_y_continuous(labels = comma)+
  labs(
    title = "Daily Energy Consumption in Megawatts",
    x = "Date",
    y = "",
    colour = ""
  )

#Neural Network
train_data <- train
test_data <- test
nn_model <- nnetar(train_data, size = 20)
nn_pred <- forecast(nn_model, h = length(test_data))
ggplot() +
  geom_line(aes(x = (agg_data$Date %>% tail(7)), 
                y = (agg_data$`Consumption (MWH)` %>% tail(7))
                , colour = "actual"))+
  geom_line(aes(x = (agg_data$Date %>% tail(7)),
                y = nn_pred$mean,   colour = "Neural Network"))+ 
  scale_y_continuous(labels = comma)+
  labs(
    title = "Daily Energy Consumption in Megawatts",
    x = "Date",
    y = "",
    colour = ""
  )

summary(nn_pred)

# Split the data into training and testing sets
train_data <- df.train.ts
test_data <- df.valid.ts
# Fit a dual seasonality model to the training data
model_ds <- dshw(train_data, biasadj = TRUE, period1 = 30, period2 = 60)
summary(model_ds)
# Forecast the test data
predictions <- forecast(model_ds, 7)
x1=as.data.frame(predictions$fitted)
df_data <- as.data.frame(train_data)
names(df_data) <- "x"
df3 <- merge(df_data, x1, sort = FALSE, all=TRUE )
ggplot() +
  geom_line(aes(x = (agg_data$Date %>% tail(7)), 
                y = (agg_data$`Consumption (MWH)` %>% tail(7))
                , colour = "actual"))+
  geom_line(aes(x = (agg_data$Date %>% tail(7)),
                y = head(predictions$mean, 7),   colour = "Dual Seasonality HoltWinter's"))+ 
  scale_y_continuous(labels = comma)+
  labs(
    title = "Daily Energy Consumption in Megawatts",
    x = "Date",
    y = "",
    colour = ""
  )
accuracy(model_ds)


# Split the data into training and testing sets

train_data <- df.train.ts
test_data <- df.valid.ts
# Fit a dual seasonality model to the training data
model_ds <- dshw(train_data, biasadj = TRUE, period1 = 30, period2 = 90)
summary(model_ds)
# Forecast the test data
predictions <- forecast(model_ds, 7)
x1=as.data.frame(predictions$fitted)
df_data <- as.data.frame(train_data)
names(df_data) <- "x"
df3 <- merge(df_data, x1, sort = FALSE, all=TRUE )
ggplot() +
  geom_line(aes(x = (agg_data$Date %>% tail(7)), 
                y = (agg_data$`Consumption (MWH)` %>% tail(7))
                , colour = "actual"))+
  geom_line(aes(x = (agg_data$Date %>% tail(7)),
                y = head(predictions$mean, 7),   colour = "Neural Network"))+ 
  scale_y_continuous(labels = comma)+
  labs(
    title = "Daily Energy Consumption in Megawatts",
    x = "Date",
    y = "",
    colour = ""
  )

