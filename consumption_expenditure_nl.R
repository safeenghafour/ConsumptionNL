# Title: "Consumption Expenditure of Households in The Netherlands"
# Subtitle: 'HarvardX PH125.9x - Data Science: Capstone'
# Author: "Safeen Ghafour"
# Date: "12/25/2020"


# Install packages if necessary


#tidyverse
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

#kableExtra
if (!require(kableExtra)) install.packages('kableExtra')
library(kableExtra)

#fpp2
if (!require(fpp2)) install.packages('fpp2')
library(fpp2)

#TSstudio
if (!require(TSstudio)) install.packages('TSstudio')
library(TSstudio)

#tseries
if (!require(tseries)) install.packages('tseries')
library(tseries)


# Consumption Expenditure of Households
# Download CSV or Read from local machine
consumption <- read.csv("https://raw.githubusercontent.com/safeenghafour/ConsumptionNL/master/82608ENG_UntypedDataSet_26122020_004554.csv", 
                        sep = ";", 
                        header = TRUE, 
                        stringsAsFactors = FALSE)

# explore variables
str(consumption)

# Category names
Key <- c('A047812', 'A047813', 'A047875', 'A047825', 'A047826', 'A047827', 'A047828', 'A047829', 'A047830', 'A047831', 'A048214', 'A047832', 'A048213', 'A047837')
Name <- c(
  '1 Domestic consumption by households', 
  '1.1 Consumption of goods by households', 
  '1.1.1 Foodproducts, beverages and tabacco', 
  '1.1.2 Durable consumer goods', 
  '1.1.2.1 Textiles and clothing', 
  '1.1.2.2 Leather goods and footwear', 
  '1.1.2.3 Home furnishing and home decoration', 
  '1.1.2.4 Electrical equipment', 
  '1.1.2.5 Vehicles', 
  '1.1.2.6 Other durable consumer goods n.e.c.', 
  '1.1.3 Other goods', 
  '1.1.4 Electricity, gas, water and motor fuels', 
  '1.1.5 Personal care and other goods', 
  '1.2 Consumption of services by households')

# Date preparation, extract year and month columns, rename columns to more meaningful names
consumption_modified <- consumption %>%
  filter( grepl("MM", Periods) ) %>%
  mutate(
    date = as.Date(paste(gsub("MM", "-", Periods), "-01", sep = "")),
    year = as.factor(substr(Periods, 1, 4)),
    month = as.factor(substr(Periods, 7, 8)),
    cvalue = as.numeric(Indices_4)) %>%
  rename(
    category_id = ConsumptionByHouseholds,
    consumption_value = cvalue
  ) %>%
  summarise(category_id, year, month, consumption_value)

# label the levels
consumption_modified$category <- 
  factor(consumption_modified$category_id, 
         levels = Key,
         labels = Name
  )

str(consumption_modified)

head(consumption_modified)

  
# Choose top category 'Domestic consumption by households'
general_consumption <- consumption_modified %>%
  filter(category_id == "A047812")

# Create time series object
general_consumption_ts <- ts(
  general_consumption$consumption_value,
  start = c(2000, 1),
  end = c(2020, 10),
  frequency = 12
)

ts_info(general_consumption_ts)

# plot trend component
plot(general_consumption_ts, col='coral1', xlab="Year", ylab="Value", main = "Trend component")
grid (14, 10, lty = 1, col = "#dddddd") 

# plot seasonal component
boxplot(general_consumption_ts~cycle(general_consumption_ts), xlab = "Year", ylab = "Value", col = "#fff9f9", main="Seasonal component")

# plot the decomposition
plot(decompose(general_consumption_ts), xlab="Year", col="coral1")

# plot ACF
acf(general_consumption_ts, col="coral1", main = "ACF")

# plot pacf
pacf(general_consumption_ts, col="coral1", main = "PACF")

# Stationarity test: Dickey Fuller Test
print(adf.test(general_consumption_ts))

# Dickey Fuller Test on one diff (waring: p-value smaller than printed p-value)
print(adf.test(diff(general_consumption_ts, differences=1)))

# test partintion size
p <- 60

# split the main dataset
general_consumption_split <- ts_split(ts.obj = general_consumption_ts, sample.out = p)
train <- general_consumption_split$train
test <- general_consumption_split$test

# training set
ts_info(train)

# testing set
ts_info(test)

# Forecast 60 months, the same period as the test partition
h <- 60

# Navie model for benchmarking
naive_model <- naive(train, h = h)
# The model's forecast
naive_forecast <- forecast(naive_model, h)
# The model's accuracy
naive_accuracy <- forecast::accuracy(naive_forecast, test)
# Print RMSE & MAPE
results <- tibble(Method = "Naive", RMSE = naive_accuracy[4], MAPE = naive_accuracy[10])

results

# Holt's Trend
holt_model <- holt(train, h = h)
holt_forecast <- forecast(holt_model, h)
holt_accuracy <- forecast::accuracy(holt_forecast, test)

results <- results %>%
  add_row(Method = "Holt", RMSE = holt_accuracy[4], MAPE = holt_accuracy[10])
results

# ARIMA Model (Takes some time)
arima_model <- auto.arima(train, stepwise = FALSE, approximation = FALSE, seasonal=TRUE)
arima_forecast <- forecast(arima_model, h)
arima_accuracy <- forecast::accuracy(arima_forecast, test)

results <- results %>%
  add_row(Method = "ARIMA", RMSE = arima_accuracy[4], MAPE = arima_accuracy[10])
results

# check residuals of ARIMA Model
checkresiduals(arima_forecast)

# Neural Network AutoRegression Model (NNAR)
set.seed(1973)
# λ=0.5 to ensure the residuals will be roughly homoscedastic.
# 4 hidden layers
nn_model <-  nnetar(train, lambda = 0.5, repeats = 40, size = 4)
nn_forecast <- forecast(nn_model, h)
nn_accuracy <- forecast::accuracy(nn_forecast, test)

results <- results %>%
  add_row(Method = "Neural Network", RMSE = nn_accuracy[4], MAPE = nn_accuracy[10])
results

# Check residuals of the NNAR Model
Box.test(nn_forecast$residuals, lag = 4, type="Ljung-Box") 
checkresiduals(nn_forecast)
