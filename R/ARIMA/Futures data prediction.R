setwd("C:/Users/Pastor/Desktop/stock_market/DataScience/R/ARIMA")
source("arima_functions.R")

library(lubridate)
library(tidyverse)

# Define paths to datasets
instrument = c("NQ", "ES", "BTC", "ETH")
term = 1

for (instrument in instrument) {
  path_file_update <- file.path(dirname("C:/Users/Pastor/Dropbox/Pastor/data/MarketProfile_data/.."))
  data_main <- sprintf("%s_updated.csv", instrument)
  
  df <- read.csv(file.path(path_file_update, data_main))
  
  df$day_month <- as.Date(df$Date, format = "%Y-%m-%d")
  
  df_daily <- df %>% group_by(day_month) %>% summarise(Open = first(Open),
                                                       High = max(High),
                                                       Low = min(Low),
                                                       Close = last(Close),
                                                       Volume = sum(Volume))
  
  ## Overall performance of the model
  df_daily_train <- df_daily %>% select(day_month, High, Low)
  
  final <- arima_evaluation(df_daily_train, 14)
  
  final <- final[order(final$day_month),]
  
  date_last <- tail(df_daily_train, nrow(final))
  
  final$day_month <- date_last$day_month
  
  df_daily_test <- df_daily %>% select(day_month, High, Low)
  
  test_arima <- df_daily_test %>% inner_join(final, by = "day_month", suffix = c("_actual", "_pred"))
  
  # High of the day prediction value
  predi_arima_high <- pred_vs_actual(test_arima, test_arima$High_actual, test_arima$High_pred, 1)
  predi_arima_high$diff_pred_actual <- predi_arima_high$High_actual - predi_arima_high$High_pred
  
  # Low of the day prediction value
  predi_arima_low <- pred_vs_actual(test_arima, test_arima$Low_actual, test_arima$Low_pred, 0)
  predi_arima_low$diff_pred_actual <- predi_arima_low$Low_actual - predi_arima_low$Low_pred
  
  
  #grafical_comparison(predi_arima_high, predi_arima_high$High_actual,
  #                    predi_arima_high$High_pred, High_low = "High", instrument = instrument)
  
  
  #cat("Accuracy High of the day",round(sum(predi_arima_high$accuracy == "Yes", na.rm = TRUE)/nrow(predi_arima_high),4)*100,"%")
  #cat("Accuracy Low of the day",round(sum(predi_arima_low$accuracy == "Yes", na.rm = TRUE)/nrow(predi_arima_low),4)*100, "%")
  
  pred_update <- predi_arima_high
  pred_update$Low_actual <- predi_arima_low$Low_actual
  pred_update$Low_pred <- round(predi_arima_low$Low_pred,2)
  pred_update$instrument <- instrument
  pred_update$High_pred <- round(pred_update$High_pred,2)
  
  pred_update <- pred_update %>% select(day_month, instrument, High_actual,High_pred, Low_actual, Low_pred)
  
  
  ## Daily predictions
  
  #df_daily <- head(df_daily,-1)
  
  pred_high <- arima_model(df_daily, df_daily$High, term)
  pred_low <- arima_model(df_daily, df_daily$Low, term)
  
  last_high <- last(df_daily$High)
  last_low <- last(df_daily$Low)
  
  High_pred <- pred_high$mean[term]
  Low_pred <- pred_low$mean[term]
  
  High_actual <- NA
  Low_actual <- NA
  
  day_month <- last(df_daily$day_month)+1
  
  new_pred <- data.frame(day_month, instrument, High_actual, High_pred, Low_actual, Low_pred)
  
  pred_update <- rbind(pred_update, new_pred)
  
  pred_update$High_direction_actual <- with(pred_update, ifelse(High_actual > dplyr::lag(High_actual),"Higher High", "Lower High" ))
  pred_update$High_direction_pred <- with(pred_update, ifelse(High_pred > dplyr::lag(High_actual),"Higher High", "Lower High" ))
  pred_update$Low_direction_actual <- with(pred_update, ifelse(Low_actual > dplyr::lag(Low_actual),"Higher Low", "Lower Low" ))
  pred_update$Low_direction_pred <- with(pred_update, ifelse(Low_pred > dplyr::lag(Low_actual),"Higher Low", "Lower Low" ))
  
  setwd("C:/Users/Pastor/Dropbox/Pastor/ARIMA")
  write.csv(pred_update, sprintf("%s_ARIMA.csv", instrument), row.names = FALSE) # out the data
}

