setwd("C:/Users/Pastor/Desktop/stock_market/R scripts")
source("arima_functions.R")

library(lubridate)
library(tidyverse)

# Define paths to datasets
path_file_main  <- file.path(dirname("C:/Users/Pastor/Dropbox/Pastor/data/futures_unadjusted_5/.."))
path_file_update <- file.path(dirname("C:/Users/Pastor/Dropbox/Pastor/data/futures_unadjusted_5_update/.."))


# futures instrument
instrument = "ES"
term = 1

data_main <- sprintf("%s_continuous_UNadjusted_5min.txt", instrument)
data_update <- sprintf("%s_5-min.txt", instrument)

# Main data
df <- read.table(file.path(path_file_main, data_main), sep = ",")
df_2 <- read.table(file.path(path_file_update, data_update), sep = ",") # daily updates

df <- rbind(df, df_2)
df <- df[!duplicated(df$V1),]

df <- plyr::rename(df, c("V1"= "Date","V2"= "Open", "V3"="High", "V4" = "Low",
                         "V5" = "Close", "V6" = "Volume"))

df$day_month <- as.Date(df$Date, format = "%Y-%m-%d")


setwd("C:/Users/Pastor/Dropbox/Pastor/data/MarketProfile_data")
write.csv(df, sprintf("%s_updated.csv", instrument), row.names = FALSE) # out the data

df_daily <- df %>% group_by(day_month) %>% summarise(Open = first(Open),
                                                       High = max(High),
                                                       Low = min(Low),
                                                       Close = last(Close),
                                                       Volume = sum(Volume))



df_daily_train <- df_daily %>% select(day_month, High, Low)

final <- arima_evaluation(df_daily_train, 50)

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


grafical_comparison(predi_arima_high, predi_arima_high$High_actual,
                    predi_arima_high$High_pred, High_low = "High")

## Daily predictions

df_daily <- head(df_daily,-1)

pred_high <- arima_model(df_daily, df_daily$High, term)
pred_low <- arima_model(df_daily, df_daily$Low, term)

last_high <- last(df_daily$High)
last_low <- last(df_daily$Low)


pred_high$mean[term]
pred_low$mean[term]


ifelse(pred_high$mean[term] < last_high, "Lower High", "Higher High")
ifelse(pred_low$mean[term] < last_low, "Lower Low", "Higher Low")

last(df_daily$day_month)
last(df$Date)
