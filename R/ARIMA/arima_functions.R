## Arima functions

arima_model <- function(data, OHLC, term = 1){
  library(forecast)
  fitA = auto.arima(OHLC, seasonal = FALSE)
  fcast1 <- forecast(fitA, h = term)
  return(fcast1)
  
}

arima_evaluation <- function(train_data, number){
  result_high <- list() # Store result as list
  result_high_upper_80 <- list()
  result_high_lower_80 <- list()
  
  result_low <- list()
  result_low_upper_80 <- list()
  result_low_lower_80 <- list()
  
  date <- list() # store date as list
  for (i in 1:number) { # create a loop to evaluate different days of forecasting with ARIMA
    data <- train_data %>% dplyr::filter(row_number() <= n()-i) # delete the observations we want to predict from the data
    day_month <- last(data$day_month)+1 # create the date var of the forecasting day
    
    # High arima
    High_pred <- arima_model(data, data$High, term = 1) # create the arima model
    High <- High_pred$mean[1]
    upper_high_80 <- High_pred$upper[1]
    lower_high_80 <- High_pred$lower[1]
    
    # Low arima
    Low_pred <- arima_model(data, data$Low, term = 1)
    Low <- Low_pred$mean[1]
    upper_low_80 <- Low_pred$upper[1]
    lower_low_80 <- Low_pred$lower[1]
    
    result_high[i] <- list(High)  # store the results in the list
    result_high_upper_80[i] <- list(upper_high_80)
    result_high_lower_80[i] <- list(lower_high_80)
    
    result_low[i] <- list(Low)
    result_low_upper_80[i] <- list(upper_low_80)
    result_low_lower_80[i] <- list(lower_low_80)
    
    
    date[i] <- list(day_month)
    result_final <- list(result_high, result_high_upper_80, result_high_lower_80,
                         result_low, result_low_upper_80, result_low_lower_80, date)
  }
  
  for (i in 1:number) {
    names(result_final[[1]][[i]]) <- "High"
    names(result_final[[2]][[i]]) <- "Upper_high_80"
    names(result_final[[3]][[i]]) <- "lower_high_80"
    
    names(result_final[[4]][[i]]) <- "Low"
    names(result_final[[5]][[i]]) <- "Upper_low_80"
    names(result_final[[6]][[i]]) <- "lower_low_80"
    
    names(result_final[[7]][[i]]) <- "day_month"
  }
  
  xsd <- result_final %>% bind_rows() %>% select(day_month, 
                                                 High, Upper_high_80,lower_high_80,
                                                 Low, Upper_low_80, lower_low_80)
  
  day_month <- na.omit(xsd$day_month)
  
  High <- na.omit(xsd$High)
  upper_high_80 <- na.omit(xsd$Upper_high_80)
  lower_high_80 <- na.omit(xsd$lower_high_80)
  
  Low <- na.omit(xsd$Low)
  upper_low_80 <- na.omit(xsd$Upper_low_80)
  lower_low_80 <- na.omit(xsd$lower_low_80)
  
  
  data_high <- data.frame(day_month, High, upper_high_80, lower_high_80, 
                          Low, upper_low_80, lower_low_80)
  
  
  
  return(data_high)
  
  
  
}

pred_vs_actual <- function(data, actual_value, pred_value, high_low){
  
  if (high_low == 1) {
    
    data$actual <- ifelse(actual_value > dplyr::lag(actual_value), "Up", "Down")
    data$pred <- ifelse(pred_value > dplyr::lag(actual_value), "Up", "Down")
    data$conf_interval_lag <- ifelse(dplyr::lag(actual_value) >= data$lower_high_80 & dplyr::lag(actual_value) <= data$upper_high_80, "Inside", "Outside")
    data$Low_actual <- NULL
    data$Low_pred <- NULL
    data$upper_low_80 <- NULL
    data$lower_low_80 <- NULL
    data$conf_interval_accuracy_actual <- ifelse(actual_value >= data$lower_high_80 & actual_value <= data$upper_high_80, "Inside", "Outside")
    
    
  }
  
  if (high_low == 0) {
    data$actual <- ifelse(actual_value < dplyr::lag(actual_value), "Down", "Up")
    data$pred <- ifelse(pred_value < dplyr::lag(actual_value), "Down", "Up")
    data$conf_interval_lag <- ifelse(dplyr::lag(actual_value) >= data$lower_low_80 & dplyr::lag(actual_value) <= data$upper_low_80, "Inside", "Outside")
    data$High_actual <- NULL
    data$High_pred <- NULL
    data$upper_high_80 <- NULL
    data$lower_high_80 <- NULL
    data$conf_interval_accuracy_actual <- ifelse(actual_value >= data$lower_low_80 & actual_value <= data$upper_low_80, "Inside", "Outside")
    
    
    
  }
  
  data$accuracy <- ifelse(data$actual == data$pred, "Yes", "No")
  data$ret_pred <- round((pred_value - dplyr::lag(actual_value))/dplyr::lag(actual_value),4)
  data$ret_actual <- round((actual_value - dplyr::lag(actual_value))/dplyr::lag(actual_value),4)
  return(data)
}

grafical_comparison <- function(data, actual_value, pred_value, High_low, instrument){
  
  p <- ggplot(data, aes(x = day_month)) + 
    geom_line(aes(y = actual_value, color = "Actual")) + 
    geom_line(aes(y = pred_value, color = "Predicted"))+
    ggtitle(sprintf("%s Predicted vs Actual return from the %s of the day", instrument, High_low))
  
  plz <- plotly::ggplotly(p)
  return(plz)
  
}


