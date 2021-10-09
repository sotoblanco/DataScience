library(lubridate)
library(tidyverse)

instrument = c("NQ", "ES", "BTC", "ETH")

for (instrument in instrument) {
  if (instrument == "NQ" | instrument == "ES") {
    path_file_main  <- file.path(dirname("C:/Users/Pastor/Dropbox/Pastor/data/futures_unadjusted_5/.."))
    path_file_update <- file.path(dirname("C:/Users/Pastor/Dropbox/Pastor/data/futures_unadjusted_5_update/.."))
    data_main <- sprintf("%s_continuous_UNadjusted_5min.txt", instrument)
    data_update <- sprintf("%s_5-min.txt", instrument)
  }
  
  if (instrument == "BTC"| instrument == "ETH") {
    path_file_main  <- file.path(dirname("C:/Users/Pastor/Dropbox/Pastor/data/crypto-active_5min/.."))
    path_file_update <- file.path(dirname("C:/Users/Pastor/Dropbox/Pastor/data/crypto-active_week-update_5-min/.."))
    data_main <- sprintf("%s_5min.txt", instrument)
    data_update <- sprintf("%s_5-min.txt", instrument)
  }
  
  # Main data
  df <- read.table(file.path(path_file_main, data_main), sep = ",")
  df_2 <- read.table(file.path(path_file_update, data_update), sep = ",") # daily updates
  
  df <- rbind(df, df_2)
  df <- df[!duplicated(df$V1),]
  
  df <- plyr::rename(df, c("V1"= "Date","V2"= "Open", "V3"="High", "V4" = "Low",
                           "V5" = "Close", "V6" = "Volume"))
  
  if (instrument == "BTC"| instrument == "ETH") {
    df$Date  <- ymd_hms(df$Date)
    df$Date <- df$Date - hours(4)
  }
  
  setwd("C:/Users/Pastor/Dropbox/Pastor/data/MarketProfile_data")
  write.csv(df, sprintf("%s_updated.csv", instrument), row.names = FALSE) # out the data
}

