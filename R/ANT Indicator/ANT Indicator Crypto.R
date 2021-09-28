library(tidyverse)
library(lubridate)
library(quantmod)
library(rlist)
library(tseries)
library(timeSeries)
library(forecast)
library(xts)
library(tidyquant)


# Read the weekly file
setwd("C:/Users/Pastor/Dropbox/Pastor/data/crypto")
filelist = list.files(pattern = ".*.txt")
crypto <- lapply(filelist, FUN=read.table, sep = ",", fill = TRUE)
filelist = str_remove_all(filelist, "[-]" )
names(crypto) <- filelist

# Read the daily update file
setwd("C:/Users/Pastor/Dropbox/Pastor/data/crypto-update")
filelist_update = list.files(pattern = ".*.txt")
crypto_update <- lapply(filelist_update, FUN=read.table, sep = ",", fill = TRUE)
filelist_update = str_remove_all(filelist_update, "[-]" )
names(crypto_update) <- filelist_update


nt <- which(!filelist %in% filelist_update)
crypto = crypto[-nt]
filelist <- filelist[-nt]
n <- 15

for (i in 1:length(crypto)) {
  crypto[[i]]$day_month <- as.Date(crypto[[i]]$V1, format = "%Y-%m-%d")
  crypto[[i]] <- crypto[[i]] %>% group_by(day_month) %>% summarise(Open = first(V2),
                                                                   High = max(V3),
                                                                   Low = min(V4),
                                                                   Close = last(V5),
                                                                   Volume = sum(V6))
  crypto[[i]]$crypto_name <- sub("\\_.*", "",filelist[i])
}

for (i in 1:length(crypto_update)) {
  crypto_update[[i]]$day_month <- as.Date(crypto_update[[i]]$V1, format = "%Y-%m-%d")
  crypto_update[[i]] <- crypto_update[[i]] %>% group_by(day_month) %>% summarise(Open = first(V2),
                                                                   High = max(V3),
                                                                   Low = min(V4),
                                                                   Close = last(V5),
                                                                   Volume = sum(V6))
  crypto_update[[i]]$crypto_name <- sub("\\_.*", "" ,filelist[i])
}

#crypto_all <- crypto

for (i in 1:length(crypto)) {
  crypto[[i]] <- rbind(crypto[[i]], crypto_update[[i]])
  crypto[[i]] <- crypto[[i]][!duplicated(crypto[[i]]$day_month),]
  
  #momentum is up at least 12 out of 15 days
  crypto[[i]]$ret <- with(crypto[[i]], log(Close/dplyr::lag(Close)))
  crypto[[i]] <- na.omit(crypto[[i]])
  crypto[[i]]$mom <- ifelse(crypto[[i]]$ret > 0, 1, 0)
  cs <- cumsum(crypto[[i]]$mom)
  crypto[[i]]$momentum_total <- c(rep_len(NA, n - 1), tail(cs, -(n - 1)) - c(0, head(cs, -n)))
  crypto[[i]]$momentum <- ifelse(crypto[[i]]$momentum_total > 11, 1,0)
  
  ## Price: The price is up at least 20% over the past 15 days
  crypto[[i]][["runmeanPrice"]] <- runMean(crypto[[i]][["Close"]], 15, FALSE)
  crypto[[i]]$price_mom <- round((crypto[[i]]$Close-crypto[[i]]$runmeanPrice)/(crypto[[i]]$runmeanPrice)*100,2)
  crypto[[i]]$pprice <- ifelse(crypto[[i]]$price_mom >= 20, 1, 0)
  
  ## The volume has increase over the past 15 days by 20% 
  crypto[[i]][["runmeanVol"]] <- round(runMean(crypto[[i]]["Volume"], 15, FALSE),2)
  crypto[[i]][["runmeanVol50"]] <- round(runMean(crypto[[i]]["Volume"], 50, FALSE),2)
  crypto[[i]]$volume_mon <- round(((crypto[[i]]$runmeanVol-crypto[[i]]$runmeanVol50)/(crypto[[i]]$runmeanVol50))*100,2)
  crypto[[i]]$vol <- ifelse(crypto[[i]]$volume_mon >= 20, 1, 0)
  
  
  # Ant indicator
  crypto[[i]]$gray <- ifelse(crypto[[i]]$momentum_total >= 12, 1, 0) # gray price is up by 20%
  crypto[[i]]$blue <- ifelse(crypto[[i]]$momentum == 1 & crypto[[i]]$pprice == 1, 1, 0) # price is up and is at least 20% up
  crypto[[i]]$yellow <- ifelse(crypto[[i]]$momentum == 1 & crypto[[i]]$vol==1, 1, 0) 
  crypto[[i]]$green <- ifelse(crypto[[i]]$momentum == 1 & crypto[[i]]$pprice == 1 & crypto[[i]]$vol == 1, 1, 0)
  
  
  crypto[[i]] <- na.omit(crypto[[i]])
}

crypto_list <- crypto %>% bind_rows %>% 
  select(day_month, crypto_name ,momentum_total, price_mom, volume_mon, gray, blue, yellow, green, Close,
         runmeanPrice, runmeanVol, runmeanVol50)

crypto_list_last <- subset(crypto_list, day_month == last(crypto_list$day_month))

ant_indicator <- crypto_list %>% group_by(crypto_name) %>% summarise(day_month = last(day_month),
                                                          momentum_total = last(momentum_total), # greater than 12
                                                          price_mom = round(last(price_mom),2), # price up 20%
                                                          volume_mon = round(last(volume_mon),2), # volume 20 to 25%
                                                          gray =  last(gray),
                                                          blue = last(blue), 
                                                          yellow = last(yellow),
                                                          green = last(green),
                                                          close = last(Close))

setwd("C:/Users/Pastor/Dropbox/Pastor/Power BI/Data")
write.csv(crypto_list, "crypto_list.csv", row.names = FALSE)