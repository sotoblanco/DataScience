library(lubridate)
library(tidyverse)

market_instrument = "Crypto"
instrument = "BTC"

if (market_instrument == "Futures") {
  path_file_main  <- file.path(dirname("C:/Users/Pastor/Dropbox/Pastor/data/futures_unadjusted_5/.."))
  path_file_update <- file.path(dirname("C:/Users/Pastor/Dropbox/Pastor/data/futures_unadjusted_5_update/.."))
  data_main <- sprintf("%s_continuous_UNadjusted_5min.txt", instrument)
  data_update <- sprintf("%s_5-min.txt", instrument)
}

if (market_instrument == "Crypto") {
  
  path_file_main  <- file.path(dirname("C:/Users/Pastor/Dropbox/Pastor/data/crypto-active_5min/.."))
  path_file_update <- file.path(dirname("C:/Users/Pastor/Dropbox/Pastor/data/crypto-active_week-update_5-min/.."))
  data_main <- sprintf("%s_5min.txt", instrument)
  data_update <- sprintf("%s_5-min.txt", instrument)
  
}

path_file_mp_funcitons <- file.path(dirname("C:/Users/Pastor/Desktop/stock_market/DataScience/R/MarketProfile/.."))
source(file.path(path_file_mp_funcitons, "MarketProfile_Futures.R")) # store the functions for futures Marketprofile

# Main data
df <- read.table(file.path(path_file_main, data_main), sep = ",")
df_2 <- read.table(file.path(path_file_update, data_update), sep = ",") # daily updates

df <- rbind(df, df_2)
df <- df[!duplicated(df$V1),]

df <- plyr::rename(df, c("V1"= "Date","V2"= "Open", "V3"="High", "V4" = "Low",
                         "V5" = "Close", "V6" = "Volume"))

if (market_instrument == "Crypto") {
  df$Date  <- ymd_hms(df$Date)
  df$Date <- df$Date - hours(4)
}

setwd("C:/Users/Pastor/Dropbox/Pastor/data/MarketProfile_data")
write.csv(df, sprintf("%s_updated.csv", instrument), row.names = FALSE) # out the data

# We need a dataframe with Date, Open, High, Low, Close, and VOlume variables
# functions from MarketProfile futures

NQ_profile <- profile_point_night(df) # provides night marketprofile 

NQ_profile_day <- profile_point_day(df) # provides day marketprofile

p_range_night <- previous_range_day_night(NQ_profile_day, NQ_profile) # provides previous structural point touch


yest_range <- p_range_night

yest_range$A_lower_B <- with(yest_range, ifelse(Low_A < Low_B,1,0))
yest_range$A_higher_B <- with(yest_range, ifelse(High_A > High_B, 1, 0))
yest_range$trend <- with(yest_range, ifelse(Close > Open, "up", "down"))


##############
yest_range <- yest_range %>% mutate(Lowest_day = case_when(Low_A == Low ~ "A",
                                                           Low_B == Low ~ "B",
                                                           Low_C == Low ~ "C",
                                                           Low_D == Low ~ "D",
                                                           Low_E == Low ~ "E",
                                                           Low_F == Low ~ "F",
                                                           Low_G == Low ~ "G",
                                                           Low_H == Low ~ "H",
                                                           Low_I == Low ~ "I",
                                                           Low_J == Low ~ "J",
                                                           Low_K == Low ~ "K",
                                                           Low_L == Low ~ "L",
                                                           Low_M == Low ~ "M",
                                                           Low_N == Low ~ "N"))


yest_range <- yest_range %>% mutate(Highest_day = case_when(High_A == High ~ "A",
                                                            High_B == High ~ "B",
                                                            High_C == High ~ "C",
                                                            High_D == High ~ "D",
                                                            High_E == High ~ "E",
                                                            High_F == High ~ "F",
                                                            High_G == High ~ "G",
                                                            High_H == High ~ "H",
                                                            High_I == High ~ "I",
                                                            High_J == High ~ "J",
                                                            High_K == High ~ "K",
                                                            High_L == High ~ "L",
                                                            High_M == High ~ "M",
                                                            High_N == High ~ "N"))


yest_range$dist_poc_night_open <- round(abs((dplyr::lag(yest_range$POC.night) - yest_range$Open)/yest_range$Open*last(yest_range$Open)),2)
yest_range$dist_poc_open <- round(abs((dplyr::lag(yest_range$POC) - yest_range$Open)/yest_range$Open*last(yest_range$Open)),2)

quant <- quantile(yest_range$dist_poc_open, na.rm = TRUE)

yest_range <- yest_range %>% mutate(dist_poc_open_cat = case_when(dist_poc_open <= quant[2]~quant[2],
                                                                  dist_poc_open > quant[2] & dist_poc_open <= quant[3]~quant[3],
                                                                  dist_poc_open > quant[3] & dist_poc_open <= quant[4]~quant[4],
                                                                  dist_poc_open > quant[4] & dist_poc_open <= quant[5]~quant[5]))

quant <- quantile(yest_range$dist_poc_night_open, na.rm = TRUE)

yest_range <- yest_range %>% mutate(dist_poc_night_open_cat = case_when(dist_poc_night_open <= quant[2]~quant[2],
                                                                        dist_poc_night_open > quant[2] & dist_poc_night_open <= quant[3]~quant[3],
                                                                        dist_poc_night_open > quant[3] & dist_poc_night_open <= quant[4]~quant[4],
                                                                        dist_poc_night_open > quant[4] & dist_poc_night_open <= quant[5]~quant[5]))



yest_range$open_close_dist <- round((yest_range$Close - yest_range$Open)/yest_range$Open * last(yest_range$Open),2)
yest_range$open_high_dist <- round((yest_range$High-yest_range$Open)/yest_range$Open * last(yest_range$Open),2)


df_data <- yest_range %>% select(day_month, range:pPOC_median, pClose.night:A_higher_B, trend, Lowest_day,
                                 Highest_day, dist_poc_night_open, dist_poc_open, dist_poc_night_open_cat,
                                 dist_poc_open_cat, open_close_dist, open_high_dist)


raw_data <- yest_range %>% select(day_month, High_A:Low_N, Low, High)


df_data_2 <- df_data %>% rename(pClose_touched = pClose, pClose_touched.night = pClose.night, popen_touched = pOpen,
                                popen_touched.night = pOpen.night, pVAH_touched = pVAH, pVAH_touched.night = pVAH.night,
                                pVAL_touched = pVAL, pVAL_touched.night = pVAL.night, pVOC_touched = pPOC, pVOC_touched.night = pPOC.night)

# Zero and one
df_data_23 <- df_data_2 %>% mutate(across(c(pClose_touched:pPOC_median.night),~case_when(.!= "0" ~ 1, TRUE ~ 0)))


# POC TRADING SYSTEM

## The distance is below or above the overnight POC

df_data_23$poc_loc <- with(yest_range, ifelse(dplyr::lag(POC.night) > Open, "Up", "Down"))

df_data_23$open_low_dist <- with(yest_range, round((Open-Low)/Open*last(Open)),2)

## MAE Maximun adverse execution (If the OPEN is below the POC we evaluate as the low of the day)

df_data_23$MAE <- with(df_data_23, ifelse(poc_loc == "Up", open_low_dist, open_high_dist))

## MFE Maximun Favorable excursion (If the OPEN is below the POC we evaluate as the high of the day)

df_data_23$MFE <- with(df_data_23, ifelse(poc_loc == "Up", open_high_dist, open_low_dist))


# set the files into a folder

setwd(sprintf("C:/Users/Pastor/Dropbox/Pastor/data/MarketProfile_data/%s", instrument))
write.csv(df_data_23, sprintf("%s_all_data.csv", instrument), row.names = FALSE) # out the data
write.csv(raw_data, sprintf("%s_raw_data.csv", instrument), row.names = FALSE) # out the data
write.csv(df_data, sprintf("%s_touch_data.csv", instrument), row.names = FALSE) # out the data
write.csv(df_data_23, sprintf("%s_yest_range.csv", instrument), row.names = FALSE) # out the data

