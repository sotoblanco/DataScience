setwd("C:/Users/Pastor/Desktop/stock_market/R scripts")
library(tidyverse)
library(quantmod)
library(tidyquant)
source("MarketProfile.R")

df_profile_night <- profile_point_night(timeframe = 5)

df_profile_day <- profile_point_day(timeframe = 5)

prange_night <- previous_range_day_night(df_profile_day, df_profile_night)

yest_range <- prange_night

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
                                 dist_poc_open_cat, open_close_dist, open_high_dist, above_3, above_5, above_8, above_12)


raw_data <- yest_range %>% select(day_month, High_A:Low_N, Low, High)


df_data_2 <- df_data %>% rename(pClose_touched = pClose, pClose_touched.night = pClose.night, popen_touched = pOpen,
                                popen_touched.night = pOpen.night, pVAH_touched = pVAH, pVAH_touched.night = pVAH.night,
                                pVAL_touched = pVAL, pVAL_touched.night = pVAL.night, pVOC_touched = pPOC, pVOC_touched.night = pPOC.night)

# Zero and one
df_data_23 <- df_data_2 %>% mutate(across(c(pClose_touched:pPOC_median.night),~case_when(.!= "0" ~ 1, TRUE ~ 0)))
setwd("C:/Users/Pastor/Desktop/stock_market")
write.csv(df_data_23, "all_data.csv", row.names = FALSE)

# Raw data without the touch data
setwd("C:/Users/Pastor/Desktop/stock_market/futures/clean_data")
write.csv(raw_data, "raw_data.csv", row.names = FALSE)

# use touch data for power bi model after step 5
write.csv(df_data, "touch data.csv", row.names = FALSE)

write.csv(yest_range, "yest_range.csv", row.names = FALSE)

#setwd("C:/Users/Pastor/Desktop/stock_market/futures")
#write.csv(df_fix_med, "overnight session.csv", row.names = FALSE)