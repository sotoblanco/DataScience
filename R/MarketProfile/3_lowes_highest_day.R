instrument = "ES"

path_file_raw_data<- file.path(dirname(sprintf("C:/Users/Pastor/Dropbox/Pastor/data/MarketProfile_data/%s/..", instrument)))
setwd(sprintf("C:/Users/Pastor/Dropbox/Pastor/data/MarketProfile_data/%s", instrument))

all_data <- read.csv(sprintf("%s_raw_data.csv", instrument))
all_data$day_month <- as.Date(all_data$day_month)
df_data <- read.csv(sprintf("%s_touch_data.csv", instrument))
df_data$day_month <- as.Date(df_data$day_month)

all_data <- all_data %>%  mutate(Lowest_Day = case_when(Low_A == Low~"A",
                                                        Low_B == Low~"B",
                                                        Low_C == Low~"C",
                                                        Low_D == Low~"D",
                                                        Low_E == Low~"E",
                                                        Low_F ==Low~"F",
                                                        Low_G ==Low~"G",
                                                        Low_H ==Low~"H",
                                                        Low_I ==Low~"I",
                                                        Low_J ==Low~"J",
                                                        Low_K ==Low~"K",
                                                        Low_L ==Low~"L",
                                                        Low_M ==Low~"M",
                                                        Low_N ==Low~"N"))

all_data <- all_data %>%  mutate(Highest_Day = case_when(High_A == High~"A",
                                                         High_B == High~"B",
                                                         High_C == High~"C",
                                                         High_D == High~"D",
                                                         High_E == High~"E",
                                                         High_F ==High~"F",
                                                         High_G ==High~"G",
                                                         High_H ==High~"H",
                                                         High_I ==High~"I",
                                                         High_J ==High~"J",
                                                         High_K ==High~"K",
                                                         High_L ==High~"L",
                                                         High_M ==High~"M",
                                                         High_N ==High~"N"))

all_data <- all_data %>% select(day_month, Lowest_Day, Highest_Day)

df_data <- df_data %>% inner_join(all_data, by = "day_month") %>% select(day_month:A_higher_B,trend, Lowest_Day, Highest_Day, dist_poc_open, dist_poc_night_open, dist_poc_night_open_cat, dist_poc_open_cat)

write.csv(df_data, sprintf("%s_touch data.csv", instrument), row.names = FALSE)

### power BI data
# touch data clean_data
# timeframe up & down (diff_OHLC)
# low, high, quantile (diff_OHLC)