# 1_overnight_session

## Night MP
tpo30_night <- function(dataframe){
  library(lubridate)
  library(tidyverse)

  # Format date
  dataframe$day_month <- as.Date(dataframe$Date, format = "%Y-%m-%d")
  dataframe$hours <- format(strptime(dataframe$Date, "%Y-%m-%d %H:%M:%S"), "%H:%M")
  dataframe$weekday <- weekdays(dataframe$day_month)
  
  # Get just night data
  dataframe <- subset(dataframe, hours >= "16:15" & hours <= "23:55" | hours >= "00:00" & hours < "09:30")
  dataframe$Date <- as.POSIXct(dataframe$Date, format = "%Y-%m-%d %H:%M:%S")
  
  dataframe$Date <- dataframe$Date - hours(10) # we subtract 10 hours so all night session has the same day (i.e overnight wednesday is tuesday)
  dataframe$day_month <- format(strptime(dataframe$Date, format = "%Y-%m-%d %H:%M:%S"), "%Y-%m-%d")
  dataframe$day_month <- as.Date(dataframe$day_month)
  dataframe$weekday <- weekdays(dataframe$day_month)
  
  # Get sundays
  df_sunday <- subset(dataframe, weekday == "Sunday")
  df_sunday$Date <- as.POSIXct(df_sunday$Date, format = "%Y-%m-%d %H:%M:%S")
  df_sunday$Date <- df_sunday$Date - days(2) # subtract two days so sunday will be friday
  df_sunday$day_month <- format(strptime(df_sunday$Date, format = "%Y-%m-%d %H:%M:%S"), "%Y-%m-%d")
  df_sunday$day_month <- as.Date(df_sunday$day_month)
  df_sunday$weekday <- weekdays(df_sunday$day_month)
  
  # join the sunday data and the rest of the days
  df_not_sunday <- subset(dataframe, weekday != "Sunday")
  
  df_sunday_as_friday <- rbind(df_not_sunday, df_sunday)
  df <- df_sunday_as_friday[order(as.Date(df_sunday_as_friday$Date, format = "%Y-%m-%d %H:%M:%S")),]
  rownames(df) <- 1:length(rownames(df))
  df$hours <- format(strptime(df$Date, "%Y-%m-%d %H:%M:%S"), "%H:%M")

  df_subset_night <- df %>% select(Open, High, Low, Close, Volume, day_month, hours)
  
  df_subset_night <- df_subset_night[!df_subset_night$day_month == "2006-04-14",]
  df_open_night <- df_subset_night %>% mutate(let =  case_when(hours >= "06:30" & hours < "06:45"~"A",
                                                               hours >= "06:45" & hours < "07:00"~"B",
                                                               hours >= "08:00" & hours < "08:15"~"D",
                                                               hours >= "08:15" & hours < "08:45"~"E",
                                                               hours >= "08:45" & hours < "09:15"~"F",
                                                               hours >= "09:15" & hours < "09:45"~"G",
                                                               hours >= "09:45" & hours < "10:15"~"H",
                                                               hours >= "10:15" & hours < "10:45"~"I",
                                                               hours >= "10:45" & hours < "11:15"~"J",
                                                               hours >= "11:15" & hours < "11:45"~"K",
                                                               hours >= "11:45" & hours < "12:15"~"L",
                                                               hours >= "12:15" & hours < "12:45"~"M",
                                                               hours >= "12:45" & hours < "13:15"~"N",
                                                               hours >= "13:15" & hours < "13:45"~"O",
                                                               hours >= "13:45" & hours < "14:15"~"P",
                                                               hours >= "14:15" & hours < "14:45"~"Q",
                                                               hours >= "14:45" & hours < "15:15"~"R",
                                                               hours >= "15:15" & hours < "15:45"~"S",
                                                               hours >= "15:45" & hours < "16:15"~"T",
                                                               hours >= "16:15" & hours < "16:45"~"U",
                                                               hours >= "16:45" & hours < "17:15"~"V",
                                                               hours >= "17:15" & hours < "17:45"~"W",
                                                               hours >= "17:45" & hours < "18:15"~"X",
                                                               hours >= "18:15" & hours < "18:45"~"a",
                                                               hours >= "18:45" & hours < "19:15"~"b",
                                                               hours >= "19:15" & hours < "19:45"~"c",
                                                               hours >= "19:45" & hours < "20:15"~"d",
                                                               hours >= "20:15" & hours < "20:45"~"e",
                                                               hours >= "20:45" & hours < "21:15"~"f",
                                                               hours >= "21:15" & hours < "21:45"~"g",
                                                               hours >= "21:45" & hours < "22:15"~"h",
                                                               hours >= "22:15" & hours < "22:45"~"i",
                                                               hours >= "22:45" & hours < "23:15"~"j",
                                                               hours >= "23:15" & hours < "23:30"~"k"))
  
  
  df_open_night <- na.omit(df_open_night)
  df_open_night_2 <- df_open_night %>% group_by(day_month, let) %>% summarise(day_month = first(day_month),
                                                                              High = max(High),
                                                                              Low = min(Low)) 
  
  df_fix <- df_open_night_2 %>%
    arrange(day_month, let) %>%
    pivot_wider(names_from = let, values_from = c(High, Low)) %>%
    select(day_month, order(sub('.*_', '', names(.))))
  
  df_median <- df_open_night %>% group_by(day_month) %>% summarise(day_month = first(day_month),
                                                                   High = max(High),
                                                                   Low = min(Low),
                                                                   POC_median = (High+Low)/2,
                                                                   Open = first(Open),
                                                                   Close = last(Close))
  
  df_fix_med <- merge(df_fix, df_median, by = "day_month")
  df_fix_med$day_month <- as.Date(df_fix_med$day_month, format = "%Y-%m-%d")
  df_fix_med <- na.omit(df_fix_med)
  
  return(df_fix_med)
}

poc_night <- function(df){
  library(modeest)
  
  x_A <- with(df, seq(Low_A, High_A, by = 0.25))
  x_B <- with(df, seq(Low_B, High_B, by = 0.25))
  x_D <- with(df, seq(Low_D, High_D, by = 0.25))
  x_E <- with(df, seq(Low_E, High_E, by = 0.25))
  x_F <- with(df, seq(Low_F, High_F, by = 0.25))
  x_G <- with(df, seq(Low_G, High_G, by = 0.25))
  x_H <- with(df, seq(Low_H, High_H, by = 0.25))
  x_I <- with(df, seq(Low_I, High_I, by = 0.25))
  x_J <- with(df, seq(Low_J, High_J, by = 0.25))
  x_K <- with(df, seq(Low_K, High_K, by = 0.25))
  x_L <- with(df, seq(Low_L, High_L, by = 0.25))
  x_M <- with(df, seq(Low_M, High_M, by = 0.25))
  x_N <- with(df, seq(Low_N, High_N, by = 0.25))
  x_O <- with(df, seq(Low_O, High_O, by = 0.25))
  x_P <- with(df, seq(Low_P, High_P, by = 0.25))
  x_Q <- with(df, seq(Low_Q, High_Q, by = 0.25))
  x_R <- with(df, seq(Low_R, High_R, by = 0.25))
  x_S <- with(df, seq(Low_S, High_S, by = 0.25))
  x_T <- with(df, seq(Low_T, High_T, by = 0.25))
  x_U <- with(df, seq(Low_U, High_U, by = 0.25))
  x_V <- with(df, seq(Low_V, High_V, by = 0.25))
  x_W <- with(df, seq(Low_W, High_W, by = 0.25))
  x_X <- with(df, seq(Low_X, High_X, by = 0.25))
  x_a <- with(df, seq(Low_a, High_a, by = 0.25))
  x_b <- with(df, seq(Low_b, High_b, by = 0.25))
  x_c <- with(df, seq(Low_c, High_c, by = 0.25))
  x_d <- with(df, seq(Low_d, High_d, by = 0.25))
  x_e <- with(df, seq(Low_e, High_e, by = 0.25))
  x_f <- with(df, seq(Low_f, High_f, by = 0.25))
  x_g <- with(df, seq(Low_g, High_g, by = 0.25))
  x_h <- with(df, seq(Low_h, High_h, by = 0.25))
  x_i <- with(df, seq(Low_i, High_i, by = 0.25))
  x_j <- with(df, seq(Low_j, High_j, by = 0.25))
  x_k <- with(df, seq(Low_k, High_k, by = 0.25))
  
  
  n <- max(length(x_A), length(x_B), length(x_D), length(x_E), length(x_F), length(x_G),
           length(x_H), length(x_I), length(x_J), length(x_K), length(x_L), length(x_M), length(x_N), length(x_O),
           length(x_P), length(x_Q), length(x_R), length(x_S), length(x_T), length(x_U), length(x_V), length(x_W),
           length(x_X), length(x_a), length(x_b), length(x_c), length(x_d), length(x_e), length(x_f), length(x_g),
           length(x_h), length(x_i), length(x_j), length(x_k))
  
  length(x_A) <- n
  length(x_B) <- n
  length(x_D) <- n
  length(x_E) <- n
  length(x_F) <- n
  length(x_G) <- n
  length(x_H) <- n
  length(x_I) <- n
  length(x_J) <- n
  length(x_K) <- n
  length(x_L) <- n
  length(x_M) <- n
  length(x_N) <- n
  length(x_O) <- n
  length(x_P) <- n
  length(x_Q) <- n
  length(x_R) <- n
  length(x_S) <- n
  length(x_T) <- n
  length(x_U) <- n
  length(x_V) <- n
  length(x_W) <- n
  length(x_X) <- n
  length(x_a) <- n
  length(x_b) <- n
  length(x_c) <- n
  length(x_d) <- n
  length(x_e) <- n
  length(x_f) <- n
  length(x_g) <- n
  length(x_h) <- n
  length(x_i) <- n
  length(x_j) <- n
  length(x_k) <- n
  
  pf <- cbind(x_A, x_B, x_D, x_E, x_F, x_G, x_H, x_I, x_J, x_K, x_L, x_M, x_N, x_O, x_P, x_Q, x_R,
              x_S, x_T, x_U, x_V, x_W, x_X, x_a, x_b, x_c, x_d, x_e, x_f, x_g, x_h, x_i, x_j, x_k)
  
  xfd <- data.frame(pf)
  long <- xfd %>% gather(x, value, x_A:x_k)
  long <- na.omit(long)
  xv = mfv(long$value)
  x = xv[which(abs(xv-df$POC_median)==min(abs(xv-df$POC_median)))]
  return(x[1])
}

vah_night <- function(df){
  x_A <- with(df, seq(Low_A, High_A, by = 0.25))
  x_B <- with(df, seq(Low_B, High_B, by = 0.25))
  x_D <- with(df, seq(Low_D, High_D, by = 0.25))
  x_E <- with(df, seq(Low_E, High_E, by = 0.25))
  x_F <- with(df, seq(Low_F, High_F, by = 0.25))
  x_G <- with(df, seq(Low_G, High_G, by = 0.25))
  x_H <- with(df, seq(Low_H, High_H, by = 0.25))
  x_I <- with(df, seq(Low_I, High_I, by = 0.25))
  x_J <- with(df, seq(Low_J, High_J, by = 0.25))
  x_K <- with(df, seq(Low_K, High_K, by = 0.25))
  x_L <- with(df, seq(Low_L, High_L, by = 0.25))
  x_M <- with(df, seq(Low_M, High_M, by = 0.25))
  x_N <- with(df, seq(Low_N, High_N, by = 0.25))
  x_O <- with(df, seq(Low_O, High_O, by = 0.25))
  x_P <- with(df, seq(Low_P, High_P, by = 0.25))
  x_Q <- with(df, seq(Low_Q, High_Q, by = 0.25))
  x_R <- with(df, seq(Low_R, High_R, by = 0.25))
  x_S <- with(df, seq(Low_S, High_S, by = 0.25))
  x_T <- with(df, seq(Low_T, High_T, by = 0.25))
  x_U <- with(df, seq(Low_U, High_U, by = 0.25))
  x_V <- with(df, seq(Low_V, High_V, by = 0.25))
  x_W <- with(df, seq(Low_W, High_W, by = 0.25))
  x_X <- with(df, seq(Low_X, High_X, by = 0.25))
  x_a <- with(df, seq(Low_a, High_a, by = 0.25))
  x_b <- with(df, seq(Low_b, High_b, by = 0.25))
  x_c <- with(df, seq(Low_c, High_c, by = 0.25))
  x_d <- with(df, seq(Low_d, High_d, by = 0.25))
  x_e <- with(df, seq(Low_e, High_e, by = 0.25))
  x_f <- with(df, seq(Low_f, High_f, by = 0.25))
  x_g <- with(df, seq(Low_g, High_g, by = 0.25))
  x_h <- with(df, seq(Low_h, High_h, by = 0.25))
  x_i <- with(df, seq(Low_i, High_i, by = 0.25))
  x_j <- with(df, seq(Low_j, High_j, by = 0.25))
  x_k <- with(df, seq(Low_k, High_k, by = 0.25))
  
  
  n <- max(length(x_A), length(x_B), length(x_D), length(x_E), length(x_F), length(x_G),
           length(x_H), length(x_I), length(x_J), length(x_K), length(x_L), length(x_M), length(x_N), length(x_O),
           length(x_P), length(x_Q), length(x_R), length(x_S), length(x_T), length(x_U), length(x_V), length(x_W),
           length(x_X), length(x_a), length(x_b), length(x_c), length(x_d), length(x_e), length(x_f), length(x_g),
           length(x_h), length(x_i), length(x_j), length(x_k))
  
  length(x_A) <- n
  length(x_B) <- n
  length(x_D) <- n
  length(x_E) <- n
  length(x_F) <- n
  length(x_G) <- n
  length(x_H) <- n
  length(x_I) <- n
  length(x_J) <- n
  length(x_K) <- n
  length(x_L) <- n
  length(x_M) <- n
  length(x_N) <- n
  length(x_O) <- n
  length(x_P) <- n
  length(x_Q) <- n
  length(x_R) <- n
  length(x_S) <- n
  length(x_T) <- n
  length(x_U) <- n
  length(x_V) <- n
  length(x_W) <- n
  length(x_X) <- n
  length(x_a) <- n
  length(x_b) <- n
  length(x_c) <- n
  length(x_d) <- n
  length(x_e) <- n
  length(x_f) <- n
  length(x_g) <- n
  length(x_h) <- n
  length(x_i) <- n
  length(x_j) <- n
  length(x_k) <- n
  
  pf <- cbind(x_A, x_B, x_D, x_E, x_F, x_G, x_H, x_I, x_J, x_K, x_L, x_M, x_N, x_O, x_P, x_Q, x_R,
              x_S, x_T, x_U, x_V, x_W, x_X, x_a, x_b, x_c, x_d, x_e, x_f, x_g, x_h, x_i, x_j, x_k)
  
  
  xfd <- data.frame(pf)
  long <- xfd %>% gather(x, value, x_A:x_k)
  long <- na.omit(long)
  
  long_2 <- long %>% group_by(value) %>% summarise(cn = n())
  long_2 <- long_2[order(long_2$value),]
  long_2$cum_sum <- cumsum(long_2$cn)
  tpo_va = round(sum(long_2$cn)*0.7)
  tpo = sum(long_2$cn)
  pos_low = (tpo - tpo_va)/2
  pos_high = pos_low + tpo_va
  vah = long_2$value[which(abs(long_2$cum_sum-pos_high)==min(abs(long_2$cum_sum-pos_high)))][1]
  return(vah)
}

val_night <- function(df){
  x_A <- with(df, seq(Low_A, High_A, by = 0.25))
  x_B <- with(df, seq(Low_B, High_B, by = 0.25))
  x_D <- with(df, seq(Low_D, High_D, by = 0.25))
  x_E <- with(df, seq(Low_E, High_E, by = 0.25))
  x_F <- with(df, seq(Low_F, High_F, by = 0.25))
  x_G <- with(df, seq(Low_G, High_G, by = 0.25))
  x_H <- with(df, seq(Low_H, High_H, by = 0.25))
  x_I <- with(df, seq(Low_I, High_I, by = 0.25))
  x_J <- with(df, seq(Low_J, High_J, by = 0.25))
  x_K <- with(df, seq(Low_K, High_K, by = 0.25))
  x_L <- with(df, seq(Low_L, High_L, by = 0.25))
  x_M <- with(df, seq(Low_M, High_M, by = 0.25))
  x_N <- with(df, seq(Low_N, High_N, by = 0.25))
  x_O <- with(df, seq(Low_O, High_O, by = 0.25))
  x_P <- with(df, seq(Low_P, High_P, by = 0.25))
  x_Q <- with(df, seq(Low_Q, High_Q, by = 0.25))
  x_R <- with(df, seq(Low_R, High_R, by = 0.25))
  x_S <- with(df, seq(Low_S, High_S, by = 0.25))
  x_T <- with(df, seq(Low_T, High_T, by = 0.25))
  x_U <- with(df, seq(Low_U, High_U, by = 0.25))
  x_V <- with(df, seq(Low_V, High_V, by = 0.25))
  x_W <- with(df, seq(Low_W, High_W, by = 0.25))
  x_X <- with(df, seq(Low_X, High_X, by = 0.25))
  x_a <- with(df, seq(Low_a, High_a, by = 0.25))
  x_b <- with(df, seq(Low_b, High_b, by = 0.25))
  x_c <- with(df, seq(Low_c, High_c, by = 0.25))
  x_d <- with(df, seq(Low_d, High_d, by = 0.25))
  x_e <- with(df, seq(Low_e, High_e, by = 0.25))
  x_f <- with(df, seq(Low_f, High_f, by = 0.25))
  x_g <- with(df, seq(Low_g, High_g, by = 0.25))
  x_h <- with(df, seq(Low_h, High_h, by = 0.25))
  x_i <- with(df, seq(Low_i, High_i, by = 0.25))
  x_j <- with(df, seq(Low_j, High_j, by = 0.25))
  x_k <- with(df, seq(Low_k, High_k, by = 0.25))
  
  
  n <- max(length(x_A), length(x_B), length(x_D), length(x_E), length(x_F), length(x_G),
           length(x_H), length(x_I), length(x_J), length(x_K), length(x_L), length(x_M), length(x_N), length(x_O),
           length(x_P), length(x_Q), length(x_R), length(x_S), length(x_T), length(x_U), length(x_V), length(x_W),
           length(x_X), length(x_a), length(x_b), length(x_c), length(x_d), length(x_e), length(x_f), length(x_g),
           length(x_h), length(x_i), length(x_j), length(x_k))
  
  length(x_A) <- n
  length(x_B) <- n
  length(x_D) <- n
  length(x_E) <- n
  length(x_F) <- n
  length(x_G) <- n
  length(x_H) <- n
  length(x_I) <- n
  length(x_J) <- n
  length(x_K) <- n
  length(x_L) <- n
  length(x_M) <- n
  length(x_N) <- n
  length(x_O) <- n
  length(x_P) <- n
  length(x_Q) <- n
  length(x_R) <- n
  length(x_S) <- n
  length(x_T) <- n
  length(x_U) <- n
  length(x_V) <- n
  length(x_W) <- n
  length(x_X) <- n
  length(x_a) <- n
  length(x_b) <- n
  length(x_c) <- n
  length(x_d) <- n
  length(x_e) <- n
  length(x_f) <- n
  length(x_g) <- n
  length(x_h) <- n
  length(x_i) <- n
  length(x_j) <- n
  length(x_k) <- n
  
  pf <- cbind(x_A, x_B, x_D, x_E, x_F, x_G, x_H, x_I, x_J, x_K, x_L, x_M, x_N, x_O, x_P, x_Q, x_R,
              x_S, x_T, x_U, x_V, x_W, x_X, x_a, x_b, x_c, x_d, x_e, x_f, x_g, x_h, x_i, x_j, x_k)
  
  
  xfd <- data.frame(pf)
  long <- xfd %>% gather(x, value, x_A:x_k)
  long <- na.omit(long)
  
  long_2 <- long %>% group_by(value) %>% summarise(cn = n())
  long_2 <- long_2[order(long_2$value),]
  long_2$cum_sum <- cumsum(long_2$cn)
  tpo_va = round(sum(long_2$cn)*0.7)
  tpo = sum(long_2$cn)
  pos_low = (tpo - tpo_va)/2
  pos_high = pos_low + tpo_va
  val = long_2$value[which(abs(long_2$cum_sum-pos_low)==min(abs(long_2$cum_sum-pos_low)))][1]
  return(val)
}

profile_point_night <- function(data){
  
  df <- tpo30_night(data)
  df$POC <- sapply(split(df, seq(nrow(df))), poc_night) 
  df$VAH <- sapply(split(df, seq(nrow(df))), vah_night) 
  df$VAL <- sapply(split(df, seq(nrow(df))), val_night) 
  df$IBH <- with(df, ifelse(High_A > High_B, High_A, High_B))
  df$IBL <- with(df, ifelse(Low_A < Low_B, Low_A, Low_B))
  df <- df %>% select(day_month, High, Low, Open, Close, POC_median, POC, VAH, VAL, IBH, IBL)
  df$day_month <- as.Date(df$day_month)
  return(df)
}


## Day MP

tpo30_day <- function(dataframe){
  # Format date
  dataframe$day_month <- as.Date(dataframe$Date, format = "%Y-%m-%d")
  dataframe$hours <- format(strptime(dataframe$Date, "%Y-%m-%d %H:%M:%S"), "%H:%M")
  dataframe$weekday <- weekdays(dataframe$day_month)
  

  dataframe <- subset(dataframe, hours >= "09:30" & hours <= "16:14")
  dataframe <- dataframe[!dataframe$day_month == "2006-04-13",]
  dataframe <- dataframe %>% mutate(let =  case_when(hours >= "09:30" & hours < "10:00"~"A",
                                       hours >= "10:00" & hours < "10:30"~"B",
                                       hours >= "10:30" & hours < "11:00"~"C",
                                       hours >= "11:00" & hours < "11:30"~"D",
                                       hours >= "11:30" & hours < "12:00"~"E",
                                       hours >= "12:00" & hours < "12:30"~"F",
                                       hours >= "12:30" & hours < "13:00"~"G",
                                       hours >= "13:00" & hours < "13:30"~"H",
                                       hours >= "13:30" & hours < "14:00"~"I",
                                       hours >= "14:00" & hours < "14:30"~"J",
                                       hours >= "14:30" & hours < "15:00"~"K",
                                       hours >= "15:00" & hours < "15:30"~"L",
                                       hours >= "15:30" & hours < "16:00"~"M",
                                       hours >= "16:00" & hours < "16:15"~"N"))
  
  df_open_2 <- dataframe %>% group_by(day_month, let) %>% summarise(day_month = first(day_month),
                                                             High = max(High),
                                                             Low = min(Low)) 
  
  df_fix_day <- df_open_2 %>%
    arrange(day_month, let) %>%
    pivot_wider(names_from = let, values_from = c(High, Low)) %>%
    select(day_month, order(sub('.*_', '', names(.))))
  

  df_median_day <- dataframe %>% group_by(day_month) %>% summarise(day_month = first(day_month),
                                                            High = max(High),
                                                            Low = min(Low),
                                                            POC_median = (High+Low)/2,
                                                            Open = first(Open),
                                                            Close = last(Close))

  
  
  
  df_fix_med_day <- merge(df_fix_day, df_median_day, by = "day_month")
  
  
  
  return(df_fix_med_day)
}

poc_day <- function(df){
  x_A <- with(df, seq(Low_A, High_A, by = 0.25))
  x_B <- with(df, seq(Low_B, High_B, by = 0.25))
  x_C <- with(df, seq(Low_C, High_C, by = 0.25))
  x_D <- with(df, seq(Low_D, High_D, by = 0.25))
  x_E <- with(df, seq(Low_E, High_E, by = 0.25))
  x_F <- with(df, seq(Low_F, High_F, by = 0.25))
  x_G <- with(df, seq(Low_G, High_G, by = 0.25))
  x_H <- with(df, seq(Low_H, High_H, by = 0.25))
  x_I <- with(df, seq(Low_I, High_I, by = 0.25))
  x_J <- with(df, seq(Low_J, High_J, by = 0.25))
  x_K <- with(df, seq(Low_K, High_K, by = 0.25))
  x_L <- with(df, seq(Low_L, High_L, by = 0.25))
  x_M <- with(df, seq(Low_M, High_M, by = 0.25))
  x_N <- with(df, seq(Low_N, High_N, by = 0.25))
  
  n <- max(length(x_A), length(x_B), length(x_C), length(x_D), length(x_E), length(x_F), length(x_G),
           length(x_H), length(x_I), length(x_J), length(x_K), length(x_L), length(x_M), length(x_N))
  
  length(x_A) <- n
  length(x_B) <- n
  length(x_C) <- n
  length(x_D) <- n
  length(x_E) <- n
  length(x_F) <- n
  length(x_G) <- n
  length(x_H) <- n
  length(x_I) <- n
  length(x_J) <- n
  length(x_K) <- n
  length(x_L) <- n
  length(x_M) <- n
  length(x_N) <- n
  
  pf <- cbind(x_A, x_B,x_C, x_D, x_E, x_F, x_G, x_H, x_I, x_J, x_K, x_L, x_M, x_N)
  
  xfd <- data.frame(pf)
  long <- xfd %>% gather(x, value, x_A:x_N)
  long <- na.omit(long)
  xv = mfv(long$value)
  x = xv[which(abs(xv-df$POC_median)==min(abs(xv-df$POC_median)))]
  return(x[1])
}

vah_day <- function(df){
  x_A <- with(df, seq(Low_A, High_A, by = 0.25))
  x_B <- with(df, seq(Low_B, High_B, by = 0.25))
  x_C <- with(df, seq(Low_C, High_C, by = 0.25))
  x_D <- with(df, seq(Low_D, High_D, by = 0.25))
  x_E <- with(df, seq(Low_E, High_E, by = 0.25))
  x_F <- with(df, seq(Low_F, High_F, by = 0.25))
  x_G <- with(df, seq(Low_G, High_G, by = 0.25))
  x_H <- with(df, seq(Low_H, High_H, by = 0.25))
  x_I <- with(df, seq(Low_I, High_I, by = 0.25))
  x_J <- with(df, seq(Low_J, High_J, by = 0.25))
  x_K <- with(df, seq(Low_K, High_K, by = 0.25))
  x_L <- with(df, seq(Low_L, High_L, by = 0.25))
  x_M <- with(df, seq(Low_M, High_M, by = 0.25))
  x_N <- with(df, seq(Low_N, High_N, by = 0.25))
  
  n <- max(length(x_A), length(x_B), length(x_C), length(x_D), length(x_E), length(x_F), length(x_G),
           length(x_H), length(x_I), length(x_J), length(x_K), length(x_L), length(x_M), length(x_N))
  
  length(x_A) <- n
  length(x_B) <- n
  length(x_C) <- n
  length(x_D) <- n
  length(x_E) <- n
  length(x_F) <- n
  length(x_G) <- n
  length(x_H) <- n
  length(x_I) <- n
  length(x_J) <- n
  length(x_K) <- n
  length(x_L) <- n
  length(x_M) <- n
  length(x_N) <- n
  
  pf <- cbind(x_A, x_B,x_C, x_D, x_E, x_F, x_G, x_H, x_I, x_J, x_K, x_L, x_M, x_N)
  
  xfd <- data.frame(pf)
  long <- xfd %>% gather(x, value, x_A:x_N)
  long <- na.omit(long)
  
  long_2 <- long %>% group_by(value) %>% summarise(cn = n())
  long_2 <- long_2[order(long_2$value),]
  long_2$cum_sum <- cumsum(long_2$cn)
  tpo_va = round(sum(long_2$cn)*0.7)
  tpo = sum(long_2$cn)
  pos_Low = (tpo - tpo_va)/2
  pos_High = pos_Low + tpo_va
  vah = long_2$value[which(abs(long_2$cum_sum-pos_High)==min(abs(long_2$cum_sum-pos_High)))][1]
  return(vah)
}

val_day <- function(df){
  x_A <- with(df, seq(Low_A, High_A, by = 0.25))
  x_B <- with(df, seq(Low_B, High_B, by = 0.25))
  x_C <- with(df, seq(Low_C, High_C, by = 0.25))
  x_D <- with(df, seq(Low_D, High_D, by = 0.25))
  x_E <- with(df, seq(Low_E, High_E, by = 0.25))
  x_F <- with(df, seq(Low_F, High_F, by = 0.25))
  x_G <- with(df, seq(Low_G, High_G, by = 0.25))
  x_H <- with(df, seq(Low_H, High_H, by = 0.25))
  x_I <- with(df, seq(Low_I, High_I, by = 0.25))
  x_J <- with(df, seq(Low_J, High_J, by = 0.25))
  x_K <- with(df, seq(Low_K, High_K, by = 0.25))
  x_L <- with(df, seq(Low_L, High_L, by = 0.25))
  x_M <- with(df, seq(Low_M, High_M, by = 0.25))
  x_N <- with(df, seq(Low_N, High_N, by = 0.25))
  
  n <- max(length(x_A), length(x_B), length(x_C), length(x_D), length(x_E), length(x_F), length(x_G),
           length(x_H), length(x_I), length(x_J), length(x_K), length(x_L), length(x_M), length(x_N))
  
  length(x_A) <- n
  length(x_B) <- n
  length(x_C) <- n
  length(x_D) <- n
  length(x_E) <- n
  length(x_F) <- n
  length(x_G) <- n
  length(x_H) <- n
  length(x_I) <- n
  length(x_J) <- n
  length(x_K) <- n
  length(x_L) <- n
  length(x_M) <- n
  length(x_N) <- n
  
  pf <- cbind(x_A, x_B,x_C, x_D, x_E, x_F, x_G, x_H, x_I, x_J, x_K, x_L, x_M, x_N)
  
  xfd <- data.frame(pf)
  long <- xfd %>% gather(x, value, x_A:x_N)
  long <- na.omit(long)
  
  long_2 <- long %>% group_by(value) %>% summarise(cn = n())
  long_2 <- long_2[order(long_2$value),]
  long_2$cum_sum <- cumsum(long_2$cn)
  tpo_va = round(sum(long_2$cn)*0.7)
  tpo = sum(long_2$cn)
  pos_Low = (tpo - tpo_va)/2
  pos_High = pos_Low + tpo_va
  val = long_2$value[which(abs(long_2$cum_sum-pos_Low)==min(abs(long_2$cum_sum-pos_Low)))][1]
  return(val)
}

profile_point_day <- function(data){
  df <- tpo30_day(data)
  df$day_month <- as.Date(df$day_month, format = "%Y-%m-%d")
  df <- na.omit(df)
  
  df$POC <- sapply(split(df, seq(nrow(df))), poc_day) 
  df$VAH <- sapply(split(df, seq(nrow(df))), vah_day) 
  df$VAL <- sapply(split(df, seq(nrow(df))), val_day) 
  
  
  df$IBH <- with(df, ifelse(High_A > High_B, High_A, High_B))
  df$IBL <- with(df, ifelse(Low_A < Low_B, Low_A, Low_B))
  return(df)
  
  
}


touch <- function(df, target){
  df$target <- with(df, ifelse(dplyr::lag(target) >= Low_A & dplyr::lag(target) <= High_A, "A", 
                               ifelse(dplyr::lag(target) >= Low_B & dplyr::lag(target) <= High_B, "B",
                                      ifelse(dplyr::lag(target) >= Low_C & dplyr::lag(target) <= High_C, "C",
                                             ifelse(dplyr::lag(target) >= Low_D & dplyr::lag(target) <= High_D, "D",
                                                    ifelse(dplyr::lag(target) >= Low_E & dplyr::lag(target) <= High_E, "E",
                                                           ifelse(dplyr::lag(target) >= Low_F & dplyr::lag(target) <= High_F, "F",
                                                                  ifelse(dplyr::lag(target) >= Low_G & dplyr::lag(target) <= High_G, "G",
                                                                         ifelse(dplyr::lag(target) >= Low_H & dplyr::lag(target) <= High_H, "H",
                                                                                ifelse(dplyr::lag(target) >= Low_I & dplyr::lag(target) <= High_I, "I",
                                                                                       ifelse(dplyr::lag(target) >= Low_J & dplyr::lag(target) <= High_J, "J",
                                                                                              ifelse(dplyr::lag(target) >= Low_K & dplyr::lag(target) <= High_K, "K",
                                                                                                     ifelse(dplyr::lag(target) >= Low_L & dplyr::lag(target) <= High_L, "L",
                                                                                                            ifelse(dplyr::lag(target) >= Low_M & dplyr::lag(target) <= High_M, "M",
                                                                                                                   ifelse(dplyr::lag(target) >= Low_N & dplyr::lag(target) <= High_N, "N",
                                                                                                                          0)))))))))))))))
  
  
  
}

previous_range_day_night <- function(day, night, touch_dummy){
  day <- day %>% 
    mutate(range = case_when(Open <= dplyr::lag(High) & Open >= dplyr::lag(Low)~"Within",
                             Open <= dplyr::lag(Low)~"Below",
                             Open >= dplyr::lag(High)~"Above"))
  
  day$pClose <- touch(day, day$Close)
  
  day$pHOD <- touch(day, day$High)
  
  day$pIBH <- touch(day, day$IBH)
  
  day$pIBL <- touch(day, day$IBL)
  
  day$pLOD <- touch(day, day$Low)
  
  day$pOpen <- touch(day, day$Open)
  
  day$pVAH <- touch(day, day$VAH)
  
  day$pVAL <- touch(day, day$VAL)
  
  day$pPOC <- touch(day, day$POC)
  
  day$pPOC_median <- touch(day, day$POC_median)
  
  
  
  #### Join day and night data
  day <- day %>% inner_join(night, by = "day_month", suffix = c("", ".night"))
  
  day$pClose.night <- touch(day, day$Close.night)
  
  day$pHOD.night <- touch(day, day$High.night)
  
  day$pIBH.night <- touch(day, day$IBH.night)
  
  day$pIBL.night <- touch(day, day$IBL.night)
  
  day$pLOD.night <- touch(day, day$Low.night)
  
  day$pOpen.night <- touch(day, day$Open.night)
  
  day$pVAH.night <- touch(day, day$VAH.night)
  
  day$pVAL.night <- touch(day, day$VAL.night)
  
  day$pPOC.night <- touch(day, day$POC.night)
  
  day$pPOC_median.night <- touch(day, day$POC_median.night)
  
  
  
  return(day)
}

timeframe_up <- function(timeframe = 5){
  setwd(sprintf("C:/Users/Pastor/Desktop/stock_market/futures/futures_unadjusted_%s", timeframe))
  df <- read.table(sprintf("NQ_continuous_UNadjusted_%smin.txt", timeframe), sep = ",")
  setwd(sprintf("C:/Users/Pastor/Desktop/stock_market/futures/futures_unadjusted_%s_update", timeframe))
  df_2 <- read.table(sprintf("NQ_%s-min.txt", timeframe), sep = ",")
  df <- rbind(df, df_2)
  df <- df[!duplicated(df$V1),]
  
  df$day_month <- as.Date(df$V1, format = "%Y-%m-%d")
  df$hours <- format(strptime(df$V1, "%Y-%m-%d %H:%M:%S"), "%H:%M")
  df$V1 <- as.POSIXct(df$V1, format = "%Y-%m-%d %H:%M:%S")
  
  df <- plyr::rename(df, c("V1"= "Date","V2"= "Open", "V3"="High", "V4" = "Low",
                           "V5" = "Close", "V6" = "Volume"))
  
  df <- subset(df, hours >= "09:30" & hours <= "16:14")
  df <- df[!df$day_month == "2006-04-13",]
  df <- df %>% mutate(let =  case_when(hours >= "09:30" & hours < "10:00"~"A",
                                       hours >= "10:00" & hours < "10:30"~"B",
                                       hours >= "10:30" & hours < "11:00"~"C",
                                       hours >= "11:00" & hours < "11:30"~"D",
                                       hours >= "11:30" & hours < "12:00"~"E",
                                       hours >= "12:00" & hours < "12:30"~"F",
                                       hours >= "12:30" & hours < "13:00"~"G",
                                       hours >= "13:00" & hours < "13:30"~"H",
                                       hours >= "13:30" & hours < "14:00"~"I",
                                       hours >= "14:00" & hours < "14:30"~"J",
                                       hours >= "14:30" & hours < "15:00"~"K",
                                       hours >= "15:00" & hours < "15:30"~"L",
                                       hours >= "15:30" & hours < "16:00"~"M",
                                       hours >= "16:00" & hours < "16:15"~"N"))
  
  df_TPO <- df %>% group_by(day_month, let) %>% summarise(day_month = first(day_month),
                                                          Open = first(Open),
                                                          Low = min(Low), 
                                                          High = max(High),
                                                          Close = last(Close))
  
  df_TPO <- df_TPO %>% group_by(day_month) %>% 
    mutate(timeFrameUp = if_else(Close > dplyr::lag(High), 1,0)) %>% 
    mutate(timeFramedown = if_else(Close < dplyr::lag(Low), 1, 0)) 
  
  return(df_TPO)
}

