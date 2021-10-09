instrument = "NQ"

path_file_raw_data<- file.path(dirname(sprintf("C:/Users/Pastor/Dropbox/Pastor/data/MarketProfile_data/%s/..", instrument)))
setwd(sprintf("C:/Users/Pastor/Dropbox/Pastor/data/MarketProfile_data/%s", instrument))

df <- read.csv(sprintf("%s_raw_data.csv", instrument))
df$day_month <- as.Date(df$day_month)

library(tidyverse)

dist <- function(df){
  df2 <- df
  df$day_month <- NULL
  
  df[2:ncol(df)] <- round((df[2:ncol(df)]-df[,1])/df[,1]*last(df[,1]),2)
  
  df$day_month <- df2$day_month
  return(df)
}

dummy <- function(df){
  
  df_dummy <- df
  df_dummy$day_month <- NULL
  
  df_dummy <- as.data.frame(ifelse(df_dummy < 0, 1, 0))
  df_dummy$day_month <- df$day_month
  df_dummy[,1] <- NULL
  return(df_dummy)
  
}
############################## Low ###########################

Lows <- grep("Low_", names(df))
Lows <- as.data.frame(df[Lows])
Lows <- Lows %>%  select(Low_A:Low_N)
Lows$day_month <- df$day_month

Low <- dist(Lows)
Low_dum <- dummy(Low)

Lows$day_month <- as.Date(Lows$day_month)
Lows_B <- Lows %>%  select(Low_B:Low_N, day_month)

Low_B <- dist(Lows_B)
Low_dum_B <- dummy(Low_B)
colnames(Low_dum_B) <- paste(colnames(Low_dum_B), "B", sep="_")

####### Low C #####
Lows_C <- Lows %>%  select(Low_C:Low_N)

Low_C <- dist(Lows_C)
Low_dum_C <- dummy(Low_C)
colnames(Low_dum_C) <- paste(colnames(Low_dum_C), "C", sep="_")

###### Low D ####
Lows_D <- Lows %>%  select(Low_D:Low_N)

Low_D <- dist(Lows_D)
Low_dum_D <- dummy(Low_D)
colnames(Low_dum_D) <- paste(colnames(Low_dum_D), "D", sep="_")

#### Low E ####
Lows_E <- Lows %>%  select(Low_E:Low_N)

Low_E <- dist(Lows_E)
Low_dum_E <- dummy(Low_E)
colnames(Low_dum_E) <- paste(colnames(Low_dum_E), "E", sep="_")

#### Low F ####
Lows_F <- Lows %>%  select(Low_F:Low_N)

Low_F <- dist(Lows_F)
Low_dum_F <- dummy(Low_F)
colnames(Low_dum_F) <- paste(colnames(Low_dum_F), "F", sep="_")


### Low G ####
Lows_G <- Lows %>%  select(Low_G:Low_N)

Low_G <- dist(Lows_G)
Low_dum_G <- dummy(Low_G)
colnames(Low_dum_G) <- paste(colnames(Low_dum_G), "G", sep="_")


### Low H ####
Lows_H <- Lows %>%  select(Low_H:Low_N)

Low_H <- dist(Lows_H)
Low_dum_H <- dummy(Low_H)
colnames(Low_dum_H) <- paste(colnames(Low_dum_H), "H", sep="_")

#### Low I ####
Lows_I <- Lows %>%  select(Low_I:Low_N)

Low_I <- dist(Lows_I)
Low_dum_I <- dummy(Low_I)
colnames(Low_dum_I) <- paste(colnames(Low_dum_I), "I", sep="_")


### Low J #####
Lows_J <- Lows %>%  select(Low_J:Low_N)

Low_J <- dist(Lows_J)
Low_dum_J <- dummy(Low_J)
colnames(Low_dum_J) <- paste(colnames(Low_dum_J), "J", sep="_")

#### Low K ####
Lows_K <- Lows %>%  select(Low_K:Low_N)

Low_K <- dist(Lows_K)
Low_dum_K <- dummy(Low_K)
colnames(Low_dum_K) <- paste(colnames(Low_dum_K), "K", sep="_")


#### Low L ####
Lows_L <- Lows %>%  select(Low_L:Low_N)

Low_L <- dist(Lows_L)
Low_dum_L <- dummy(Low_L)
colnames(Low_dum_L) <- paste(colnames(Low_dum_L), "L", sep="_")


#### Low M ####
Lows_M <- Lows %>%  select(Low_M:Low_N)

Low_M <- dist(Lows_M)
Low_dum_M <- dummy(Low_M)
colnames(Low_dum_M) <- paste(colnames(Low_dum_M), "M", sep="_")

######## Merge Lows #############
## for probability to have a Low Lower than 
Low_ <- cbind(Low_dum_B, Low_dum_C, Low_dum_D, Low_dum_E, Low_dum_F,
              Low_dum_G, Low_dum_H, Low_dum_I, Low_dum_J, Low_dum_K,
              Low_dum_L, Low_dum_M)

Low_ <- Low_ %>% select(day_month_B, Low_C_B, Low_D_C, Low_E_D, Low_F_E, Low_G_F, Low_H_G, Low_I_H, Low_J_I,
                        Low_K_J, Low_L_K, Low_M_L, Low_N_M)

low_ <- plyr::rename(Low_, c("day_month_B"= "day_month"))

write.csv(low_, "low_dummy.csv", row.names = FALSE)
##### data for the difference between each period #######
## lower lows
Low_data_day <- Low
Low_data_day$Low_C <- Low_B[,2]
Low_data_day$Low_D <- Low_C[,2]
Low_data_day$Low_E <- Low_D[,2]
Low_data_day$Low_F <- Low_E[,2]
Low_data_day$Low_G <- Low_F[,2]
Low_data_day$Low_H <- Low_G[,2]
Low_data_day$Low_I <- Low_H[,2]
Low_data_day$Low_J <- Low_I[,2]
Low_data_day$Low_K <- Low_J[,2]
Low_data_day$Low_L <- Low_K[,2]
Low_data_day$Low_M <- Low_L[,2]
Low_data_day$Low_N <- Low_M[,2]

write.csv(Low_data_day, "low_data_day.csv", row.names = FALSE)



