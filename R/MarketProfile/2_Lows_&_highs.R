instrument = "NQ"

path_file_raw_data<- file.path(dirname(sprintf("C:/Users/Pastor/Dropbox/Pastor/data/MarketProfile_data/%s/..", instrument)))
setwd(sprintf("C:/Users/Pastor/Dropbox/Pastor/data/MarketProfile_data/%s", instrument))


df <- read.csv(file.path(path_file_raw_data, sprintf("%s_raw_data.csv", instrument)))

library(tidyverse)

# get the difference between the next period and the previous period
dist <- function(df){
  df2 <- df
  df$day_month <- NULL
  
  df[2:ncol(df)] <- round((df[2:ncol(df)]-df[,1])/df[,1]*last(df[,1]),2)
  
  df$day_month <- df2$day_month
  return(df)
}


# Lower Low are zero and Higher Low are 1

dummy <- function(df){
  
  df_dummy <- df
  df_dummy$day_month <- NULL
  
  df_dummy <- as.data.frame(ifelse(df_dummy > 0, 1, 0))
  df_dummy$day_month <- df$day_month
  df_dummy[,1] <- NULL
  return(df_dummy)
  
}
############################## Low ###########################

Lows <- grep("Low_", names(df))
Lows <- as.data.frame(df[Lows])
Lows$day_month <- df$day_month

Low <- dist(Lows)
Low_dum <- dummy(Low)

################## Lows categories ###################
Lows$IBL <- ifelse(Lows$Low_A < Lows$Low_B, Lows$Low_A, Lows$Low_B)

Lows$IBL_cat <- ifelse(Lows$Low_C < Lows$IBL, "C",
                       ifelse(Lows$Low_D < Lows$IBL, "D", 
                              ifelse(Lows$Low_E < Lows$IBL, "E", 
                                     ifelse(Lows$Low_F < Lows$IBL, "F",
                                            ifelse(Lows$Low_G < Lows$IBL, "G",
                                                   ifelse(Lows$Low_H < Lows$IBL, "H",
                                                          ifelse(Lows$Low_I < Lows$IBL, "I",
                                                                 ifelse(Lows$Low_J < Lows$IBL, "J",
                                                                        ifelse(Lows$Low_K < Lows$IBL, "K",
                                                                               ifelse(Lows$Low_L < Lows$IBL, "L",
                                                                                      ifelse(Lows$Low_M < Lows$IBL, "M",
                                                                                             ifelse(Lows$Low_N < Lows$IBL, "N",
                                                                                                    ifelse(Lows$Low_A == Lows$IBL, "A",
                                                                                                           ifelse(Lows$Low_B == Lows$IBL, "B",NA))))))))))))))
#################
Lows$IBL_BK <- ifelse(Lows$IBL_cat == "A" | Lows$IBL_cat == "B",0,1)
Low$IBL_BK <- Lows$IBL_BK
Low$IBL_cat <- Lows$IBL_cat
Lows_2 <- Lows

write.csv(Low,"low.csv", row.names = FALSE)
write.csv(Low_dum,"low_dum.csv", row.names = FALSE)

###### Low B #####
Lows$day_month <- as.Date(Lows$day_month)
Lows <- Lows %>%  select(Low_B:Low_N, day_month)

Low <- dist(Lows)
Low_dum_B <- dummy(Low)
colnames(Low_dum_B) <- paste(colnames(Low_dum_B), "B", sep="_")

####### Low C #####
Lows <- Lows %>%  select(Low_C:Low_N, day_month)

Low <- dist(Lows)
Low_dum_C <- dummy(Low)
colnames(Low_dum_C) <- paste(colnames(Low_dum_C), "C", sep="_")

###### Low D ####
Lows <- Lows %>%  select(Low_D:Low_N, day_month)

Low <- dist(Lows)
Low_dum_D <- dummy(Low)
colnames(Low_dum_D) <- paste(colnames(Low_dum_D), "D", sep="_")

#### Low E ####
Lows <- Lows %>%  select(Low_E:Low_N, day_month)

Low <- dist(Lows)
Low_dum_E <- dummy(Low)
colnames(Low_dum_E) <- paste(colnames(Low_dum_E), "E", sep="_")

#### Low F ####
Lows <- Lows %>%  select(Low_F:Low_N, day_month)

Low <- dist(Lows)
Low_dum_F <- dummy(Low)
colnames(Low_dum_F) <- paste(colnames(Low_dum_F), "F", sep="_")

### Low G ####
Lows <- Lows %>%  select(Low_G:Low_N, day_month)

Low <- dist(Lows)
Low_dum_G <- dummy(Low)
colnames(Low_dum_G) <- paste(colnames(Low_dum_G), "G", sep="_")

### Low H ####
Lows <- Lows %>%  select(Low_H:Low_N, day_month)

Low <- dist(Lows)
Low_dum_H <- dummy(Low)
colnames(Low_dum_H) <- paste(colnames(Low_dum_H), "H", sep="_")

#### Low I ####
Lows <- Lows %>%  select(Low_I:Low_N, day_month)

Low <- dist(Lows)
Low_dum_I <- dummy(Low)
colnames(Low_dum_I) <- paste(colnames(Low_dum_I), "I", sep="_")

### Low J #####
Lows <- Lows %>%  select(Low_J:Low_N, day_month)

Low <- dist(Lows)
Low_dum_J <- dummy(Low)
colnames(Low_dum_J) <- paste(colnames(Low_dum_J), "J", sep="_")

#### Low K ####
Lows <- Lows %>%  select(Low_K:Low_N, day_month)

Low <- dist(Lows)
Low_dum_K <- dummy(Low)
colnames(Low_dum_K) <- paste(colnames(Low_dum_K), "K", sep="_")

#### Low L ####
Lows <- Lows %>%  select(Low_L:Low_N, day_month)

Low <- dist(Lows)
Low_dum_L <- dummy(Low)
colnames(Low_dum_L) <- paste(colnames(Low_dum_L), "L", sep="_")

#### Low M ####
Lows <- Lows %>%  select(Low_M:Low_N, day_month)

Low <- dist(Lows)
Low_dum_M <- dummy(Low)
colnames(Low_dum_M) <- paste(colnames(Low_dum_M), "M", sep="_")

######## Merge Lows #############
Low_ <- cbind(Low_dum_B, Low_dum_C, Low_dum_D, Low_dum_E, Low_dum_F, Low_dum_G, Low_dum_H, Low_dum_I, Low_dum_J, Low_dum_K, Low_dum_L, Low_dum_M)

######################### High ###############################

Highs <- grep("High_", names(df))
Highs <- as.data.frame(df[Highs])
Highs <- Highs %>% select(High_A:High_N)
Highs$day_month <- df$day_month

High <- dist(Highs)
High_dum <- dummy(High)


Highs$IBH <- ifelse(Highs$High_A > Highs$High_B, Highs$High_A, Highs$High_B)

Highs$IBH_cat <- ifelse(Highs$High_C > Highs$IBH, "C",
                        ifelse(Highs$High_D > Highs$IBH, "D", 
                               ifelse(Highs$High_E > Highs$IBH, "E", 
                                      ifelse(Highs$High_F > Highs$IBH, "F",
                                             ifelse(Highs$High_G > Highs$IBH, "G",
                                                    ifelse(Highs$High_H > Highs$IBH, "H",
                                                           ifelse(Highs$High_I > Highs$IBH, "I",
                                                                  ifelse(Highs$High_J > Highs$IBH, "J",
                                                                         ifelse(Highs$High_K > Highs$IBH, "K",
                                                                                ifelse(Highs$High_L > Highs$IBH, "L",
                                                                                       ifelse(Highs$High_M > Highs$IBH, "M",
                                                                                              ifelse(Highs$High_N > Highs$IBH, "N",
                                                                                                     ifelse(Highs$High_A == Highs$IBH, "A",
                                                                                                            ifelse(Highs$High_B == Highs$IBH, "B",NA))))))))))))))

Highs$IBH_BK <- ifelse(Highs$IBH_cat == "A" | Highs$IBH_cat == "B",0,1)
High$IBH_BK <- Highs$IBH_BK
High$IBH_cat <- Highs$IBH_cat

High$IBL_BK <- Lows_2$IBL_BK
High$IBL_cat <- Lows_2$IBL_cat
High$IB_BK_or <- with(High, ifelse(IBH_BK == 1 | IBL_BK == 1, 1, 0))
High$IB_BK_and <- with(High, ifelse(IBH_BK == 1 & IBL_BK == 1, 1, 0))

write.csv(High, "high.csv", row.names = FALSE)
write.csv(High_dum,"high_dum.csv", row.names = FALSE)

# Get the quantiles

hl_range <- round((Highs[,1:14] - Lows_2[,1:14])/Lows_2[,1:14]*last(Lows_2$Low_A),2)

hl_range <- (Highs[,1:14] - Lows_2[,1:14])/Lows_2[,1:14]*last(Highs$High_A)


quant <- sapply(hl_range, quantile)

range_HL <- function(df, High, i){
  df <- df %>% mutate(High = case_when(High <= quant[2+i]~quant[2+i],
                                       High > quant[2+i] & High <= quant[3+i]~quant[3+i],
                                       High > quant[3+i] & High <= quant[4+i]~quant[4+i],
                                       High > quant[4+i] & High <= quant[5+i]~quant[5+i]))
  return(df$High)
  
}

hl_range$A <- range_HL(hl_range, hl_range$High_A, 0)
hl_range$B <- range_HL(hl_range, hl_range$High_B, 5)
hl_range$C <- range_HL(hl_range, hl_range$High_C, 10)
hl_range$D <- range_HL(hl_range, hl_range$High_D, 15)
hl_range$E <- range_HL(hl_range, hl_range$High_E, 20)
hl_range$F <- range_HL(hl_range, hl_range$High_F, 25)
hl_range$G <- range_HL(hl_range, hl_range$High_G, 30)
hl_range$H <- range_HL(hl_range, hl_range$High_H, 35)
hl_range$I <- range_HL(hl_range, hl_range$High_I, 40)
hl_range$J <- range_HL(hl_range, hl_range$High_J, 45)
hl_range$K <- range_HL(hl_range, hl_range$High_K, 50)
hl_range$L <- range_HL(hl_range, hl_range$High_L, 55)
hl_range$M <- range_HL(hl_range, hl_range$High_M, 60)
hl_range$N <- range_HL(hl_range, hl_range$High_N, 65)
hl_range$day_month <- Highs$day_month

quantile_range <- hl_range %>% select(day_month, A:N) %>% mutate(across(c(A:N),~round(.,2)))

write.csv(quantile_range, "quantile_range.csv", row.names = FALSE)
