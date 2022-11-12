library(dplyr)
library(ggplot2)

df <- read.csv("transfers.csv")
head(df)
dim(df)

# working df
df_u <- df

# find NAs
na_loc <- apply(is.na(df_u), 2, which) # na locations in each col
na_age <- na_loc$player_age
na_transfer_amt <- na_loc$transfer_fee_amnt
na_market_amt <- na_loc$market_val_amnt

na_loc

# impute mean of the age to the observations missing age
df_u[na_age,]$player_age <- mean(df_u$player_age[-na_age])
df_u[na_age,]

# impute transfer amount
head(df_u[na_transfer_amt,])

df_u_no_na <- df_u[c(-na_transfer_amt,-na_market_amt),] # remove NAs

dt <- sort(sample(nrow(df_u_no_na), nrow(df_u_no_na)*.7)) # train test split
train <- df_u_no_na[dt,]
test <- df_u_no_na[-dt,]

ta_mvr <- lm(transfer_fee_amnt ~ league + season + player_age + market_val_amnt + is_free, train) # build model
summary(ta_mvr)

results <- data.frame(actual = test$transfer_fee_amnt, predicted = predict(ta_mvr, test)) # predict test
results$diff <- abs(results$actual - results$predicted) # test accuracy
mean(results$diff) # 4 802 714

df_u[na_transfer_amt,]$transfer_fee_amnt <- predict(ta_mvr, df_u[na_transfer_amt,]) # impute

# find NAs again
na_loc <- apply(is.na(df_u), 2, which) # na locations in each col
na_loc
na_transfer_amt <- na_loc$transfer_fee_amnt
na_market_amt <- na_loc$market_val_amnt

# impute market amount
head(df_u[na_market_amt,])

df_u_no_na <- df_u[c(-na_transfer_amt,-na_market_amt),] # remove NAs

dt <- sort(sample(nrow(df_u_no_na), nrow(df_u_no_na)*.7)) # train test split
train <- df_u_no_na[dt,]
test <- df_u_no_na[-dt,]

ma_mvr <- lm(market_val_amnt ~ league + season + player_age + transfer_fee_amnt + is_free + is_loan, train) # build model
summary(ma_mvr)

results <- data.frame(actual = test$market_val_amnt, predicted = predict(ma_mvr, test)) # predict test
results$diff <- abs(results$actual - results$predicted) # test accuracy
mean(results$diff) # 3 862 976

df_u[na_market_amt,]$market_val_amnt <- predict(ma_mvr, df_u[na_market_amt,]) # impute

# find NAs again
na_loc <- apply(is.na(df_u), 2, which) # na locations in each col
na_loc
na_transfer_amt <- na_loc$transfer_fee_amnt
na_market_amt <- na_loc$market_val_amnt

# impute transfer & market for observations missing both values
head(df_u[c(na_transfer_amt,na_market_amt),])

df_u_no_na <- df_u[c(-na_transfer_amt,-na_market_amt),] # remove NAs

dt <- sort(sample(nrow(df_u_no_na), nrow(df_u_no_na)*.7)) # train test split
train <- df_u_no_na[dt,]
test <- df_u_no_na[-dt,]

ta_mvr <- lm(transfer_fee_amnt ~ league + season + player_age + is_free + is_loan, train) # build model
ma_mvr <- lm(market_val_amnt ~ league + season + player_age + is_free + is_loan, train) # build model
summary(ta_mvr)
summary(ma_mvr)

results <- data.frame(actual_ta = test$transfer_fee_amnt, actual_ma = test$market_val_amnt, predicted_ta = predict(ta_mvr, test), predicted_ma = predict(ma_mvr, test)) # predict test
results$diff_ta <- abs(results$actual_ta - results$predicted_ta) # test accuracy
results$diff_ma <- abs(results$actual_ma - results$predicted_ma) # test accuracy
mean(results$diff_ta) # 3 135 225
mean(results$diff_ma) # 3 644 175

df_u[na_transfer_amt,]$transfer_fee_amnt <- predict(ta_mvr, df_u[na_transfer_amt,]) # impute
df_u[na_market_amt,]$market_val_amnt <- predict(ma_mvr, df_u[na_market_amt,]) # impute

# final df
df_f <- df_u
write.csv(df_f, "/Users/dylanfox/Desktop/transfers-final.csv")

# group by league
by_league <- df_f %>% group_by(league)

# avg age by league
by_league %>% summarise(
  player_age = mean(player_age)
)

age <- df_f %>% group_by(league, season) %>% summarise(avg_age = mean(player_age))
ggplot(age, aes(x=season, y=avg_age, color=league)) + geom_point()

tfa <- df_f %>% group_by(league, season) %>% summarise(avg_tfa = mean(transfer_fee_amnt))
ggplot(tfa, aes(x=season, y=avg_tfa, color=league)) + geom_point()

mva <- df_f %>% group_by(league, season) %>% summarise(avg_mva = mean(market_val_amnt))
ggplot(mva, aes(x=season, y=avg_mva, color=league)) + geom_point()

df_u %>%
  group_by(season) %>%
  summarise(t_transfer = n())