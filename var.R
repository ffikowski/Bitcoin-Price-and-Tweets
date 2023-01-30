# ATMM (Felix Fikowski, 280189)

################################################################################
#Laden benötigter Pakete
{
  library(ggplot2)
  library(xtable)
  library(ggpubr)
  library(vars)
  library(glue)
}
################################################################################

#Setzen des Dateipfades

#Import of the data
{
  setwd("C:\\Users\\Felix\\Documents\\TU Dortmund\\Advanced Text Mining Methods")
    
  df <- read.csv("final_data.csv")
  df <- as.data.frame(df)
  head(df)
  dim(df)[1]
  #[1] 1183   13
}


#xtable(con_table)

################################################################################
#plot of the different time series

#plot BTC
BTC_plot<-plot(df$BTC, xlab = "t", ylab = expression(BTC[t]), type = "o",
     main = "BTC Time Series")

#plot likes
likes_plot<-plot(df$likes_mean, xlab = "t", ylab = expression(likes[t]), type = "o",
                 main = "likes Time Series")

#plot replies
replies_plot<-plot(df$replies_mean, xlab = "t", ylab = expression(replies[t]), type = "o",
                 main = "replies Time Series")

#plot retweets
retweets_plot<-plot(df$retweets_mean, xlab = "t", ylab = expression(retweets[t]), type = "o",
                 main = "retweets Time Series")

#plot BERT_SCORE
BERT_SCORE_plot<-plot(df$BERT_SCORE_mean, xlab = "t", ylab = expression(BERT_SCORE[t]), type = "o",
                 main = "BERT_SCORE Time Series")

#plot Vader_Comp
Vader_Comp_plot<-plot(df$Vader_Comp_, xlab = "t", ylab = expression(Vader_Comp[t]), type = "o",
                 main = "Vader_Comp Time Series")

#plot tweets_number
tweets_number_plot<-plot(df$tweets_number, xlab = "t", ylab = expression(tweets_number[t]), type = "o",
                 main = "tweets_number Time Series")
################################################################################
#Transform the data as mentioned in the project description

{
  matrix <- data.matrix(df, rownames.force = NA)
  dim(df)
  #transform BTC
  BTC <- matrix[,9]
  BTC_per <- diff(BTC,lag=1, differences = 1)/BTC[1:(dim(df)[1]-1)]*100
  length(BTC_per)
  #transform manufacturing industry capacity utilization (likes)
  likes <- matrix[2:dim(df)[1],4]
  length(likes)
  #transform repliesloymentrate (replies )
  replies <- matrix[2:dim(df)[1],5]
  
  retweets <- matrix[2:dim(df)[1],6]
  
  BERT_score <- matrix[2:dim(df)[1],7]
  
  vader <- matrix[2:dim(df)[1],8]
  
  tweets_number <- matrix[2:dim(df)[1],3]

  dates <- as.Date(df[2:dim(df)[1],1], "%Y-%m-%d")

  per_matrix <- cbind(BTC_per, likes, replies, retweets, BERT_score, vader, tweets_number)
  head(per_matrix)
  
  data <- data.frame(dates,per_matrix[,1], per_matrix[,2], per_matrix[,3],
                     per_matrix[,4], per_matrix[,5], per_matrix[,6], per_matrix[,7])
  colnames(data) <- c("dates", "BTC_per", "likes", "replies", 
                      "retweets", "BERT_score", "vader", "tweets_number")
  #5-Number summary of the data:
  
  BTC_per<-summary(data$BTC_per)
  likes<-summary(data$likes)
  replies<-summary(data$replies)
  retweets<-summary(data$retweets)
  BERT_score<-summary(data$BERT_score)
  vader<-summary(data$vader)
  tweets_number<-summary(data$tweets_number)
  
  
  compare1<-cbind(BTC_per,likes, replies, retweets, BERT_score, vader, tweets_number)
  con_table <- round(compare1,2)
  con_table
}

#BTC_per   likes replies retweets BERT_score vader tweets_number
#Min.     -18.96    0.00    0.00     0.00       0.00 -0.77          0.00
#1st Qu.   -1.06   19.53    1.00    15.75       0.84  0.02          2.00
#Median     0.23   89.94    7.38    45.37       0.87  0.17          9.00
#Mean       0.27  193.87   15.94    92.63       0.81  0.16         73.29
#3rd Qu.    1.71  214.79   16.99    89.26       0.90  0.26         29.00
#Max.      25.59 9312.21  772.70  3071.57       0.95  0.87       4521.00
xtable(con_table)

################################################################################
#TAsk a)
#plot generation of the 7 time series
{
  series_BTC_per <- ggplot(data = data, aes(x=dates, y=BTC_per)) +
    geom_line() + 
    ylab("BTC gr. [%]") +
    xlab("")
  
  series_likes <-ggplot(data, aes(x=dates, y=likes)) +
    geom_line() + 
    ylab("likes [%]") +
    xlab("")
  
  series_replies <-ggplot(data, aes(x=dates, y=replies)) +
    geom_line() + 
    ylab("replies [%]") +
    xlab("")
  
  series_retweets <-ggplot(data, aes(x=dates, y=retweets)) +
    geom_line() + 
    ylab("Inflation [%]") +
    xlab("")
  
  series_BERT_score <-ggplot(data, aes(x=dates, y=BERT_score)) +
    geom_line() + 
    ylab("BERT_score [%]") +
    xlab("")
  
  series_vader <-ggplot(data, aes(x=dates, y=vader)) +
    geom_line() + 
    ylab("vader gr. [%]") +
    xlab("year")
  
  series_tweets_number <-ggplot(data, aes(x=dates, y=tweets_number)) +
    geom_line() +
    ylab("tweets_number gr. [%]") +
    xlab("year")
  
  figure <- ggarrange(series_BTC_per, series_likes, series_replies, series_retweets, 
                      series_BERT_score, series_vader, series_tweets_number,
                      ncol = 2, nrow = 4)
  figure <- figure +theme(plot.margin = margin(0.5,0,0,0, "cm"))
  

  pdf("series.pdf")
  plot(figure)
  dev.off()
}
pdf("series.pdf")
################################################################################
#task b)

#Forecast using an AR(1) model
forecast_ar <- c()
for(i in 3:(length(data$BTC_per)-1)) {
  BTC_ar<-ar.ols(data$BTC_per[1:i], aic = FALSE,order.max = 1)
  BTC_ar_forecast <- predict(BTC_ar, n.ahead = 1)
  new_value <- BTC_ar_forecast$pred           
  forecast_ar <- c(forecast_ar, new_value)    
}

data_forecast <- data.frame(data$dates[4:dim(data)[1]],data$BTC_per[4:dim(data)[1]], forecast_ar)
colnames(data_forecast) <- c("dates", "BTC_per", "forecast_ar")

write.csv(data_forecast,"C:\\Users\\Felix\\Documents\\TU Dortmund\\Advanced Text Mining Methods\\data_forecast.csv", row.names = FALSE)

ar_plot <- ggplot(data_forecast, aes(dates)) + 
  geom_line(aes(y = BTC_per, colour = "actual BTC growth")) + 
  geom_line(aes(y = forecast_ar, colour = "AR(1) forecast"), linetype = "dashed") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") + ylab("BTC growth in %") +
  theme(legend.position="bottom")
ar_plot

pdf("ar_plot.pdf")
plot(ar_plot)
dev.off()

#calculate the RMSE

RMSE_AR <- sqrt(mean((data_forecast$BTC_per - data_forecast$forecast_ar)^2))
RMSE_AR

###################################### ##########################################
#task c)
#Forecast using an VAR(1) model

var_forecast <- c()
for(i in 9:(dim(data)[1]-1)) {
  var_model <- VAR(data[c(1:i),c(2,3,4,5,6,7,8)], p = 1, type = "cons", season = NULL, exog = NULL)
  var_forecast_total <- predict(var_model, n.ahead = 1)
  new_value <- var_forecast_total$fcst$BTC_per[1]           
  var_forecast <- c(var_forecast, new_value)    
}
length(var_forecast)
length(7:dim(data_forecast)[1])
dim(data_forecast)

data_var_forecast <- data.frame(data_forecast[7:dim(data_forecast)[1],1:3],var_forecast)
colnames(data_var_forecast) <- c("dates", "BTC_per", "ar_forecast", "var_forecast")
write.csv(data_var_forecast,"C:\\Users\\Felix\\Documents\\TU Dortmund\\Case Studies\\project 2\\data_var_forecast.csv", row.names = FALSE)

var1_plot <- ggplot(data_var_forecast, aes(dates)) + 
  geom_line(aes(y = BTC_per, colour = "actual BTC growth")) + 
  geom_line(aes(y = ar_forecast, colour = "AR(1) forecast"), linetype = "dashed") +
  geom_line(aes(y = var_forecast, colour = "VAR(1) forecast"), linetype = "dashed") +
  xlab("Time") + ylab("BTC Growth in %") +
  theme(legend.position="bottom")
var1_plot

pdf("var1_plot.pdf")
plot(var1_plot)
dev.off()

#calculate the RMSE

RMSE_VAR <- sqrt(mean((data_var_forecast$BTC_per - data_var_forecast$var_forecast)^2))
RMSE_VAR
################################################################################
#d)

#Calculate the t statistics for the Granger causality test
dim(data[,-c(1)])[1]-dim(data[,-c(1)])[2]
for (i in 1:(dim(data[,-c(1)])[1]-dim(data[,-c(1)])[2])){
  
  ones_felix = rep(1,(dim(data[,-c(1)])[2]-1+i))
  length(yt1_felix)
  yt1_felix = data$BTC_per[1:((dim(data[,-c(1)])[2]-1)+i)]
  yt2_felix = data$likes[1:((dim(data[,-c(1)])[2]-1)+i)]
  yt3_felix = data$replies[1:((dim(data[,-c(1)])[2]-1)+i)]
  yt4_felix = data$retweets[1:((dim(data[,-c(1)])[2]-1)+i)]
  yt5_felix = data$BERT_score[1:((dim(data[,-c(1)])[2]-1)+i)]
  yt6_felix = data$vader[1:((dim(data[,-c(1)])[2]-1)+i)]
  yt7_felix = data$tweets_number[1:((dim(data[,-c(1)])[2]-1)+i)]
  
  z_felix = rbind(ones_felix,yt1_felix,yt2_felix,yt3_felix,yt4_felix,yt5_felix,yt6_felix,yt7_felix)
  Y = rbind(data$BTC_per[2:((dim(data[,-c(1)])[2])+i)],
            data$likes[3:((dim(data[,-c(1)])[2])+1+i)],
            data$replies[3:(dim(data[,-c(1)])[2]+1+i)],
            data$retweets[3:(dim(data[,-c(1)])[2]+1+i)],
            data$BERT_score[3:(dim(data[,-c(1)])[2]+1+i)],
            data$vader[3:(dim(data[,-c(1)])[2]+1+i)],
            data$tweets_number[3:(dim(data[,-c(1)])[2]+1+i)])
}
length(data$BTC_per[2:((dim(data[,-c(1)])[2])+i)])
length(data$likes[3:((dim(data[,-c(1)])[2])+1+i)])
C = solve(z_felix%*%t(z_felix))
B = Y%*%t(z_felix)
A = B%*%C

Resd_1 = Y- (A%*%z_felix)

head(z_felix)
var_P1_1 = solve(z_felix%*%t(z_felix))
var_P1_2 = ((Resd_1%*%t(Resd_1))/(250-(7*1)-1))
var_P1 = kronecker(var_P1_1,var_P1_2)

dim(var_P1_1)

# T-test statistic for BTC

t_test_BTC= A[1,2]/sqrt(var_P1[8,8])
t_test_BTC

#t_test_BTC  1.448

# For a large sample size it follows a normal distribution
# at the level of significance alpha = 0.05 we have that for a 
# 2 sided test the critical value is (+/-) 1.96
# because 0.05203193 < 1.96 we do not reject the null hypothesis, which indicates
# that the parameter estimation for lag 1 BTC could be 0.

t_test_likes= A[1,3]/sqrt(var_P1[15,15])
t_test_likes

# t_test_likes = 4.456844

t_test_replies= A[1,4]/sqrt(var_P1[22,22])
t_test_replies

# t_test_replies = 4.03169

t_test_retweets= A[1,5]/sqrt(var_P1[29,29])
t_test_retweets

# t_test_retweets = -1.607346 

t_test_BERT_score= A[1,6]/sqrt(var_P1[36,36])
t_test_BERT_score

# t_test_BERT_score =  -2.013595

t_test_vader= A[1,7]/sqrt(var_P1[43,43])
t_test_vader

# t_test_vader = 4.618165 

t_test_tweets_number= A[1,8]/sqrt(var_P1[50,50])
t_test_tweets_number

# t_test_tweets_number = 3.453106

test_res <- cbind(t_test_BTC, t_test_likes, t_test_replies, t_test_retweets, t_test_BERT_score, t_test_vader, t_test_tweets_number)
tests <- data.frame(test_res)
colnames(tests) <- c("BTC_per", "likes", "replies", "retweets", "BERT_score", "vader", "tweets_number")

xtable(tests)

################################################################################
#e)

#determine the AIC criteria up to lag-order 4

var_model_lag_sel <- VARselect(data[c(1:dim(data)[1]),c(2,3,4,5,6,7,8)], lag.max = 4, type = "const",season = NULL, exogen = NULL)
var_model_lag_sel$criteria
xtable(var_model_lag_sel$criteria)
varp_model <- VAR(data[c(1:33),c(2,3,4,5,6,7,8)], p = 4, type = "cons", season = NULL, exog = NULL)
varp_model

#VAR(p) process for the p that minimizes the AIC criteria
varp_forecast <- c()
for(i in 25:(dim(data)[1]-1)) {
  varp_model <- VAR(data[c(1:i),c(2,3,4,5,6,7,8)], p = which.min(var_model_lag_sel$criteria[1,]), type = "cons", season = NULL, exog = NULL)
  varp_forecast_total <- predict(varp_model, n.ahead = 1)
  new_value <- varp_forecast_total$fcst$BTC_per[1]           
  varp_forecast <- c(varp_forecast, new_value)    
}
dim(data_var_forecast)
length(varp_forecast)
data_varp_forecast <- data.frame(data_var_forecast[17:242,1:4],varp_forecast)
colnames(data_varp_forecast) <- c("dates", "BTC_per", "forecast", "var_forecast", "varp_forecast")
write.csv(data_varp_forecast,"C:\\Users\\Felix\\Documents\\TU Dortmund\\Case Studies\\project 2\\data_varp_forecast.csv", row.names = FALSE)


varp_plot <- ggplot(data_varp_forecast, aes(dates)) + 
  geom_line(aes(y = BTC_per, colour = "actual BTC growth")) + 
  geom_line(aes(y = varp_forecast, colour = "VAR(3) forecasted BTC growth")) +
  ggtitle("Forecast of BTC Growth") + theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") + ylab("BTC growth in %") +
  theme(legend.position="bottom")
varp_plot

varp_plot_total <- ggplot(data_varp_forecast, aes(dates)) + 
  geom_line(aes(y = BTC_per, colour = "actual BTC growth")) + 
  geom_line(aes(y = forecast, colour = "AR(1) forecast"), linetype = "dashed") +
  geom_line(aes(y = var_forecast, colour = "VAR(1) forecast"), linetype = "dashed") +
  geom_line(aes(y = varp_forecast, colour = "VAR(3) forecast"), linetype = "dashed") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") + ylab("BTC growth in %") +
  theme(legend.position="bottom")
varp_plot_total

pdf("var3_plot.pdf")
plot(varp_plot_total)
dev.off()

RMSE_VARp <- sqrt(mean((data_varp_forecast$BTC_per - data_varp_forecast$varp_forecast)^2))
RMSE_VARp

################################################################################
#f)

#calculate the RSME for different time frames and models
dim(data_varp_forecast)
data_varp_forecast[219:226,]
#60s

RMSE_AR1_60 <- sqrt(mean((data_varp_forecast$BTC_per[1:18] - data_varp_forecast$forecast[1:18])^2))
RMSE_AR1_60

RMSE_VAR1_60 <- sqrt(mean((data_varp_forecast$BTC_per[1:18] - data_varp_forecast$var_forecast[1:18])^2))
RMSE_VAR1_60

RMSE_VAR3_60 <- sqrt(mean((data_varp_forecast$BTC_per[1:18] - data_varp_forecast$varp_forecast[1:18])^2))
RMSE_VAR3_60

RMSE_60 <- rbind(RMSE_AR1_60,RMSE_VAR1_60,RMSE_VAR3_60)

#70s

RMSE_AR1_70 <- sqrt(mean((data_varp_forecast$BTC_per[19:58] - data_varp_forecast$forecast[19:58])^2))
RMSE_AR1_70

RMSE_VAR1_70 <- sqrt(mean((data_varp_forecast$BTC_per[19:58] - data_varp_forecast$var_forecast[19:58])^2))
RMSE_VAR1_70

RMSE_VAR3_70 <- sqrt(mean((data_varp_forecast$BTC_per[19:58] - data_varp_forecast$varp_forecast[19:58])^2))
RMSE_VAR3_70

RMSE_70 <- rbind(RMSE_AR1_70,RMSE_VAR1_70,RMSE_VAR3_70)


#80s

RMSE_AR1_80 <- sqrt(mean((data_varp_forecast$BTC_per[59:98] - data_varp_forecast$forecast[59:98])^2))
RMSE_AR1_80

RMSE_VAR1_80 <- sqrt(mean((data_varp_forecast$BTC_per[59:98] - data_varp_forecast$var_forecast[59:98])^2))
RMSE_VAR1_80

RMSE_VAR3_80 <- sqrt(mean((data_varp_forecast$BTC_per[59:98] - data_varp_forecast$varp_forecast[59:98])^2))
RMSE_VAR3_80

RMSE_80 <- rbind(RMSE_AR1_80,RMSE_VAR1_80,RMSE_VAR3_80)

#90s

RMSE_AR1_90 <- sqrt(mean((data_varp_forecast$BTC_per[99:138] - data_varp_forecast$forecast[99:138])^2))
RMSE_AR1_90

RMSE_VAR1_90 <- sqrt(mean((data_varp_forecast$BTC_per[99:138] - data_varp_forecast$var_forecast[99:138])^2))
RMSE_VAR1_90

RMSE_VAR3_90 <- sqrt(mean((data_varp_forecast$BTC_per[99:138] - data_varp_forecast$varp_forecast[99:138])^2))
RMSE_VAR3_90

RMSE_90 <- rbind(RMSE_AR1_90,RMSE_VAR1_90,RMSE_VAR3_90)

#00s

RMSE_AR1_00 <- sqrt(mean((data_varp_forecast$BTC_per[139:178] - data_varp_forecast$forecast[139:178])^2))
RMSE_AR1_00

RMSE_VAR1_00 <- sqrt(mean((data_varp_forecast$BTC_per[139:178] - data_varp_forecast$var_forecast[139:178])^2))
RMSE_VAR1_00

RMSE_VAR3_00 <- sqrt(mean((data_varp_forecast$BTC_per[139:178] - data_varp_forecast$varp_forecast[139:178])^2))
RMSE_VAR3_00

RMSE_00 <- rbind(RMSE_AR1_00,RMSE_VAR1_00,RMSE_VAR3_00)

#10s

RMSE_AR1_10 <- sqrt(mean((data_varp_forecast$BTC_per[179:218] - data_varp_forecast$forecast[179:218])^2))
RMSE_AR1_10

RMSE_VAR1_10 <- sqrt(mean((data_varp_forecast$BTC_per[179:218] - data_varp_forecast$var_forecast[179:218])^2))
RMSE_VAR1_10

RMSE_VAR3_10 <- sqrt(mean((data_varp_forecast$BTC_per[179:218] - data_varp_forecast$varp_forecast[179:218])^2))
RMSE_VAR3_10
RMSE_10 <- rbind(RMSE_AR1_10,RMSE_VAR1_10,RMSE_VAR3_10)

#20s

RMSE_AR1_20 <- sqrt(mean((data_varp_forecast$BTC_per[219:226] - data_varp_forecast$forecast[219:226])^2))
RMSE_AR1_20

RMSE_VAR1_20 <- sqrt(mean((data_varp_forecast$BTC_per[219:226] - data_varp_forecast$var_forecast[219:226])^2))
RMSE_VAR1_20

RMSE_VAR3_20 <- sqrt(mean((data_varp_forecast$BTC_per[219:226] - data_varp_forecast$varp_forecast[219:226])^2))
RMSE_VAR3_20

RMSE_20 <- rbind(RMSE_AR1_20,RMSE_VAR1_20,RMSE_VAR3_20)

#total
RMSE_AR1_total <- sqrt(mean((data_varp_forecast$BTC_per[1:226] - data_varp_forecast$forecast[1:226])^2))
RMSE_AR1_total

RMSE_VAR1_total <- sqrt(mean((data_varp_forecast$BTC_per[1:226] - data_varp_forecast$var_forecast[1:226])^2))
RMSE_VAR1_total

RMSE_VAR3_total <- sqrt(mean((data_varp_forecast$BTC_per[1:226] - data_varp_forecast$varp_forecast[1:226])^2))
RMSE_VAR3_total

RMSE_total <- rbind(RMSE_AR1_total,RMSE_VAR1_total,RMSE_VAR3_total)
RSME <- cbind(RMSE_60,RMSE_70,RMSE_80,RMSE_90,RMSE_00,RMSE_10,RMSE_20,RMSE_total)
RSME

xtable(RSME,digits=c(0,4,4,4,4,4,4,4,4))

