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
  tail(df)
  head(df)
}

################################################################################
#Transform the data as mentioned in the project description

{
  dates <- as.POSIXct(df[,1], "%Y-%m-%d %H:%M", tz="UTC")
  data_bert <- data.frame(dates, df$btc_price_per, df$volume_sum_per, df$replies_mean, df$BERT_Sentiment_mean)
  colnames(data_bert) <- c("dates", "BTC_per", "volume_per", "replies","BERT_score")
  dim(data_bert)
}

################################################################################
# BERT
var_model_lag_sel <- VARselect(data_bert[c(1:dim(data_bert)[1]),c(2,3,4,5)], lag.max = 5, type = "const",season = NULL, exogen = NULL)
var_model_lag_sel$criteria
xtable(var_model_lag_sel$criteria,digits=3)


#VAR(p) process for the p that minimizes the AIC criteria
varp_bert <- c()
for(i in 21:(dim(data_bert)[1]-1)) {
  varp_model <- VAR(data_bert[c(1:i),c(2,3,4,5)], p = which.min(var_model_lag_sel$criteria[1,]), type = "cons", season = NULL, exog = NULL)
  varp_forecast_total <- predict(varp_model, n.ahead = 1)
  new_value <- varp_forecast_total$fcst$BTC_per[1]
  varp_bert <- c(varp_bert, new_value)    
}
varp_bert
length(varp_bert)
dim(data)
data_varp_bert <- data.frame(data[22:dim(data)[1],1:2],varp_bert)
colnames(data_varp_bert) <- c("dates", "BTC_per", "varp_forecast")
write.csv(data_varp_bert,"C:\\Users\\Felix\\Documents\\TU Dortmund\\Advanced Text Mining Methods\\data_varp_bert.csv", row.names = FALSE)
head(data_varp_bert)
RMSE_VARp <- sqrt(mean((data_varp_bert$BTC_per - data_varp_bert$varp_forecast)^2))
RMSE_VARp

################################################################################

