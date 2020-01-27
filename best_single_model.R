# Set working directory
setwd("~/Desktop/av_ltfs")

# Import necessary libraries
library(forecast)
library(tseries)
library(prophet)
library(forecastHybrid)
library(smooth)
library(dplyr)
library(padr)
library(readr)
library(zoo)
library(xts)
library(tidyverse)
library(timetk)
library(sweep)
library(lubridate)
library(forecTheta)
library(imputeTS)
library(robustbase)
library(forecTheta)
library(opera)
library(dfoptim)

# Read in the dataset
df = read.table("train_fwYjLYX.csv",fill=TRUE,header=TRUE,sep=",")

# Change column names
colnames(df) <- c("Date","segment","branch_id","state","zone","y")

# Convert date to correct format
df$Date = as.Date(df$Date,format="%Y-%m-%d")

# Replace missing value of dates and values
df2 <- df %>% pad(group = c("segment","branch_id","state","zone"),interval = "day")
df2[is.na(df2)] <- 0

# Group by segment
df2 <- df2 %>% group_by(Date,segment) %>% summarise(y=sum(y))

# Convert values to a ts object
to_ts2 <- function(data, freq = 365L) {
  dates <- data$Date
  value <- data$y
  ts(value, start = c(year(dates)[1], yday(dates)[1]), frequency = freq)
}

df3 <- df2 %>% 
  group_by(segment) %>% 
  nest() %>%

nnetar6_forec <- function(x,h) {
  model <- forecast::nnetar(x, h=h, P=1, p=37, size = 23,lambda = "auto", repeats = 200)
  forecast::forecast(model, h=h)
}

forecast61 = nnetar6_forec(head(df3[1,3]$data_ts[[1]],825),88)
forecast62 = nnetar6_forec(df3[2,3]$data_ts[[1]],93)
f61 = forecast(forecast61)
f62 = forecast(forecast62)
plot(f61)
plot(f62)
v61 = as.vector(f61$mean)
v62 = as.vector(f62$mean)
v6 = c(v61[2:length(v61)],v62)

# Prepare submission file
dfo = read.table("sample_submission.csv",fill=TRUE,header=TRUE,sep=",")
dfo$case_count = vavg
write.csv(dfo,file="test_submission_best_single_model.csv", row.names=FALSE)
