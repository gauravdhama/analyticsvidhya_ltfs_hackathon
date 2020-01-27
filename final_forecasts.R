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
  mutate(data_ts = map(data, to_ts2))

# Declare different algorithms to run
nnetar0_forec <- function(x,h) {
  model <- forecast::nnetar(x, h=h, P=1, p=31, size = 27,lambda = "auto", repeats = 100)
  forecast::forecast(model, h=h)
}

nnetar1_forec <- function(x,h) {
  model <- forecast::nnetar(x, h=h, P=1, p=32, size = 26,lambda = "auto", repeats = 100)
  forecast::forecast(model, h=h)
}

nnetar2_forec <- function(x,h) {
  model <- forecast::nnetar(x, h=h, P=1, p=33, size = 25,lambda = "auto", repeats = 100)
  forecast::forecast(model, h=h)
}

nnetar3_forec <- function(x,h) {
  model <- forecast::nnetar(x, h=h, P=1, p=34, size = 25,lambda = "auto", repeats = 100)
  forecast::forecast(model, h=h)
}

nnetar4_forec <- function(x,h) {
  model <- forecast::nnetar(x, h=h, P=1, p=35, size = 24,lambda = "auto", repeats = 100)
  forecast::forecast(model, h=h)
}

nnetar5_forec <- function(x,h) {
  model <- forecast::nnetar(x, h=h, P=1, p=36, size = 24,lambda = "auto", repeats = 100)
  forecast::forecast(model, h=h)
}

nnetar6_forec <- function(x,h) {
  model <- forecast::nnetar(x, h=h, P=1, p=37, size = 23,lambda = "auto", repeats = 100)
  forecast::forecast(model, h=h)
}

nnetar7_forec <- function(x,h) {
  model <- forecast::nnetar(x, h=h, P=1, p=38, size = 23,lambda = "auto", repeats = 100)
  forecast::forecast(model, h=h)
}

nnetar8_forec <- function(x,h) {
  model <- forecast::nnetar(x, h=h, P=1, p=39, size = 23,lambda = "auto", repeats = 100)
  forecast::forecast(model, h=h)
}

nnetar9_forec <- function(x,h) {
  model <- forecast::nnetar(x, h=h, P=1, p=40, size = 23,lambda = "auto", repeats = 100)
  forecast::forecast(model, h=h)
}

nnetar10_forec <- function(x,h) {
  model <- forecast::nnetar(x, h=h, P=1, p=30, size = 28,lambda = "auto", repeats = 100)
  forecast::forecast(model, h=h)
}

nnetar11_forec <- function(x,h) {
  model <- forecast::nnetar(x, h=h, P=1, p=29, size = 29,lambda = "auto", repeats = 100)
  forecast::forecast(model, h=h)
}

nnetar12_forec <- function(x,h) {
  model <- forecast::nnetar(x, h=h, P=1, p=28, size = 30,lambda = "auto", repeats = 100)
  forecast::forecast(model, h=h)
}

nnetar13_forec <- function(x,h) {
  model <- forecast::nnetar(x, h=h, P=1, p=27, size = 31,lambda = "auto", repeats = 100)
  forecast::forecast(model, h=h)
}

nnetar14_forec <- function(x,h) {
  model <- forecast::nnetar(x, h=h, P=1, p=26, size = 32,lambda = "auto", repeats = 100)
  forecast::forecast(model, h=h)
}

nnetar15_forec <- function(x,h) {
  model <- forecast::nnetar(x, h=h, P=1, p=25, size = 33,lambda = "auto", repeats = 100)
  forecast::forecast(model, h=h)
}

## Run for different variations

# NNETAR0
forecast01 = nnetar0_forec(head(df3[1,3]$data_ts[[1]],825),88)
forecast02 = nnetar0_forec(df3[2,3]$data_ts[[1]],93)
f01 = forecast(forecast01)
f02 = forecast(forecast02)
plot(f01)
plot(f02)
v01 = as.vector(f01$mean)
v02 = as.vector(f02$mean)
v0 = c(v01[2:length(v01)],v02)

# NNETAR1
forecast11 = nnetar1_forec(head(df3[1,3]$data_ts[[1]],825),88)
forecast12 = nnetar1_forec(df3[2,3]$data_ts[[1]],93)
f11 = forecast(forecast11)
f12 = forecast(forecast12)
plot(f11)
plot(f12)
v11 = as.vector(f11$mean)
v12 = as.vector(f12$mean)
v1 = c(v11[2:length(v11)],v12)

# NNETAR2
forecast21 = nnetar2_forec(head(df3[1,3]$data_ts[[1]],825),88)
forecast22 = nnetar2_forec(df3[2,3]$data_ts[[1]],93)
f21 = forecast(forecast21)
f22 = forecast(forecast22)
plot(f21)
plot(f22)
v21 = as.vector(f21$mean)
v22 = as.vector(f22$mean)
v2 = c(v21[2:length(v21)],v22)

# NNETAR3
forecast31 = nnetar3_forec(head(df3[1,3]$data_ts[[1]],825),88)
forecast32 = nnetar3_forec(df3[2,3]$data_ts[[1]],93)
f31 = forecast(forecast31)
f32 = forecast(forecast32)
plot(f31)
plot(f32)
v31 = as.vector(f31$mean)
v32 = as.vector(f32$mean)
v3 = c(v31[2:length(v31)],v32)

# NNETAR4
forecast41 = nnetar4_forec(head(df3[1,3]$data_ts[[1]],825),88)
forecast42 = nnetar4_forec(df3[2,3]$data_ts[[1]],93)
f41 = forecast(forecast41)
f42 = forecast(forecast42)
plot(f41)
plot(f42)
v41 = as.vector(f41$mean)
v42 = as.vector(f42$mean)
v4 = c(v41[2:length(v41)],v42)

# NNETAR5
forecast51 = nnetar5_forec(head(df3[1,3]$data_ts[[1]],825),88)
forecast52 = nnetar5_forec(df3[2,3]$data_ts[[1]],93)
f51 = forecast(forecast51)
f52 = forecast(forecast52)
plot(f51)
plot(f52)
v51 = as.vector(f51$mean)
v52 = as.vector(f52$mean)
v5 = c(v51[2:length(v51)],v52)

# NNETAR6
forecast61 = nnetar6_forec(head(df3[1,3]$data_ts[[1]],825),88)
forecast62 = nnetar6_forec(df3[2,3]$data_ts[[1]],93)
f61 = forecast(forecast61)
f62 = forecast(forecast62)
plot(f61)
plot(f62)
v61 = as.vector(f61$mean)
v62 = as.vector(f62$mean)
v6 = c(v61[2:length(v61)],v62)

# NNETAR7
forecast71 = nnetar7_forec(head(df3[1,3]$data_ts[[1]],825),88)
forecast72 = nnetar7_forec(df3[2,3]$data_ts[[1]],93)
f71 = forecast(forecast71)
f72 = forecast(forecast72)
plot(f71)
plot(f72)
v71 = as.vector(f71$mean)
v72 = as.vector(f72$mean)
v7 = c(v71[2:length(v71)],v72)

# NNETAR8
forecast81 = nnetar8_forec(head(df3[1,3]$data_ts[[1]],825),88)
forecast82 = nnetar8_forec(df3[2,3]$data_ts[[1]],93)
f81 = forecast(forecast81)
f82 = forecast(forecast82)
plot(f11)
plot(f12)
v81 = as.vector(f81$mean)
v82 = as.vector(f82$mean)
v8 = c(v81[2:length(v81)],v82)

# NNETAR9
forecast91 = nnetar9_forec(head(df3[1,3]$data_ts[[1]],825),88)
forecast92 = nnetar9_forec(df3[2,3]$data_ts[[1]],93)
f91 = forecast(forecast91)
f92 = forecast(forecast92)
plot(f91)
plot(f92)
v91 = as.vector(f91$mean)
v92 = as.vector(f92$mean)
v9 = c(v91[2:length(v91)],v92)

# NNETAR10
forecast101 = nnetar10_forec(head(df3[1,3]$data_ts[[1]],825),88)
forecast102 = nnetar10_forec(df3[2,3]$data_ts[[1]],93)
f101 = forecast(forecast101)
f102 = forecast(forecast102)
plot(f101)
plot(f102)
v101 = as.vector(f101$mean)
v102 = as.vector(f102$mean)
v10 = c(v101[2:length(v101)],v102)

# NNETAR11
forecast111 = nnetar11_forec(head(df3[1,3]$data_ts[[1]],825),88)
forecast112 = nnetar11_forec(df3[2,3]$data_ts[[1]],93)
f111 = forecast(forecast111)
f112 = forecast(forecast112)
plot(f111)
plot(f112)
v111 = as.vector(f111$mean)
v112 = as.vector(f112$mean)
v11 = c(v111[2:length(v111)],v112)

# NNETAR12
forecast121 = nnetar12_forec(head(df3[1,3]$data_ts[[1]],825),88)
forecast122 = nnetar12_forec(df3[2,3]$data_ts[[1]],93)
f121 = forecast(forecast121)
f122 = forecast(forecast122)
plot(f121)
plot(f122)
v121 = as.vector(f121$mean)
v122 = as.vector(f122$mean)
v12 = c(v121[2:length(v121)],v122)

# NNETAR13
forecast131 = nnetar13_forec(head(df3[1,3]$data_ts[[1]],825),88)
forecast132 = nnetar13_forec(df3[2,3]$data_ts[[1]],93)
f131 = forecast(forecast131)
f132 = forecast(forecast132)
plot(f131)
plot(f132)
v131 = as.vector(f131$mean)
v132 = as.vector(f132$mean)
v13 = c(v131[2:length(v131)],v132)

# NNETAR14
forecast141 = nnetar14_forec(head(df3[1,3]$data_ts[[1]],825),88)
forecast142 = nnetar14_forec(df3[2,3]$data_ts[[1]],93)
f141 = forecast(forecast141)
f142 = forecast(forecast142)
plot(f141)
plot(f142)
v141 = as.vector(f141$mean)
v142 = as.vector(f142$mean)
v14 = c(v141[2:length(v141)],v142)

# NNETAR15
forecast151 = nnetar15_forec(head(df3[1,3]$data_ts[[1]],825),88)
forecast152 = nnetar15_forec(df3[2,3]$data_ts[[1]],93)
f151 = forecast(forecast151)
f152 = forecast(forecast152)
plot(f151)
plot(f152)
v151 = as.vector(f151$mean)
v152 = as.vector(f152$mean)
v15 = c(v151[2:length(v151)],v152)

## Create an ensemble
vavg_nnetar = rowMeans(cbind(v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15))
vavg = vavg_nnetar

# Prepare submission file
dfo = read.table("sample_submission.csv",fill=TRUE,header=TRUE,sep=",")
dfo$case_count = vavg
write.csv(dfo,file="test_submission_final.csv", row.names=FALSE)
