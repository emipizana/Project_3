library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(forecast)

setwd("/Users/emipiz/Desktop/Proyecto 3")
data_frame_1 <- read.csv("FAOSTAT_data_en_8-3-2022.csv")

##Data summary

summary(data_frame_1)
str(data_frame_1)

##Missing Values

no_missing_values <- sum(is.na(data_frame_1))
sprintf("We have left %1.0f missing values", no_missing_values)

##China Population

China_pop <- data_frame_1 %>%
  filter(Area == "China")

China_graph <- ggplot(China_pop, aes(Year, Value, color = Element)) + 
  geom_line() + labs(
    title = "Population in china",
    y = "Value (1000 people)", x = "Year")

China_graph

##Mexico Population

Mexico_pop <- data_frame_1 %>%
  filter(Area == "Mexico")

Mexico_graph <- ggplot(Mexico_pop, aes(Year, Value, color = Element)) + 
  geom_line() + labs(
    title = "Population in Mexico",
    y = "Value (1000 people)", x = "Year"
  )

Mexico_graph

##USA Population

USA_pop <- data_frame_1 %>%
  filter(Area == "United States of America")

USA_graph <- ggplot(USA_pop, aes(Year, Value, color = Element)) + 
  geom_line() + labs(
    title = "Population in USA",
    y = "Value (1000 people)", x = "Year"
  )

USA_graph

#Time series China

China_urban <- China_pop %>%
  filter(Element == "Urban population")

China_rural <- China_pop %>%
  filter(Element == "Rural population")

china_ts_urban_2000 <- ts(China_urban$Value, start = 1950, end = 2000, frequency = 1)
china_ts_urban <- ts(China_urban$Value, start = 1950, end = 2018, frequency = 1)

prediction_urban_train <- forecast(china_ts_urban_2000, h= 18)
prediction_urban <- forecast(china_ts_urban, h = 20)

summary(prediction_urban_train)
summary(prediction_urban)

#Time serie graph
China_df_train <- data.frame(prediction_urban_train)
China_df_train <- China_df_train %>%
  mutate(Year = c(2001:2018), Value = Point.Forecast)

China_df_pred <- data.frame(prediction_urban)
China_df_pred <- China_df_pred %>%
  mutate(Year = c(2019:2038), Value = Point.Forecast)

graph_china_urban_2000 <- ggplot() +
  geom_line(data = China_urban, aes(Year,Value)) + 
  geom_line(data = China_df_train, aes(Year,Value), color = "Red")

graph_china_urban_2000

graph_china_urban_pred <- ggplot() + 
  geom_line(data = China_urban, aes(Year, Value)) +
  geom_line(data = China_df_pred, aes(Year, Value), color = "Red") + 
  labs(title = "Urban Population in China", y = "Population", x = "Year")

graph_china_urban_pred


#Regression Model China population
lm_model <- lm(Value ~ Year, data = China_urban)

summary(lm_model)
RSS <- c(crossprod(lm_model$residuals))
MSE <- RSS / length(lm_model$residuals)
RMSE_lm <- sqrt(MSE)

plot(prediction_urban_train)

df_Year <- data.frame(c(2019:2038))
df_Year <- df_Year %>%
  mutate(Year= c.2019.2038.) %>%
  select(Year)

predict_lm <- predict(lm_model, df_Year)


#Linear Regression Graph
df_prediction_lm <- cbind(df_Year, predict_lm)

graph_lm_model <- ggplot() + 
  geom_point(data = China_urban, aes(x = Year, y = Value)) + 
  geom_point(data = df_prediction_lm, aes(x = Year, y = predict_lm), color = "Red")
graph_lm_model

#We can see that with a time serie we can predict better results.

