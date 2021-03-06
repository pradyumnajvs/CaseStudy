library(plyr)
library(ggplot2)
library(caTools)

rm(list = ls())
setwd("C:/Users/vj853t/Documents/DataScience/Case Study/Case Study 12")

var_name <- read.csv("~/DataScience/Case Study/Case Study 12/variable_names.csv", header = FALSE)
data <- read.csv("~/DataScience/Case Study/Case Study 12/data.csv", header = FALSE)

str(var_name)
str(data)

var_name$clean = substr(var_name$V1, 4, 1000)
var_name$clean = gsub(":.+",'', var_name$clean)

names(data) <- var_name$clean

names(data)

model_data <- data[, names(data) %in% c('ViolentCrimesPerPop', 'pctUrban',
                                        'agePct16t24', 'PctUnemployed', 'medIncome')]

cor(model_data$medIncome, model_data$PctUnemployed)
cor(model_data$medIncome, model_data$ViolentCrimesPerPop)
cor(model_data$PctUnemployed, model_data$ViolentCrimesPerPop)
cor(model_data$PctUnemployed, model_data$agePct16t24)

plot(model_data$medIncome, model_data$PctUnemployed,
xlab = "Median Household Income",
ylab = " percentage of people 16 and over, in the labor force, and unemployed") 
+ title(main ="Median Income per percentage of people 16 and over, in the labor force, and unemployed")

plot(model_data$medIncome, model_data$ViolentCrimesPerPop, 
     xlab = "Median Household Income",
     ylab = " Total amount of crimes per 100K population") 
+ title(main ="Median Income per Total amount of crimes")

plot(model_data$PctUnemployed, model_data$ViolentCrimesPerPop, 
     xlab = "Percentage of Unemployed Population",
     ylab = "Total amount of crimes per population")
+ title(main ="Percentage of Unemployed Population per Total amount of crimes per 100K population")

plot(model_data$PctUnemployed, model_data$agePct16t24, 
     xlab = "Percentage of Unemployed Population",
     ylab = "percentage of population that is 16-24 in age")
+ title(main ="Percentage of Unemployed Population per percentage of population that is 16-24 in age")

set.seed(123)
split = sample.split(model_data$ViolentCrimesPerPop, SplitRatio = 0.8)

data_train <- model_data[split,] 
data_test <- model_data[!split,]

model = lm(formula = ViolentCrimesPerPop ~ medIncome + PctUnemployed + pctUrban + agePct16t24, data = data_train)
model = lm(formula = ViolentCrimesPerPop ~ medIncome + PctUnemployed + pctUrban, data = data_train)
summary(model)

test_predicted_data <- predict(model, newdata = data_test)

ss_residual <- sum((test_predicted_data - data_test$ViolentCrimesPerPop)^2)
ss_total <- sum((data_test$ViolentCrimesPerPop - mean(data_test$ViolentCrimesPerPop))^2)

r_square <- 1 - ss_residual/ss_total

r_square
