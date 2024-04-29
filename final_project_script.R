## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2024_1_17
# Who:       Zachary D. Kline



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
data <- read_delim("data.csv")



##################################################################################
############### Table 1: descriptive statistics    ####################   
##################################################################################
mean(data$year_published)
sd(data$year_published)
table(data$year_published)
describe(data$year_published)
summary(data$year_published)

mean(data$season_show)
sd(data$season_show)
table(data$season_show)
describe(data$season_show)
summary(data$season_show)

mean(data$women_led)
sd(data$women_led)
table(data$women_led)
describe(data$women_led)
summary(data$women_led)

mean(data$emotion)
sd(data$emotion)
table(data$emotion)
describe(data$emotion)
summary(data$emotion)

mean(data$effort)
sd(data$effort)
table(data$effort)
describe(data$effort)
summary(data$effort)

mean(data$credit_rating)
sd(data$credit_rating)
table(data$credit_rating)
describe(data$credit_rating)
summary(data$credit_rating)

##################################################################################
#################### Figure 1: boxplot             ####################   
##################################################################################

#BOX PLOT

ggplot(data, aes(x = data$emotion, y = data$effort)) +
  geom_boxplot() +
  labs(title = "Gilmore Girls, Credit and Effort",
       x = "emotion",
       y = "effort") +
  theme_minimal()

##################################################################################
####################   Figure 2: scatter plot             ####################   
##################################################################################
linear_plot <- plot(data$year_published, data$effort)
print(linear_plot)

# add x line and y line for means
meany <- mean(data$year_published)
meanx <- mean(data$effort)

abline(h = meanx, col = "black")
abline(v = meany, col = "black")


##################################################################################
####################  Figure 3: residual plot                ####################   
##################################################################################
linear_relationship <- lm(data$year_published ~ data$effort, data = data)
summary(linear_relationship)

# Add the linear regression line to the scatter plot
# NOTE: double check the scatter plot is currently in your utilities window!
abline(linear_relationship, col = "red")

plot(data$year_published, residuals(linear_relationship))

plot(data$effort, residuals(linear_relationship))

# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")


##################################################################################
####################  Table 2: contingency table                ####################   
##################################################################################

table(data$emotion,data$credit_rating)
chisq.test(table(data$emotion,data$credit_rating))

table(data$credit_rating)
chisq.test(data$credit_rating)

table(data$emotion,data$credit_rating)
