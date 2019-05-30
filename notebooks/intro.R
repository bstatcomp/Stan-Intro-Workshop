# install required packages
install.packages("dplyr") # reshape2, mcmcse, ggplot2, rstan

# load packages
library(dplyr)
library(reshape2)

# set working directory
setwd("D:/Projects/Stan-Intro-Workshop/notebooks")

# load data
data <- read.csv("../data/50_startups.csv")

# filter
data <- filter(data, state == "NewYork")

# select
data_select <- select(data, research, administration, marketing, profit)
data_select2 <- select(data, -state, -profit)

# melt
data_melt <- melt(data_select2)

# pipe
data2 <- data %>% filter(state == "NewYork") %>% select(-state, -profit)

# to access columns of a data frame use the $ sign
data$research
data$state

# to get number of rows in a data frame use nrow
nrow(data)

# we can create new data frames like this, c() combines elements into a vector
new_df <- data.frame(first_column=c(1, 2), second_column=c(10, 11))
new_df2 <- data.frame(first_column=c(3, 4), second_column=c(12, 13))

# we can merge data frames together (if they have same columns) using rbind
new_df3 <- rbind(new_df, new_df2)

# we can extract a column and save it into a vector
research <- data$research

# to get length of a vector use the length function
length(research)

# we pass data to stan through lists
n <- nrow(data)
y <- data$research
stan_data <- list(n = n, y = y)