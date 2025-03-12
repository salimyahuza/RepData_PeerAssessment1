---
title: "Reproducible Research: Peer Assessment 1"
author: "Salim Yahuza Gwarjo"
date: "`r Sys.Date()`"
output: html_document
---

## **Introduction**
This report analyzes personal activity monitoring data collected at 5-minute intervals over two months. The objective is to explore activity patterns, handle missing values, and compare weekday versus weekend activity levels.

```{r setup, include=FALSE}
# Load required libraries
library(ggplot2)
library(dplyr)
library(knitr)

# Loading and Preprocessing the Data
# Read the dataset
activity_data <- read.csv("activity.csv")

# Convert date to Date format
activity_data$date <- as.Date(activity_data$date)

# Total Number of Steps Taken per Day
# Calculate total steps per day
total_steps_per_day <- activity_data %>% group_by(date) %>% summarise(total_steps = sum(steps, na.rm = TRUE))

# Plot histogram
hist(total_steps_per_day$total_steps, col = "blue", main = "Total Steps per Day", xlab = "Steps")

# Compute mean and median
mean_steps <- mean(total_steps_per_day$total_steps, na.rm = TRUE)
median_steps <- median(total_steps_per_day$total_steps, na.rm = TRUE)

# Display results
mean_steps
median_steps

# Average Daily Activity Pattern
# Calculate average steps per interval
avg_steps_interval <- activity_data %>% group_by(interval) %>% summarise(avg_steps = mean(steps, na.rm = TRUE))

# Time series plot
plot(avg_steps_interval$interval, avg_steps_interval$avg_steps, type = "l", col = "red", 
     xlab = "Interval", ylab = "Average Steps", main = "Average Daily Activity Pattern")

# Find interval with max steps
max_interval <- avg_steps_interval[which.max(avg_steps_interval$avg_steps), ]
max_interval

# Inputting Missing Values
# Count missing values
missing_values <- sum(is.na(activity_data$steps))

# Impute missing values using interval mean
activity_data_imputed <- activity_data
activity_data_imputed$steps[is.na(activity_data_imputed$steps)] <- 
  ave(activity_data$steps, activity_data$interval, FUN = function(x) mean(x, na.rm = TRUE))

# Histogram after imputation
total_steps_imputed <- activity_data_imputed %>% group_by(date) %>% summarise(total_steps = sum(steps))
hist(total_steps_imputed$total_steps, col = "green", main = "Total Steps per Day (After Imputation)", xlab = "Steps")

# Compute new mean and median
mean_steps_imputed <- mean(total_steps_imputed$total_steps)
median_steps_imputed <- median(total_steps_imputed$total_steps)

# Display results
mean_steps_imputed
median_steps_imputed

# Weekday vs. Weekend Activity Patterns
# Create a weekday/weekend factor variable
activity_data_imputed$day_type <- ifelse(weekdays(activity_data_imputed$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")

# Calculate average steps by interval and day type
avg_steps_by_day <- activity_data_imputed %>% group_by(interval, day_type) %>% summarise(avg_steps = mean(steps))

# Panel plot for weekday vs weekend
ggplot(avg_steps_by_day, aes(x = interval, y = avg_steps, color = day_type)) +
  geom_line() +
  facet_wrap(~day_type, ncol = 1) +
  labs(title = "Weekday vs Weekend Activity", x = "Interval", y = "Average Steps")


