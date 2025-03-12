---
title: "NOAA Storm Data Analysis"
author: "Your Name"
date: "2025-03-12"
output: html_document
---

## Synopsis
This analysis explores the NOAA Storm Database to identify the most harmful weather events in terms of population health and economic consequences.

## Data Processing

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

library(dplyr)
library(ggplot2)

# Load the data
storm_data <- read.csv("repdata_data_StormData.csv.bz2")

# Convert the event types to lowercase for consistency
storm_data$EVTYPE <- tolower(storm_data$EVTYPE)

# Summarize data for health impact
health_impact <- storm_data %>%
  group_by(EVTYPE) %>%
  summarize(fatalities = sum(FATALITIES), injuries = sum(INJURIES)) %>%
  arrange(desc(fatalities + injuries))

# Summarize data for economic impact
economic_impact <- storm_data %>%
  group_by(EVTYPE) %>%
  summarize(property_damage = sum(PROPDMG), crop_damage = sum(CROPDMG)) %>%
  arrange(desc(property_damage + crop_damage))

  top_health_impact <- health_impact[1:10, ]
ggplot(top_health_impact, aes(x = reorder(EVTYPE, -(fatalities + injuries)), y = fatalities + injuries)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Most Harmful Weather Events to Population Health",
       x = "Event Type", y = "Total Fatalities and Injuries")

top_economic_impact <- economic_impact[1:10, ]
ggplot(top_economic_impact, aes(x = reorder(EVTYPE, -(property_damage + crop_damage)), y = property_damage + crop_damage)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Weather Events with Greatest Economic Consequences",
       x = "Event Type", y = "Total Property and Crop Damage")

