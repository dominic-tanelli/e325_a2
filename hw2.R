# Dominic Tanelli
# Prof. Kropp
# ENVST 325
# 26 September 2024

# Homework 2: Intro to Data Wrangling

# In-Class Prompts
# Starter Code
# install.packages(c("dplyr", "lubridate", "ggplot2"))
library(dplyr)
library(lubridate)
library(ggplot2)
streamH <- read.csv("/cloud/project/stream_gauge.csv")
siteinfo <- read.csv("/cloud/project/site_info.csv")

# Prompt 1
floods <- full_join(streamH, siteinfo, by = "siteID")
cat("\nPrompt 1 Answer (Flood Dataframe): \n")
print(floods)
floods_inner <- inner_join(streamH, siteinfo, by = "siteID")
floods_left <- left_join(streamH, siteinfo, by = "siteID")
floods_right <- right_join(streamH, siteinfo, by = "siteID")
cat("\nPrompt 1 Answer (Different Join Type Comparisons): \n")
dim(floods)  # Full join
dim(floods_inner)  # Inner join
dim(floods_left)  # Left join
dim(floods_right)  # Right join

# Prompt 2
cat("\nPrompt 2 Answer: \n")
ymd_hm(floods$datetime)

# Prompt 3
flood_stage_earliest_date <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= flood.ft) %>%
  slice_min(order_by = datetime, with_ties = FALSE) %>%
  select(names, datetime)
cat("\nPrompt 3 Answer: \n")
print(flood_stage_earliest_date)

# Homework Part II
# Question 1
fisheating_creek <- floods %>%
  filter(siteID == 2256500) %>%
  select(gheight.ft, datetime)
fisheating_creek$datetime <- as.POSIXct(fisheating_creek$datetime)

ggplot(fisheating_creek, aes(x = datetime, y = gheight.ft)) +
  geom_line(color = "green") +  
  labs(title = "Fisheating Creek Stream Stage Over Time",
       x = "Date",
       y = "Stream Stage (ft)") +
  theme_minimal()

peace_river <- floods %>%
  filter(siteID == 2295637) %>%
  select(gheight.ft, datetime)
peace_river$datetime <- as.POSIXct(peace_river$datetime)

ggplot(peace_river, aes(x = datetime, y = gheight.ft)) +
  geom_line(color = "blue") +  
  labs(title = "Peace River Stream Stage Over Time",
       x = "Date",
       y = "Stream Stage (ft)") +
  theme_minimal()

santa_fe_river <- floods %>%
  filter(siteID == 2322500) %>%
  select(gheight.ft, datetime)
santa_fe_river$datetime <- as.POSIXct(santa_fe_river$datetime)

ggplot(santa_fe_river, aes(x = datetime, y = gheight.ft)) +
  geom_line(color = "chocolate") +  
  labs(title = "Santa Fe River Stream Stage Over Time",
       x = "Date",
       y = "Stream Stage (ft)") +
  theme_minimal()

withlacoochee_river <- floods %>%
  filter(siteID == 2312000) %>%
  select(gheight.ft, datetime)
withlacoochee_river$datetime <- as.POSIXct(withlacoochee_river$datetime)

ggplot(withlacoochee_river, aes(x = datetime, y = gheight.ft)) +
  geom_line(color = "aquamarine") +  
  labs(title = "Withlacoochee River Stream Stage Over Time",
       x = "Date",
       y = "Stream Stage (ft)") +
  theme_minimal()

# Question 2
action_stage_earliest_date <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= action.ft) %>%
  slice_min(order_by = datetime, with_ties = FALSE) %>%
  select(names, datetime)
print(action_stage_earliest_date)

print(flood_stage_earliest_date)

moderate_stage_earliest_date <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= moderate.ft) %>%
  slice_min(order_by = datetime, with_ties = FALSE) %>%
  select(names, datetime)
print(moderate_stage_earliest_date)

major_stage_earliest_date <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= major.ft) %>%
  slice_min(order_by = datetime, with_ties = FALSE) %>%
  select(names, datetime)
print(major_stage_earliest_date)

# Question 3

# Question 4
cat("\nQuestion 4 Answer: See Word PDF")
