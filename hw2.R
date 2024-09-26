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
# fisheating_creek data frame set-up
fisheating_creek <- floods %>%
  filter(siteID == 2256500) %>%
  select(gheight.ft, datetime)
fisheating_creek$datetime <- ymd_hm(fisheating_creek$datetime)

# peace_river data frame set-up
peace_river <- floods %>%
  filter(siteID == 2295637) %>%
  select(gheight.ft, datetime)
peace_river$datetime <- ymd_hm(peace_river$datetime)

# santa_fe_river data frame set-up
santa_fe_river <- floods %>%
  filter(siteID == 2322500) %>%
  select(gheight.ft, datetime)
santa_fe_river$datetime <- ymd_hm(santa_fe_river$datetime)

# withlacoochee_river data frame set-up
withlacoochee_river <- floods %>%
  filter(siteID == 2312000) %>%
  select(gheight.ft, datetime)
withlacoochee_river$datetime <- ymd_hm(withlacoochee_river$datetime)

cat("\nQuestion 1 Answer: \n")
# fisheating_creek plot
ggplot(fisheating_creek, aes(x = datetime, y = gheight.ft)) +
  geom_line(color = "green") +  
  labs(title = "Fisheating Creek Stream Stage Over Time",
       x = "Date",
       y = "Stream Stage (ft)") +
  theme_minimal()

# peace_river plot
ggplot(peace_river, aes(x = datetime, y = gheight.ft)) +
  geom_line(color = "blue") +  
  labs(title = "Peace River Stream Stage Over Time",
       x = "Date",
       y = "Stream Stage (ft)") +
  theme_minimal()

# santa_fe_river plot
ggplot(santa_fe_river, aes(x = datetime, y = gheight.ft)) +
  geom_line(color = "chocolate") +  
  labs(title = "Santa Fe River Stream Stage Over Time",
       x = "Date",
       y = "Stream Stage (ft)") +
  theme_minimal()

# withlacoochee_river plot
ggplot(withlacoochee_river, aes(x = datetime, y = gheight.ft)) +
  geom_line(color = "aquamarine") +  
  labs(title = "Withlacoochee River Stream Stage Over Time",
       x = "Date",
       y = "Stream Stage (ft)") +
  theme_minimal()

# Question 2
# Action Stage Earliest Date
action_stage_earliest_date <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= action.ft) %>%
  slice_min(order_by = datetime, with_ties = FALSE) %>%
  select(names, datetime)

# Moderate Stage Earliest Date
moderate_stage_earliest_date <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= moderate.ft) %>%
  slice_min(order_by = datetime, with_ties = FALSE) %>%
  select(names, datetime)

# Major Stage Earliest Date
major_stage_earliest_date <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= major.ft) %>%
  slice_min(order_by = datetime, with_ties = FALSE) %>%
  select(names, datetime)

cat("\nQuestion 2 Answer (Earliest Date of Occurrence for each Flood Category in each River): \n")
print(action_stage_earliest_date)
print(flood_stage_earliest_date)
print(moderate_stage_earliest_date)
print(major_stage_earliest_date)
cat("\nQuestion 2 Answer (Withlacoochee River Flood Category Change in Hours): \n")
difftime(flood_stage_earliest_date$datetime[1], action_stage_earliest_date$datetime[1]) # Withlacoochee River Action to Flood
difftime(moderate_stage_earliest_date$datetime[1], flood_stage_earliest_date$datetime[1]) # Withlacoochee River Flood to Moderate
difftime(major_stage_earliest_date$datetime[1], moderate_stage_earliest_date$datetime[1]) # Withlacoochee River Moderate to Major
cat("\nQuestion 2 Answer (Fisheating Creek Flood Category Change in Hours): \n")
difftime(flood_stage_earliest_date$datetime[2], action_stage_earliest_date$datetime[2]) # Fisheating Creek Action to Flood
difftime(moderate_stage_earliest_date$datetime[2], flood_stage_earliest_date$datetime[2]) # Fisheating Creek Flood to Moderate
difftime(major_stage_earliest_date$datetime[2], moderate_stage_earliest_date$datetime[2]) # Fisheating Creek Moderate to Major
cat("\nQuestion 2 Answer (Peace River Flood Category Change in Hours): \n")
difftime(flood_stage_earliest_date$datetime[3], action_stage_earliest_date$datetime[3]) # Peace River Action to Flood
difftime(moderate_stage_earliest_date$datetime[3], flood_stage_earliest_date$datetime[3]) # Peace River Flood to Moderate
difftime(major_stage_earliest_date$datetime[3], moderate_stage_earliest_date$datetime[3]) # Peace River Moderate to Major

# Question 3
major_stage <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= major.ft) %>%
  slice_max(order_by = gheight.ft, with_ties = FALSE) %>%
  select(names, gheight.ft)
cat("\nQuestion 3 Answer: \n")
print(major_stage)

# Question 4
cat("\nQuestion 4 Answer: See Word PDF")
