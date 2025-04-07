#### NT CRIME STATISTICS - DATA ANALYTICS PROJECT ----

### CLEANING ----
#installing packages
install.packages("tidyverse")
Winstall.packages("dplyr")

#loading packages
library(tidyverse)
library(dplyr)
library(ggplot2)

#loading csv into enviro.
crime_jan25 <- read.csv("nt_crime_statistics_jan_2025.csv")
crime_dec24 <- read.csv("nt_crime_statistics_dec_2024.csv")
crime_nov24 <- read.csv("nt_crime_statistics_nov_2024.csv")
crime_sep24 <- read.csv("nt_crime_statistics_sep_2024.csv")
crime_jul24 <- read.csv("nt_crime_statistics_jul_2024.csv")
crime_jun24 <- read.csv("nt_crime_statistics_jun_2024.csv")
crime_may24 <- read.csv("nt_crime_statistics_may_2024.csv")
crime_mar24 <- read.csv("nt_crime_statistics_mar_2024.csv")
crime_feb24 <- read.csv("nt_crime_statistics_feb_2024.csv")
crime_jan24 <- read.csv("nt_crime_statistics_jan_2024.csv")

#combining into one data set
crime_data <- bind_rows(
  crime_jan25,
  crime_dec24,
  crime_nov24,
  crime_sep24,
  crime_jul24,
  crime_jun24,
  crime_may24,
  crime_mar24,
  crime_feb24,
  crime_jan24
)

#ensure col names are consistent
crime_data <- janitor::clean_names(crime_data)

#need to remove dupes
duplicated_rows <- crime_data[duplicated(crime_data), ] #finds dupes and extracts into one dataset

duplicated_records <- crime_data %>%
  group_by(offence_type, ,year, month_number, offence_category, alcohol_involvement, dv_involvement, reporting_region, statistical_area_2, number_of_offences) %>%
  filter(n() > 1) %>%   # Find offenses appearing more than once
  arrange(offence_type, ,year, month_number, offence_category, alcohol_involvement, dv_involvement, reporting_region, statistical_area_2, number_of_offences, as_at) # Sort by confirmation date

print(duplicated_records)  # View multi-month duplicates

cleaned_crime_data <- crime_data %>%
  group_by(offence_type, ,year, month_number, offence_category, alcohol_involvement, dv_involvement, reporting_region, statistical_area_2, number_of_offences) %>%
  arrange(as_at, .by_group = TRUE) %>%
  slice_tail(n = 1)  # Keeps the latest record per group

#went from 19554 records to 4865 - checked against csv manually (large amount of ongoing cases inbetween months)

#check for missing values
colSums(is.na(cleaned_crime_data))  # Count missing values per column

#trimming spaces and lowercase text (avoiding dupes category entries)
cleaned_crime_data <- cleaned_crime_data %>%
  mutate(across(where(is.character), str_trim),
         across(where(is.character), tolower))

#checking type of each col.
str(cleaned_crime_data)

#checking dist of num of offences
boxplot(cleaned_crime_data$number_of_offences)

cleaned_crime_data <- cleaned_crime_data %>%
  mutate(alcohol_involvement = replace_na(alcohol_involvement, "Unknown"),
         dv_involvement = replace_na(dv_involvement, "Unknown"))

#changing yes/no to binary for alcohol involvement and dv involvement
cleaned_crime_data <- cleaned_crime_data %>%
  mutate(alcohol_involvement = if_else(alcohol_involvement == "Yes", 1, 0))

cleaned_crime_data <- cleaned_crime_data %>%
  mutate(dv_involvement = if_else(dv_involvement == "Yes", 1, 0))

#Writing CSV file of cleaned data
#write.csv(cleaned_crime_data, "final_crime_data.csv", row.names = FALSE)


### EXPLORATORY DATA ANALYSIS ----
summary(cleaned_crime_data)  # Gives an overview of numerical columns
str(cleaned_crime_data)  # Checks column types

#missing values
colSums(is.na(cleaned_crime_data)) #0 missing values

#alcohol and dv involvement trends
cleaned_crime_data %>%
  group_by(alcohol_involvement, dv_involvement) %>%
  summarise(total_offenses = sum(number_of_offences))
