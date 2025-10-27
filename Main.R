################################
# Import packages
#install.packages("tidyverse")
#install.packages("readODS")
library(tidyverse)
library(readODS)
# Import data sets
arrests_25_7_30 <- read_ods("arrests-latest_2025_07_30.ods", 
                            sheet = 1,
                            col_names = TRUE,
                            col_types = NULL,
                            na = "",
                            skip = 3,
)

detainers_25_7_30 <- read_ods("detainers-latest_2025_07_30.ods", 
                            sheet = 1,
                            col_names = TRUE,
                            col_types = NULL,
                            na = "",
                            skip = 4,
)

# Format

arrests_25_7_30 <- arrests_25_7_30[,c(2:28)] # remove empty column
detainers_25_7_30 <- detainers_25_7_30[,c(2:71)]

arrests_25_7_30$apprehension_date <- as.Date(arrests_25_7_30$apprehension_date) #month name
arrests_25_7_30$month_name <- month(arrests_25_7_30$apprehension_date, label = T, abbr = FALSE)

arrests_25_7_30$criminal_conviction <- if_else(arrests_25_7_30$apprehension_criminality=="1 Convicted Criminal","yes","no") 

# 287g Arrests

arrests_287g <- arrests_25_7_30 %>%
  filter(apprehension_method == "287(g) Program")

criminality_arrests287g <- arrests_287g %>%
  group_by(apprehension_criminality) %>%
  summarize(total_arrests = sum(count, na.rm = TRUE))

# Yearly df

yearly_arrests <- arrests_25_7_30 %>%
  group_by(year, apprehension_criminality, ) %>%
  summarize(total_arrests = sum(count, na.rm = T))

yearly_detainers <- detainers_25_7_30 %>%
  group_by(year, `Detainer Prepared Criminality`) %>%
  summarize(total_arrests = sum(count, na.rm = T))

arrests_by_age <- arrests_25_7_30 %>%
  mutate(age = year - birth_year) %>%
  group_by(age, apprehension_criminality) %>%
  summarize(total_arrests = sum(count, na.rm = T))

detainers_by_age <- detainers_25_7_30 %>%
  mutate(age = year - `Birth Year`) %>%
  group_by(age, `Detainer Prepared Criminality`) %>%
  summarize(total_arrests = sum(count, na.rm = T))
  
# Monthly df



# Cases by county


