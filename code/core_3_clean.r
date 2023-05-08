### Core ideas for efficient data analysis in R ##
## 3: Data cleaning is like nativating a river

## Diego Catalan Molina, March 2023

# 1. Libraries & Data --------------------------------------
library(tidyverse)

# data
core_data <- read_csv("data/core_data.csv")

# clean data
clean_data <- 
  core_data %>%
  rename(
    test_score = ach, # new name = old name
    belonging = sb
  ) %>% 
  mutate( # create new variables
    above_avg = # name of new variable
      if_else(
        test_score > mean(test_score, na.rm = TRUE), # condition
        1, # value if true
        0 # value if false
      )
  )
  
# check results
summary(clean_data)
