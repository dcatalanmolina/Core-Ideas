### Core ideas for efficient data analysis in R ##
## 1: Everything is an object

## Diego Catalan Molina, March 2023

# 1. Libraries & Data --------------------------------------
library(tidyverse)

# data
core_data <- read_csv("data/core_data.csv")


# what's in your data (object)?
summary(core_data)

# sb = sense of belonging at school (standardized, M = 0, SD = 1)
# threat = stereotype threat at school (standardized, M = 0, SD = 1)
# ach = test scores (standardized, M = 0, SD = 1)
# minority = whether student belongs to minority group or not
# gender = female, male, non-binary
# standards = whether student achieves a standard's low, average, or high level
# engaged = whether student was engaged (1) or not (0) at school
# stgy = Likert item that measures how often students 
  # used a learning strategy (1 = never to 5 = always) 

# running functions as a chain of events or a pipe
core_data %>% summary(.)
