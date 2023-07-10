##############################################
## Clean Your Data ##
## Create new variables ##
##############################################



# 1. Libraries & Data --------------------------------------
library(tidyverse)

# data
core_data <- read_csv("data/core_data.csv")

# 2. Clean data ---------------------------------------------

clean_data <- 
  core_data %>%
  mutate( # create new variables
    sb_scaled = sb*100,
    sb_ach = sb + ach,
    sb_divided = sb/2,
    sb_minus = sb - threat

  )

# check your results
head(clean_data$sb_scaled)
head(clean_data$sb)
head(clean_data$threat)
head(clean_data$sb_ach)
head(clean_data$sb_divided)
head(clean_data$sb_minus)
