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

  )