##############################################
## Clean Your Data ##
## Create new variables ##
##############################################



# 1. Libraries & Data --------------------------------------
library(tidyverse)

# data
core_data <- read_csv("data/core_data.csv")

# 2. Create new variables ---------------------------------------------

clean_data <- 
  core_data %>%
  mutate( # create new variables
    # basic operations
    sb_scaled = sb*100,
    sb_ach = sb + ach,
    sb_divided = sb/2,
    sb_minus = sb - threat,
    
    # based on conditions
    threat_over_zero = 
      if_else(
        threat > 0, # condition
        1, # value if true
        0, # value if false
      ),
    
    threat_ach_over_zero = 
      if_else(
        threat > 0 | ach > 0, # condition
        1, # value if true
        0, # value if false
      ),
    
    ach_high = 
      if_else(
        ach > mean(ach, na.rm = TRUE),
        "high",
        "low"
      ),
    
    # use case_when to use multiple T or F
    # statements
    cases = 
      case_when(
        ach_high == "low" ~ "case1",
        ach_high == "high" & threat > 0 ~ "case2",
        TRUE ~ "case3"
      ),
    
    # use str_detect when you want to match
    # parts of a word or string to each row
    cases_2 = 
      case_when(
        str_detect(ach_high, "lo") ~ "case1",
        str_detect(ach_high, "hi") & threat > 0 ~ "case2",
        TRUE ~ "case3"
      )
    

  )

# check your results
clean_data %>% 
  select(ach, ach_high) %>% 
  head(10)

mean(clean_data$ach, na.rm = TRUE)

table(clean_data$cases)
table(clean_data$cases_2)

# 3. Select variables and filter rows -------------------------
## select variables sb, sb_scaled, sb_ach

sense_of_belonging_df <- 
  clean_data %>% 
  select(starts_with("sb"))

first_few_variables <- 
  clean_data %>% 
  select(sb:gender)

achievement_df <- 
  clean_data %>% 
  select(contains("ach"))
