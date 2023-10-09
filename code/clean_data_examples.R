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
## select variables ----

sense_of_belonging_df <- 
  clean_data %>% 
  select(starts_with("sb"))

first_few_variables <- 
  clean_data %>% 
  select(sb:gender)

achievement_df <- 
  clean_data %>% 
  select(contains("ach"))

## filter rows ----
### only engaged
engaged_df <- 
  core_data %>% 
  filter(engaged == 1)

# check the result
table(engaged_df$engaged)

### only average
avg_df <- 
  core_data %>% 
  filter(standards == "Average")

# check the result
table(avg_df$standards)


### not engaged
not_engaged_df <- 
  core_data %>% 
  filter(engaged != 1)

table(not_engaged_df$engaged)

### not average
not_avg_df <- 
  core_data %>% 
  filter(standards != "Average")

table(not_avg_df$standards)

### two conditions
engaged_and_not_avg <- 
  core_data %>% 
  filter(engaged == 1 & standards != "Average")

table(engaged_and_not_avg$engaged, 
      engaged_and_not_avg$standards)

### detect multiple values in a vector
specific_values <- 
  core_data %>% 
  filter(stgy %in% c(3,4,5))

table(specific_values$stgy)

### detect a string
non_minority_df <- 
  core_data %>% 
  filter(str_detect(minority, "non"))

table(non_minority_df$minority)

minority_df <- 
  core_data %>% 
  filter(!str_detect(minority, "non"))

table(minority_df$minority)

# 4. Modify variables -----------------------------

## modify variables that satisfy a condition
clean_data %>% 
  modify_if(
    is.numeric,
    ~.x*1000
  )

## only modify numeric variables whose mean is above zero
clean_data %>% 
  modify_if(
    ~is.numeric(.x) && mean(.x, na.rm = T) > 0,
    ~.x*1000
  )

# 5. Aggregate data -----------------------------------

## one grouping variable
core_data %>% 
  group_by(minority) %>% 
  summarise(
    m_threat = mean(threat, na.rm = TRUE)
  )

## two grouping variables
core_data %>% 
  group_by(minority, gender) %>% 
  summarise(
    m_threat = mean(threat, na.rm = TRUE)
  )
