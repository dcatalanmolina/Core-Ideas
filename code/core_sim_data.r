### Core ideas for efficient data analysis in R ##
## Generating fake dataset

## Diego Catalan Molina, Mar 2023

# 1. Libraries & Data --------------------------------------
library(tidyverse)
library(MASS)

# simulate data: https://www.r-bloggers.com/2021/05/how-to-generate-correlated-data-in-r/
set.seed(12)

# simulating N(0,1) variables with different degrees of correlation.
sim_data <- 
  as_tibble(
    mvrnorm(n = 500, mu = c(0, 0, 0), 
            Sigma = 
              rbind(
                c(1, -.50, .40), # sb
                c(-.50, 1, -.80), # stereotype threat
                c(.40, -.80, 1) # achievement
              ))
  )

sim_dat_cat <- 
  sim_data %>% 
  rename(
    sb = V1, threat = V2, ach = V3
  ) %>% 
  mutate(
    minority = 
      if_else(
        sb < -.05 & threat > .05,
        # value if true
        sample(
          c("non-minority","minority"), #values
          n(), #n
          replace = TRUE,
          prob = c(.25, .75)
          ),
        # value if false
        sample(
          c("non-minority","minority"), #values
          n(), #n
          replace = TRUE,
          prob = c(.75, .25)
        )
      ),
    gender = # female, male, non-binary
      case_when(
        minority == "non-minority" & threat < 0 ~ # non-minority, low threat
          sample(
            c("female","male","non-binary"), #values
            n(), #n
            replace = TRUE,
            prob = c(.15, .70, .15)
          ),
         minority == "minority" & threat < 0 ~ # minority, low-threat
           sample(
             c("female","male","non-binary"), #values
             n(), #n
             replace = TRUE,
             prob = c(.25, .50, .25)
           ),
        TRUE ~ # high threat
          sample(
            c("female","male","non-binary"), #values
            n(), #n
            replace = TRUE,
            prob = c(.45, .10, .45)
          )
        ),
    standards = # Categorical outcome
      case_when(
        ach > 1 ~ "High",
        ach < -1 ~ "Low",
        TRUE ~ "Average"
      ),
    
    engaged = # binary outcome
      if_else(
        ach > 0,
        # condition if true
        sample(
          c(0,1),
          n(),
          replace = TRUE,
          prob = c(.25, .75)
        ),
        # condition if false
        sample(
          c(0,1),
          n(),
          replace = TRUE,
          prob = c(.75, .25)
        )
      ),
    
    stgy = #likert item
      if_else(
        ach > 0,
        # condition if true
        sample(
          c(1,2,3,4,5),
          n(),
          replace = TRUE,
          prob = c(.10,.15,.20, .25,.30)
        ),
        # condition if false
        sample(
          c(1,2,3,4,5),
          n(),
          replace = TRUE,
          prob = c(.30, .25, .20, .15, .10)
        )
        
      )
  ) %>% 
  modify_at(
    .at = "standards",
    ~factor(., levels = c("Low", "Average", "High"))
  )

# 2. Check simulated data -----------------------------------

summary(sim_data)

cor(sim_data)

summary(sim_dat_cat)

cor(sim_dat_cat)

sim_dat_cat %>% filter(minority == "minority") %>% summary(.)
sim_dat_cat %>% filter(minority != "minority") %>% summary(.)

# 3. Save sim data -----------------------------------------

write_csv(sim_dat_cat, "data/core_data.csv")

