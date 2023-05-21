### Core ideas for efficient data analysis in R ##
## Analyzing data is like lazy shopping. 
## You need to clean up your mess to make results usable.

## Diego Catalan Molina, May 2023

# 1. Libraries & Data --------------------------------------
library(tidyverse)
library(broom) # to tidy up your results
library(flextable) # pretty tables

# data
core_data <- read_csv("data/core_data.csv")

# 2. Analysis ----------------------------------------------

## We will test the association between stereotype threat and sense of belonging at school,
## controlling for whether students were Black or Latinx.

## We will use a linear regression, lm(), which is one of the most common 
## analysis functions in R and analysis models in the real world.

model_belonging <- # fit the model and store in object model_belonging. This is your "shopping cart"
  lm(
    sb ~ 1 + threat + blk_ltn, # formula
    core_data # data
  )

# 3. Explore the results list -------------------------------

summary(model_belonging) # print output

model_belonging$coefficients[2]

model_belonging["coefficients"]

# 4. Create a table -----------------------------------------

ugly_table <- 
  model_belonging %>% 
  tidy(.) %>% # function to get basic table
  modify_if(is.numeric, ~round(.,2)) # round numbers if column is a number/double 

ugly_table %>% 
  flextable(.) # gives us a better-looking version of the table

