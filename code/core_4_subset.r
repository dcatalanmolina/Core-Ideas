### Core ideas for efficient data analysis in R ##
## Your object is a box

## Diego Catalan Molina, May 2023

# 1. Libraries & Data --------------------------------------
library(tidyverse)

# data
core_data <- read_csv("data/core_data.csv")

# 2. Subset a variable ------------------------------------
  # two ways of subsetting a variable
  # First, use $. Then, use []

core_data

core_data[,"threat"] 
  # within the square brackets, the first dimension refers to rows
  # and the second dimension refers to columns

# 3. Subset a row ----------------------------------------
core_data[31,]

# 3. Subset a value ---------------------------------------

core_data$threat[2]

core_data[2,"threat"] 

  # notice the differences
  # $ gets you a single value
  # whereas [ ] gets you a 1 x 1 data frame



# 4. Explore a list -------------------------------------

achievement_plot <-
  core_data %>% 
  ggplot(
    aes(y = sb, x = ach, color = minority)
  ) +
  geom_point() +
  scale_color_manual(values = c("#022851", "#FFBF00")) +
  theme_classic() +
  theme(
    legend.position = c(.85, .15)
  ) +
  labs(y = "Sense of Belonging", x = "Test Scores", color = "")

# use $ first
achievement_plot$labels

# you can also use double square brackets and the 
# name of the object you want to subset
achievement_plot["labels"]
