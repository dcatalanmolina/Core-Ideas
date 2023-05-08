### Core ideas for efficient data analysis in R ##
## 2: Plotting data is like layering a cake

## Diego Catalan Molina, March 2023

# 1. Libraries & Data --------------------------------------
library(tidyverse)

# data
core_data <- read_csv("data/core_data.csv")
  

# 2. Plots -------------------------------------------------

# Basic layers
core_data %>% # pipe operator (take what's on the left and do what's next with it)
  ggplot(
    aes(y = sb, x = ach) #mapping
    ) + # non-data layers are assembled using plus sign
  geom_point() # geometry
  

# Basic layers, cleaner
core_data %>% 
  ggplot(
    aes(y = sb, x = ach) 
  ) +
  geom_point() +
  theme_classic() +
  labs(y = "Sense of Belonging", x = "Test Scores")




## Adding colors ------------------------------------------
core_data %>% 
  ggplot(
    aes(y = sb, x = ach, color = minority) #adding colors
  ) +
  geom_point() +
  theme_classic() +
  labs(y = "Sense of Belonging", x = "Test Scores")

## Set custom colors ----------------------------------------------

core_data %>% 
  ggplot(
    aes(y = sb, x = ach, color = minority)
  ) +
  geom_point() +
  scale_color_manual(values = c("#022851", "#FFBF00")) + #custom colors, Hex codes
  theme_classic() +
  labs(y = "Sense of Belonging", x = "Test Scores")

# If you don't use Hex color codes, google "Hex colors" and you'll find
# lots of resources to identify the right colors for you.



## Clean the legend -------------------------------------------------

core_data %>% 
  ggplot(
    aes(y = sb, x = ach, color = minority)
  ) +
  geom_point() +
  scale_color_manual(values = c("#022851", "#FFBF00")) +
  theme_classic() +
  theme(
    legend.position = c(.90, .20) # horizontal and vertical coordinates
  ) +
  labs(y = "Sense of Belonging", x = "Test Scores", color = "") # no legend title



## Save plot as file in your computer ----------------------------------
final_plot <- # name for the plot object
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


ggsave(
  "plots/final_plot.png", # path, location where you want to save the plot
  final_plot, # plot you want to save
  width = 8, height = 5
)
  


