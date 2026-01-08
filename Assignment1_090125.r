library(tidyverse)
library(ggthemes)
library(readxl)

# Data used is under the branch 'CS_data'
df1 <- read_excel("CS_data.xlsx") 

# Getting the average of triplicate data
df_CS <- df1 %>%
  group_by(across(1:4)) %>%
  summarise(mean_CS = mean(CS_activity, na.rm = TRUE))
