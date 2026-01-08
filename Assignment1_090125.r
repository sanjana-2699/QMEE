library(tidyverse)
library(ggthemes)
library(readxl)
theme_set(theme_linedraw() %+replace%
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  text = element_text(face = "bold"),
                  legend.background = element_rect(size = 0.5, colour = "black"),
                  strip.background = element_rect(
                    color="black", fill="gray88", size = 0.5),
                  strip.text.x = element_text (color = "black"),
                  strip.text.y = element_text (color = "black")))

df1 <- read_excel("CS_data.xlsx")

# Getting the average of triplicate data
df_CS <- df1 %>%
  group_by(across(1:4)) %>%
  summarise(mean_CS = mean(CS_activity, na.rm = TRUE))