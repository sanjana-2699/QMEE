library(tidyverse)
library(ggthemes)
library(readxl)
library(glmmTMB)
library(performance)
library(car)

df1 <- read_excel("CS_data.xlsx")

# Getting the average of triplicate data
df_CS <- df1 %>%
  group_by(across(1:4)) %>%
  summarise(mean_CS = mean(CS_activity, na.rm = TRUE))

df_CS[,c(1:4)]<- lapply(df_CS[,c(1:4)], as.factor)

# Linear mixed model
mod1 <- glmmTMB(mean_CS ~ (AT + AS + TT)^3 + (1|genotype), data = df_CS)
check_model(mod1)
summary(mod1)

# Run ANOVA
Anova(mod1, type = "II")
