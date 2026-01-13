## BMB: don't need to date your assignment file, version control (Github)
##  should handle that for you)
library(tidyverse)
library(ggthemes) ## BMB: do we need this?
library(readxl)
library(glmmTMB)
library(performance)
library(car)

df1 <- read_excel("CS_data.xlsx")

# Getting the average of triplicate data
df_CS <- df1 %>%
    ## this is OK but maybe better to list the categories explicitly?
    ## (e.g. what if column order changes?)
    group_by(across(1:4)) %>%
    summarise(mean_CS = mean(CS_activity, na.rm = TRUE))

df_CS[,c(1:4)]<- lapply(df_CS[,c(1:4)], as.factor)

## equivalent:
(df1
    |> summarise(mean_CS = mean(CS_activity, na.rm = TRUE), .by = 1:4)
    |> mutate(across(1:4, as.factor))
)

# Linear mixed model
mod1 <- glmmTMB(mean_CS ~ (AT + AS + TT)^3 + (1|genotype), data = df_CS)
check_model(mod1)
summary(mod1)

# Run ANOVA
Anova(mod1, type = "II")

## mark: 2
