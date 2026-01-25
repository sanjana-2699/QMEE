library(tidyverse)
library(glmmTMB)
library(performance)
library(emmeans)
library(car)

options(contrasts = c("contr.sum", "contr.poly"), digits = 3)

#read rds file
df1 <- readRDS("masterdoc_final.rds")

#subset data for one var_name (CS) from the masterfile
df_CS <- (df1
        |> subset (var_name == "CS")
)

#model
mod1 <- glmmTMB(var_measure ~ (AT + AS + TT)^3 + (1|genotype), data = df_CS)
check_model(mod1)
summary(mod1)

# Run ANOVA
Anova(mod1, type = "II")
