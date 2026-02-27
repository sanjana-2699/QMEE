library(tidyverse)
library(ggthemes)
library(glmmTMB)
library(performance)
theme_set(theme_linedraw() %+replace%
            theme(text = element_text(face = "bold"),
                  legend.background = element_rect(linewidth = 0.5, colour = "black"),
                  strip.background = element_rect(
                    color="black", fill="gray88", linewidth = 0.5),
                  strip.text.x = element_text (color = "black"),
                  strip.text.y = element_text (color = "black")))

df1 <- readRDS("masterdoc_final.rds") #From Assignment 2a

df_RMR <- (df1
           |> subset(var_name == "RMR")
)

#GLM: routine metabolic rate ~ log(mass)
#this is a gaussian distribution - does not have a set maximum (not poisson), is a continuous variable (not binary, not binomial)

mod_RMR <- glm(var_measure ~ log(mass)
               , family = gaussian (link = log)
               , data = df_RMR)

summary(mod_RMR)

check_model(mod_RMR)
#The normality of residuals plot looks bad, but influential observations seem fine, linearity plots seem fine, I'm not sure I'm too worried about normality?

mod_plot <- ggplot(data = df_RMR, aes(x = mass, y = var_measure)) +
  geom_point() +
  geom_smooth(method="glm",
                      formula = y ~ log(x),
                      method.args = list(family = gaussian(link = log))) +
  labs(x = "mass (mg)",
       y = "Routine Metabolic Rate (umol/h)")
