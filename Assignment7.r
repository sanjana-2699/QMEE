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

## BMB: log-link Gaussian is reasonable, but different from lm(log(var_measure) ~ ...) . In general I'd say the latter is preferred,
##  as it also handles likely patterns of heteroscedasticity
mod_RMR <- glm(var_measure ~ log(mass)
               , family = gaussian (link = log)
               , data = df_RMR)

summary(mod_RMR)

check_model(mod_RMR)
#The normality of residuals plot looks bad, but influential observations seem fine, linearity plots seem fine, I'm not sure I'm too worried about normality?
## BMB: I would check to see what the two extreme points are, but otherwise I agree

mod_plot <- ggplot(data = df_RMR, aes(x = mass, y = var_measure)) +
  geom_point() +
  geom_smooth(method="glm",
              formula = y ~ log(x),
              aes(group = 1),  ## BMB: add group=1 so we can add genotype colour in next plot
              method.args = list(family = gaussian(link = log))) +
  ## BMB: note that the axes on your plot are *not* logged, even though you're using log-log in your regression
  labs(x = "mass (mg)",
       y = "Routine Metabolic Rate (µmol/h)")

mod_plot
##shows a weak relationship between RMR and mass for this data? that seems weird. the effects on RMR are likely stronger from other experimental factors, this is an incomplete set of predictors

## BMB: reasonable. Seems weak even when we split the data set though ... ? Maybe mass scale isn't wide enough to matter?
mod_plot + facet_wrap(~AT*AS*TT, labeller = label_both) + aes(colour = genotype)
## ignore warnings, could restructure code to avoid them

## mark: 2
