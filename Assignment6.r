library(tidyverse)
library(ggthemes)
library(glmmTMB)
library(performance)
library(emmeans)
library(car)
theme_set(theme_linedraw() %+replace%
            theme(text = element_text(face = "bold"),
                  legend.background = element_rect(linewidth = 0.5, colour = "black"),
                  strip.background = element_rect(
                    color="black", fill="gray88", linewidth = 0.5),
                  strip.text.x = element_text (color = "black"),
                  strip.text.y = element_text (color = "black")))

options(contrasts = c("contr.sum", "contr.poly"), digits = 3)

df1 <- readRDS("masterdoc_final.rds")

df_RMR <- (df1
           |> subset(var_name == "RMR")
)

df_RMR[,c(1:4)]<- lapply(df_RMR[,c(1:4)], as.factor)

##Hypothesis: In response to thermal and saline stress (both individual and interactive effects), metabolic compensation is seen in N. vectensis as an acclimation response.
##All possible terms in the below model are inluded because of known effects of temperature and salinity alone on metabolic rate - we are hoping to see interesting trends in these effects

mod_RMR <- glmmTMB(log(var_measure) ~ ((AT + AS + TT)^3) + log(mass) + (1|genotype), data = df_RMR)

mod_check_plots <- plot(check_model(mod_RMR, panel = FALSE))
mod_check_plots[[1]] ##prediction plot

##The predicted data seems to trend similarly with the observed data points. It seems like the model-predicted values around the peak (median?) seem to be lower than the observed data points, but it doesn't seem too concerning overall

check_model(mod_RMR) ##these are all diagnostic plots that I have been looking at before proceeding

##The homogeneity of variance plot (that checks for homoscedasticity) is a little concerning. 
#I'm honestly not sure how to fix it (or how troubling this is). It gets worse if I don't log transform the data, but that makes sense - log of metabolic rate has a more linear relationship with the log of mass; I don't necessarily want to remove the log transform.

summary(mod_RMR)

Anova(mod_RMR, type = "II")

em_RMR <- emmeans(mod_RMR, ~ TT|AS + AT)
print(em_RMR)

em_df_RMR <- as.data.frame(em_RMR)
levels(em_df_RMR$AS) <- paste ((levels(em_df_RMR$AS)), "ppt")

em_plot_RMR <- ggplot(data = em_df_RMR, aes(x = TT, y = emmean, group = AT, colour = AT)) +
  facet_wrap(~AS, nrow = 1) +
  geom_point(position = position_dodge(width = 0.2), size = 3) +
  geom_line(position = position_dodge(width = 0.2), linewidth = 0.75) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2,
                position = position_dodge(width = 0.2), linewidth = 0.75) +
  ylab("log RMR (umol/h)") + xlab("Test temperature") +
  scale_x_discrete (labels = c("16C","24C")) +
  scale_color_manual(labels = c("16C","24C"),
                     values = c("deepskyblue3","firebrick3")) 

print (em_plot_RMR)

