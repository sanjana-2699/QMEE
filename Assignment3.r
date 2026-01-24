library(tidyverse)
theme_set(theme_linedraw() %+replace%
            theme(text = element_text(face = "bold"),
                  strip.background = element_rect(
                    color="black", fill="gray88", size = 0.5),
                  strip.text.x = element_text (color = "black"),
                  strip.text.y = element_text (color = "black"))
) 
#I have chosen to include gridlines in this script. these plots are all exploratory with minimal inferences.
#because I'm plotting raw data, the gridlines help to illustrate differences.

## BMB: hmm, I haven't see %+replace% before ...
## I get a warning (with ggplot 4.0.1) that you should use
## linewidth = 0.5 rather than size = 0.5 ...

set.seed(9) #for jitter

df1 <- readRDS("masterdoc_final.rds")

#1. Plot to look at the citrate synthase (CS) data for outliers within genotypes

#subset data that I want to use
df_CS <- (df1
          |> subset(var_name == "CS")
)

#plot
CS_boxplot <- ggplot(data = df_CS, aes(x = genotype, y = var_measure)) +
  geom_boxplot(outlier.shape = NA) +
  labs(x = "genotype ID",
       y = "CS activity (U/mg)") +
  ## geom_point(size = 3, 
  ##            alpha = 0.3, 
  ##            position = position_jitterdodge())
  ## BMB: geom_jitter() is a shortcut for geom_point + position
  ## I strongly recommend *not* jittering in the y-direction ...
  ## (or geom_beeswarm) -- could also make x-jitter smaller (via width= arg)
  geom_jitter(size = 3, alpha = 0.3, height = 0)

print (CS_boxplot)

#There seems to be 4 points that are potential outliers. Across the 8 genotypes, CS activity seems to be comparable and within range otherwise.

#A box plot is useful to get an idea of ranges. I kept the median line in the box plot so it's easier to compare between genotypes (this allows for comparison of positions across a common scale).
#I did consider a dot and line graph instead, but I did not want to compare or look at means/standard errors.
#The raw data points are shown as slightly transparent circles.
#The 'outlier.shape = NA' argument was used because there was an additional data point that was shown from the box plot's outlier calculations, and this overlapped with the raw data points.
## BMB: good.

#2. Allometric relationship between mass and citrate synthase activity

#plot

plot_CS <- ggplot(data = df_CS, aes(x = mass, y = var_measure)) +
  geom_point() +
  labs( x = "mass (mg)",
        y = "CS activity (U/mg)") +
  geom_smooth()

print(plot_CS)

#log transforming data to draw out differences between datapoints better
plot_CS_log_transform <- ggplot(data = df_CS, (aes(x = log(mass), y = log(var_measure)))) +
  geom_point() +
  labs( x = "log mass (mg)",
        y = "log CS activity (U/mg)") +
  geom_smooth()

print(plot_CS_log_transform)

## BMB: *strongly* preferrable to use scale_*_log10
## (same graph, axes are much easier to interpret)
plot_CS + scale_x_log10() + scale_y_log10() +
  ## can also add a linear fit (alpha makes ribbon lighter)
  geom_smooth(method = "lm", colour = "red", fill = "red", alpha = 0.2)


#a log transform seems to show a "more linear" relationship - this is useful to know for modelling the data going forward.

#a scatter plot was the most useful for showing the relationship between two variables. 
#I have added a 'geom_smooth' to better depict the "linearity" to the reader. 
#the range of citrate synthase values can also be seen if needed, but this is not inherently clear from the graph. 

#3. Relationship between RMR and CS activity among acclimation groups: 
#CS is a marker of mitochondrial content. However, CS values obtained from assays usually tell us the 'maximal' CS activity in the tissue used. 
#A way of putting CS activity into context is by relating it to the metabolic rate, which is largely reflective of how/where CS is being used.
#Results seen in this graph will not change any planned analyses.

#subset and arrange data from masterfile
## BMB: any reason you're not doing this (especially renaming) upstream?
df_CS_RMR <- (df1
              |> filter(var_name == "CS")
              |> select (var_measure)
              |> bind_cols(df1 
                           |>filter(var_name == "RMR")
                           |> select(var_measure,AT, AS))
              |> na.omit()
              |> rename(CS = 1, RMR = 2, AS = 3, AT = 4)
) 
##using pivot_longer is probably more efficient and what I should have used, but this was easier (BMB: OK)

## BMB: see comment about log scaling above
plot_CS_RMR <- ggplot(data = df_CS_RMR, aes(x = log(CS), y = log(RMR))) +
  geom_point() +
  geom_smooth() +
  labs(x = "log CS activity (U/mg)",
       y = "log RMR (µmol/h)") 

print(plot_CS_RMR)

#variables are log transformed to separate out distances between data points better.
#a scatterplot is the best way to evaluate the relationship between two variables.
#'geom_smooth" was added to illustrate the relationship.
#I have not changed the geom_point settings in any way, the default looks clear enough.

#renaming some factors with units for the graph. 
#I don't know how to make this easier - I am not able to change the titles of the facets in ggplot directly instead of modifying the data frame.
## BMB: it may be possible but changing the data frame is OK
## BMB I would do levels(df_CS_RMR$AS) <- paste(levels(df_CS_RMR$AS, "ppt"))
##  etc.
df_CS_RMR$AS <- gsub("15", "15 ppt", df_CS_RMR$AS)
df_CS_RMR$AS <- gsub("30", "30 ppt", df_CS_RMR$AS)
df_CS_RMR$AT <- gsub("24", "24C", df_CS_RMR$AT)
df_CS_RMR$AT <- gsub("16", "16C", df_CS_RMR$AT)

#I want to see if there are any differences in this relationship between acclimation groups, so I am faceting by the temperature and salinity treatments.
plot_CS_RMR_by_acc_group <- ggplot(data = df_CS_RMR, aes(x = log(CS), y = log(RMR))) +
  facet_grid (rows = vars(AS), cols = vars(AT)) + 
  geom_point() +
  geom_smooth() +
  labs(x = "log CS activity (U/mg)",
       y = "log RMR (umol/h)") +
  theme(strip.text.y = element_text(angle = 270))

print(plot_CS_RMR_by_acc_group)

## BMB: how about this?
ggplot(data = df_CS_RMR, aes(x = CS, y = RMR,
                             colour = interaction(AS, AT),
                             fill = interaction(AS, AT)
                             )) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_colour_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  scale_x_log10() +
  scale_y_log10()

#The differences in slopes are not very apparent to me (Cleveland's hierarchy - angle/slope is 4th), so I am going to plot the slopes separately.
df_CS_RMR_slopes <- (df_CS_RMR
                     |> group_by(AT,AS)
                     |> na.omit()
                     |> summarise(vars(c(CS, RMR)), 
                                  slope = coef(lm(RMR ~ CS))[[2]])
)

df_CS_RMR_slopes <- df_CS_RMR_slopes [,-3] #removing additional column

treatment_groups <- c("LSLT", "LSHT", "HSLT", "HSHT") #low salinity - LS, high salinity - HS, low temperature is LT, high temperature is HT
cbind(df_CS_RMR_slopes, treatment_groups)

plot_CS_RMR_slope <- ggplot(data = df_CS_RMR_slopes, aes(x = treatment_groups, y = slope)) +
  geom_point(size = 3) +
  labs (x = "acclimation group",
        y = "slope of RMR ~ CS") 

print (plot_CS_RMR_slope) 
## BMB: this is OK but you should *definitely* include confidence intervals

df_regr <-df_CS_RMR |>
  split(list(df_CS_RMR$AT, df_CS_RMR$AS)) |>
  purrr::map(~ broom::tidy(lm(RMR ~ CS, data = .),
                           conf.int = TRUE)) |>
  bind_rows(.id = "condition") |>
  select(condition, term, estimate, lwr = conf.low, upr = conf.high) |>
  filter(term == "CS")

ggplot(df_regr, aes(condition, estimate)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr))
## mark: 2.2
