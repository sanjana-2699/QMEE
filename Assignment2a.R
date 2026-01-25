library(tidyverse)
library(ggthemes)
theme_set(theme_linedraw() %+replace%
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  text = element_text(face = "bold"),
                  legend.background = element_rect(size = 0.5, colour = "black"),
                  strip.background = element_rect(
                    color="black", fill="gray88", size = 0.5),
                  strip.text.x = element_text (color = "black"),
                  strip.text.y = element_text (color = "black")))

masterdoc <- read.csv("TxS_masterfile.csv")

#check structure of variables
str(masterdoc)

#change datatypes of certain variables to factors 
masterdoc[,c("AT", "AS", "TT", "day", "plate", "genotype")] <- lapply(masterdoc[,c("AT", "AS", "TT", "day", "plate", "genotype")], 
                                                          as.factor)
#check that it worked
str(masterdoc)

#plot number of entries per variable measured to check if data has been inputted correctly
ggplot(data = masterdoc, aes(x = var_name)) +
  geom_histogram(stat = "count") +
  ylab("name of the variable meausred") + 
  xlab("number of entries") 

#plot makes sense, CS values were not averaged before entering data into masterdoc, so there is a higher count

#subset values for one particular measure (CS) 
df_CS <- (masterdoc
          |> subset(var_name == "CS")
)

#check if there are values other than "CS" in var_name
print(unique(df_CS$var_name))

#CS measures are in triplicates -> get average values for each genotype at each test condition instead
#code adapted from class today
df_CS_avg <- (df_CS
  |> summarise(across(c(BSA, mass, var_measure),
                      mean),
               .by = c(AT, AS, TT, genotype, day, plate, var_name, unit))
)

#helpful if i got rid of the above averaging step and compiled final results back in the masterfile
masterdoc_final <- (masterdoc
  |> subset (var_name != "CS")
  |> rbind(df_CS_avg)
)

#plot to check that things worked:
#RMR and CS should have equal counts (because of two test temperatures per treatment group)
#PCC and TBARS should have equal counts (only one test temperature per treatment group)
ggplot(data = masterdoc_final, aes(x = var_name)) +
  geom_histogram(stat = "count") +
  ylab("name of the variable meausred") +
  xlab("number of entries") 

#plot makes sense!

#check structure of variables
str(masterdoc_final)

#save as .rds file
saveRDS(masterdoc_final, file = "masterdoc_final.rds")
