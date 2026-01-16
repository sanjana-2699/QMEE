# Exploring mechanisms of acclimation responses to thermal and saline stress in the cnidarian, _Nematostella vectensis_ 

## Background and study design: 
Starlet sea anemones (_Nematostella vectensis_) are cnidarians endemic to estuarine environments along the Atlantic coast of North America. Across this broad latitudinal range, natural growth habitats vary drastically, particularly in terms of temperature, salinity, reactive oxidative species (ROS), and pollution. Using anemones collected from New Hampshire, USA, we aimed to explore interactions between thermal and saline stress, to determine their capacity for plasticity, and identify underlying signaling pathways. We acclimated anemones using a two-factor design — warm and cold treatments at 16°C or 24°C, and high and low salinity treatments at 30‰ or 15‰ ASW. Leveraging the clonal nature of these anemones, 4 clones from 8 genotypes were used for the 4 treatment groups (i.e., genotype represents a random effect). After an acclimation period of 2 weeks, routine metabolic rates (RMRs), enzyme activities of citrate synthase (CS) which is a marker of mitochondrial activity, and the osmoregulatory activities of ATPases including Na+-K+ ATPase (NKA) were measured acutely - both at their acclimation temperature, and at the orthogonal temperature. Further, two markers of ROS damage, namely protein carbonyl content (PCC) and thiobarbituric acid reactive substances (TBARS) were also measured at the respective acclimation temperatures (these are endpoint measures). 

## Predictions and goals:
When anemones are exposed to high temperatures, I predict that metabolic rate will increase as a result of thermodynamic effects on biochemical reaction rates. Any kind of saline stress in these circumstances, will initiate the activity of osmoregulatory pathways, which are ATP expensive. This will augment a classic case where energy demands outweigh supply. We will also expect an increased ROS production as a result of both stressors.
In contrast, at lower temperatures, I predict there will be reduced rates of oxidative phosphorylation and thereby reduced ATP availability. This may represent a problem for the ATP expensive osmoregulatory pathways when salinity also fluctuates. In this situation, I predict a case where energy supply is unable to meet demand. Similar to cases at high temperatures, I also expect increased ROS production.

This study is exploratory and is meant to understand/establish stress response mechanisms in this study system. Using these measures, I want to understand how each of these variables are modified in response to either or both of these stressors. 

## Data description: 
From class, I've learned that it is better practice to have a document for a project and use this for analysis and just general data storage. The updated datafile, "TxS_masterfile.csv" contains all the data that has been collected so far. This file includes columns on the acclimation and test conditions for each data point - acclimation temperature ('AT') in degrees Celcius, acclimation salinity ('AS'), in parts per thousand (ppt), and test temperature ('TT') in degrees Celcius. 

The column 'genotype' serves as an ID for which genotype the data point was collected from.

The column 'BSA' refers to the protein content in the sample used, expressed in mg/mL. Protein content was estimated using the Bradford Assay. The final enzyme activities are standardised using the protein content.

The column 'mass' is the mass of tissue used in each assay in mg.

The column 'var_name' refers to the name of the variable measured. This can be RMR, CS, PCC, or TBARS (so far). 

The column 'var_measure' refers to the value of the variable measured. 

The column 'units' refers to the units of the variable measured. 

Finally, the columns 'day' and 'plate' are variables specific to the RMR measures. As the names suggest, these variables refer to the day that the experiment was run on and the plate ID of the microplate used.

Note: The CS activity measures are in triplicates, so there are three separate values for the same genotype in each acclimation group at each test temperature.
