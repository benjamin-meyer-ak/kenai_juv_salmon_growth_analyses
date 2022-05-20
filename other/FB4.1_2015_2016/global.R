source('model_function.R')
library(ggplot2)
library(tidyr)

remove_cols = c("Final_W", "eaten", "Ration_prey", "Ration", "p_value")

variable_column_list = list("Day"="Day", ### Variables by individuals
                                     "Temperature (C)"="Temperature.C",
                                     "Weight (g)"="Weight.g",
                                     "Net Production (g)"="Net.Production.g",
                                     "Net Production (J)"="Net.Production.J",
                                     "Cumulative Net Production (g)"="Cum.Net.Production.g",
                                     "Cumulative Net Production (J)"="Cum.Net.Production.J",
                                     "Gross Production (g)"="Gross.Production.g",
                                     "Gross Production (J)"="Gross.Production.J",
                                     "Cumulative Gross Production (g)"="Cum.Gross.Production.g",
                                     "Cumulative Gross Production (J)"="Cum.Gross.Production.J",
                                     "Specific Growth Rate (g/g/d)"="Specific.Growth.Rate.g.g.d",
                                     "Specific Growth Rate (J/g/d)"="Specific.Growth.Rate.J.g.d",
                                     "Specific Consumption Rate (g/g/d)"="Specific.Consumption.Rate.g.g.d",
                                     "Specific Consumption Rate (J/g/d)"="Specific.Consumption.Rate.J.g.d",
                                     "Consumption (g)"="Consumption.g",
                                     "Consumption (J)"="Consumption.J",
                                     "Cumulative Consumption (g)"="Cum.Cons.g",
                                     "Cumulative Consumption (J)"="Cum.Cons.J",
                                     #"Consumption of Prey (g)"=choicePreyGrams,
                                     #"Consumption of Prey (J)"=choicePreyJoules,
                                     "Specific Egestion Rate (J/g/d)"="Specific.Egestion.Rate.J.g.d",
                                     "Specific Excretion Rate (J/g/d)"="Specific.Excretion.Rate.J.g.d",
                                     "Specific Respiration Rate (J/g/d)"="Specific.Respiration.Rate.J.g.d",
                                     "Specific SDA Rate (J/g/d)"="Specific.SDA.Rate.J.g.d",
                                     "Initial Predator Energy Density (J/g)"="Initial.Predator.Energy.Density.J.g",
                                     "Final Predator Energy Density (J/g)"="Final.Predator.Energy.Density.J.g",
                                     "Mean Prey Energy Density (J/g)"="Mean.Prey.Energy.Density.J.g",
                                     "Gametic Production (g)"="Gametic.Production.g",
                                     "Gametic Production (J)"="Cum.Gametic.Production.J")

population_column_list = list(
  "Population Number"="Population.Number", ### Variables by population
  "Population Biomass (g)"="Population.Biomass.g",
  "Consumption by Population (g)"="Cons.Pop.g",
  "Consumption by Population (J)"="Cons.Pop.J",
  "Cumulative Consumption by Population (g)"="Cum.Cons.Pop.g",
  "Cumulative Consumption by Population (J)"="Cum.Cons.Pop.J",
  #"Population Consumption of Prey (g)"=choicePreyPopGrams,
  #"Population Consumption of Prey (J)"=choicePreyPopJoules,
  "Mortality"="Mortality.number",
  "Mortality Biomass (g)"="Mortality.g")

nutrient_column_list = list( 
  "Nitrogen Egestion (g)"="Nitrogen.Egestion.g", ### Nutrient regeneration variables 
  "Phosphorous Egestion (g)"="Phosphorous.Egestion.g",
  "N:P Egestion (mass ratio)"="N.to.P.Egestion",
  "Nitrogen Excretion (g)"="Nitrogen.Excretion.g",
  "Phosphorous Excretion (g)"="Phosphorous.Excretion.g",
  "N:P Excretion (mass ratio)"="N.to.P.Excretion",
  "Nitrogen Consumption (g)"="Nitrogen.Consumption.g",
  "Phosphorous Consumption (g)"="Phosphorous.Consumption.g",
  "N:P Consumption (mass ratio)"="N.to.P.Consumption",
  "Nitrogen Growth (g)"="Nitrogen.Growth.g",
  "Phosphorous Growth (g)"="Phosphorous.Growth.g",
  "N:P Growth (mass ratio)"="N.to.P.Growth")

contaminant_column_list = list(
  "Contaminant Clearance Rate (ug/d)"="Contaminant.Clearance.Rate.ug.d", ### Contaminant analysis variables
  "Contaminant Uptake (ug)"="Contaminant.Uptake.ug",
  "Contaminant Burden (ug)"="Contaminant.Burden.ug",
  "Contaminant Predator Concentration (ug/g)"="Contaminant.Predator.Concentration.ug.g")

full_column_list = c(variable_column_list,population_column_list,nutrient_column_list,contaminant_column_list)
length(full_column_list)

#df = read.csv('simulation_settings.csv')
#background_settings = df[,c(2:dim(df)[2])]
getwd()
input_path = 'inputs'
input_folder_list = list.dirs(input_path)
input_folder_list
folder_list = c()
folder = input_folder_list[5]
folder
input_file_list = c("Diet_prop.csv", "Indigestible_Prey.csv","Pred_E.csv", "Prey_E.csv","Temperature.csv") 
input_file_list
for(folder in input_folder_list){
  if(input_file_list %in% list.files(folder)){
  #if(length(list.files(folder)) >=5){
    folder_list = c(folder_list,folder)
  }
}
folder_list

# Fish Bioenergetics Model 4, version v1.0.3
FB4.version = "v1.0.3"  # This patch adds an FB4_Log_File to record input values used for this run;
#  some summary information is also added to this log file at the end of the run.
FB4.File_dir = getwd()  # Remember the file directory for this session

# Bioenergetics parameters, by species
parms <- read.csv("Parameters_official.csv",stringsAsFactors = FALSE) #  Read parameter values from .csv file

#   Main Input files
Temperature_File = "inputs/Main_Inputs/Temperature.csv" # Temperature (deg C), over time
Diet_prop_File   = "inputs/Main_Inputs/Diet_prop.csv"   # Diet proportions, by prey type, over time
Prey_E_File      = "inputs/Main_Inputs/Prey_E.csv"      # Energy density, by prey type, over time
Indigestible_Prey_File = "inputs/Main_Inputs/Indigestible_Prey.csv" # Fraction indigestible, by prey type, over time
Predator_E_File  = "inputs/Main_Inputs/Pred_E.csv"      # Predator energy density, over time; only read if PREDEDEQ == 1
#   Mortality
Mortality_File   = "Sub-Models/Mortality/Mortality.csv" # Mortality during time intervals
#   Reproduction
Reproduction_File= "Sub-Models/Reproduction/Reproduction.csv" # Day(s) and fraction wt lost spawning
#   Contaminants    # Only read if calc.contaminant == TRUE
Prey_conc_File       = "Sub-Models/Contaminant Accumulation/Contaminant Concentration.csv" # Contam conc in prey, by prey type, over time
Contam_assim_File    = "Sub-Models/Contaminant Accumulation/Contaminant Assimilation.csv" # Fraction of contaminant assimilated by predator, from each prey type, over time
Contam_trans_eff_File= "Sub-Models/Contaminant Accumulation/Transfer Efficiency.csv"  # Transfer efficiency of contam from prey to predator, by prey type, over time
#   Phosphorus      # Only read if calc.nut == TRUE
Phos_Ae_File        = "Sub-Models/Nutrient Regeneration/Phos_Ae.csv" # Predator's Phos assimilation efficiency, by prey type, over time
Phos_Conc_Pred_File = "Sub-Models/Nutrient Regeneration/Phos_Conc_Pred.csv" # Phos in predator (g Phos/g), over time
Phos_Conc_Prey_File = "Sub-Models/Nutrient Regeneration/Phos_Conc_Prey.csv" # Phos in prey (g Phos/g), by prey type, over time
#   Nitrogen        # Only read if calc.nut == TRUE
Nit_Ae_File         = "Sub-Models/Nutrient Regeneration/Nit_Ae.csv" # Predator's N assimilation efficiency, by prey type, over time
Nit_Conc_Pred_File  = "Sub-Models/Nutrient Regeneration/Nit_Conc_Pred.csv" # N in predator (g N/g), over time
Nit_Conc_Prey_File  = "Sub-Models/Nutrient Regeneration/Nit_Conc_Prey.csv" # N in prey (g N/g), by prey type, over time
#
#   Output file
FB4_Log_File        = "FB4_Log_File.csv" # File to save values used in this run, and summary output.

#choicesSpecies <- setNames(1:nrow(parms),parms$Species ### Allows the consumption output to be broken down by species for individual fish
choicesSpecies <- parms$Species ### Allows the consumption output to be broken down by species for individual fish
