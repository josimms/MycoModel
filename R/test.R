tests <- function() {
  ###
  # Parameters
  ###
  microbe_turnover = 0.016906 # Preeven 2013, Symphony model paper, rate of microbes going to SOM
  NC_in_root_opt = (0.001*2875)/1881 # Helmisaari 1995: Nutroent cycling in Pinus sylvestris stands in eastern Finland, belowground component
  NC_microbe_opt = 28.73
  # Average C/N ratio is 28.73, 1.28% N molecular content - Hyytälä average - Heinonsalo, 2015
  # The stable SOM C:N ratio is about 1/6, if it is assumed that proteins are the main cause - Adamczyk, 2019
  # Högberg 2017, the C/N ratio of microorganisms in the F + H layer is around 5
  NC_fungal_opt = 0.025 # Meyer 2010, CN ratio of 40 fungal
  percentage_C_biomass = 0.5 # Used in CASSIA
  N_limits_plant = c(10, 10, 10) # Arbitrarty - Bayesian Callibration
  N_limits_fungal = c(10, 10, 10) # Arbitrarty - Bayesian Callibration
  N_limits_microbes = c(10, 10, 10) # Arbitrarty - Bayesian Callibration
  N_k_plant = c(0.2, 0.2, 0.2) # Arbitrarty - Bayesian Callibration
  N_k_fungal = c(0.2, 0.2, 0.2) # Arbitrarty - Bayesian Callibration
  N_k_microbes = c(0.2, 0.2, 0.2) # Arbitrarty - Bayesian Callibration
  SWC_k_plant = c(0.5, 0.5, 0.5) # Arbitrarty - Bayesian Callibration
  SWC_k_fungal = c(0.5, 0.5, 0.5) # Arbitrarty - Bayesian Callibration
  SWC_k_microbes = c(0.5, 0.5, 0.5) # Arbitrarty - Bayesian Callibration
  NH4_on_NO3 = c(10, 0.2, 0.5) # Arbitrarty - Bayesian Callibration
  Rm = 0.00958
  Q10 = 2.5575
  optimal_root_fungal_biomass_ratio = 0.9 # TODO: Heinonsalo
  turnover_mantle = 1/625 # Meyer, 2010 ERM turnover ranges between 10 days to 100 days, while mantle turnover time is 625 days
  turnover_ERM = 1/50
  turnover_roots = 1/365 # Meyer, 2010 Non-mycorrhizal roots have a turnover (dr_r) of 365 days TODO: CASSIA contrast
  turnover_roots_mycorrhized = 1/625 # Meyer, 2010
  growth_C = 0.2 # Arbitrary - TODO
  growth_N = 0.2 # Arbitrary - TODO
  C_value_param_myco = 4 # Arbitrary - Bayesian Calibration
  N_value_param_myco = 4 # Arbitrary- Bayesian Calibration
  C_value_param_plant = 4 # Arbitrary- Bayesian Calibration
  N_value_param_plant = 4 # Arbitrary- Bayesian Calibration
  Cmic_Corg = 0.5 # TODO: find this value from hyytiälä
  
  ###
  # State variables
  ###
  
  Temp = 15
  SoilWater = 0.153 # Sensible value from Hyytiälä tracker data
  
  C_roots = 2.8/2 # CASSIA value from the starch and sugar 15_16 file
  N_roots = C_roots * NC_in_root_opt # As initial condition assume that the values are at an optimum
  C_fungal = C_roots * optimal_root_fungal_biomass_ratio
  N_fungal = C_fungal * NC_fungal_opt
  Norg_FOM = 1500 # TODO: this should be an input, make sure that the Norg and Norg_FOM are used consistently
  micorization = N_roots / (C_roots * optimal_root_fungal_biomass_ratio)
  NH4 = 0.31 # kg N ha−1 Korhonen, 2013 
  NO3 = 0.002 # kg N ha−1 Korhonen, 2013
  Norg = 26.5 # kg N ha−1 Korhonen, 2013
  C_SOM = 5.6 # kg m-2 C in 40cm hummus layer measured 2003 - Hyytiälä site desciption
  C_microbe = C_SOM * Cmic_Corg
  N_microbe = C_microbe * NC_microbe_opt
  C_fungal_in = C_SOM # This is just for the graphs, in the model the value should be caluclated by uptake
  N_fungal_in = NH4 + NO3 + Norg # This is just for the graphs, in the model the value should be calculated by uptake
  mantle_mass = 0.5*C_roots # TODO: what values should these have?
  ERM_mass = 0.5*C_roots
  
  parameters_R = c(microbe_turnover, NC_in_root_opt, NC_fungal_opt, NC_microbe_opt, percentage_C_biomass, 
                   N_limits_plant, N_limits_fungal, N_limits_microbes,
                   N_k_plant, N_k_fungal, N_k_microbes, 
                   SWC_k_plant, SWC_k_fungal, SWC_k_microbes, NH4_on_NO3,
                   c(Rm, Q10, Rm, Q10, Rm, Q10), 
                   optimal_root_fungal_biomass_ratio, turnover_mantle, turnover_ERM, turnover_roots, turnover_roots_mycorrhized,
                   growth_C, growth_N, C_value_param_myco, N_value_param_myco, C_value_param_plant, N_value_param_plant)

  
  ###
  # Check that the outputs are providing the right behaviour
  ### 
  # TODO: make the graph titles match the input variables
  
  respiration_graphs(Rm, Q10)
  
  ## NH4 version
  nitrogen_graphs(NH4, Temp, N_limits_plant[1], N_k_plant[1], SoilWater, SWC_k_plant[1])
  
  decision_graphs(C_roots, N_roots, C_fungal, N_fungal, 0.5, 0.5, 0.5, 0.5*C_fungal, 0.5*C_fungal,
                  C_value_param_plant, N_value_param_plant, C_value_param_myco, N_value_param_myco)
  
  myco_growth_graphs(C_fungal_in, N_fungal_in, growth_C, growth_N)
  
  nitrogen_graphs_plant(NC_in_root_opt, Temp, SoilWater, micorization, NH4, NO3, Norg, 
                        N_limits_plant, N_k_plant, SWC_k_plant, C_roots, N_roots, C_fungal, 
                        percentage_C_biomass, c(Rm, Q10), C_value_param_plant, N_value_param_plant)
  nitrogen_graphs_fungal(C_fungal, N_fungal, NC_fungal_opt, mantle_mass, ERM_mass, percentage_C_biomass, Temp, SoilWater,
                         NH4, NO3, Norg, N_limits_fungal, N_k_fungal, SWC_k_fungal, C_value_param_myco, N_value_param_myco)
  nitrogen_graphs_microbe(C_microbe, N_microbe, NC_microbe_opt, NH4, NO3, Norg, 
                          Temp, SoilWater, N_limits_microbes, N_k_microbes, SWC_k_microbes,
                          TRUE, Forg_FOM, Rm, Q10)
  
  mycofon_balence_graphs()
  
  ###
  # Check that there is mass conservation
  ###
  
  
  
}