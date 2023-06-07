#include <Rcpp.h>
#include "mycomodel.h"

// Initisation from values I have gathered

parameters parameters_initalise_test(std::vector<double> parameters_R) {
  parameters out;
  out.microbe_turnover = parameters_R[1];
  out.NC_in_root_opt = parameters_R[2];
  out.NC_fungal_opt = parameters_R[3];
  out.NC_microbe_opt = parameters_R[4];
  out.percentage_C_biomass = parameters_R[5];
  out.N_limits_plant = {parameters_R[6], parameters_R[7], parameters_R[8]};
  out.N_limits_fungal = {parameters_R[9], parameters_R[10], parameters_R[11]};
  out.N_limits_microbes = {parameters_R[12], parameters_R[13], parameters_R[14]};
  out.N_k_plant = {parameters_R[15], parameters_R[16], parameters_R[17]};
  out.N_k_fungal = {parameters_R[18], parameters_R[19], parameters_R[20]};
  out.N_k_microbes = {parameters_R[21], parameters_R[22], parameters_R[23]};
  out.SWC_k_plant = {parameters_R[24], parameters_R[25], parameters_R[26]};
  out.SWC_k_fungal = {parameters_R[27], parameters_R[28], parameters_R[29]};
  out.SWC_k_microbes = {parameters_R[30], parameters_R[31], parameters_R[32]};
  out.NH4_on_NO3 = {parameters_R[33], parameters_R[34], parameters_R[35]};
  out.respiration_params = {parameters_R[36], parameters_R[37], parameters_R[38], parameters_R[39], parameters_R[40], parameters_R[41]};
  out.optimal_root_fungal_biomass_ratio = parameters_R[42];
  out.turnover_mantle = parameters_R[43];
  out.turnover_ERM = parameters_R[44];
  out.turnover_roots_mycorrhized = parameters_R[45];
  out.turnover_fungal = parameters_R[46];
  out.mantle_mass = parameters_R[47];
  out.ERM_mass = parameters_R[48];
  out.carbon_use = parameters_R[49];
  out.nitrogen_use = parameters_R[50];
  out.C_value_param_myco = parameters_R[51];
  out.N_value_param_myco = parameters_R[52];
  out.C_value_param_plant = parameters_R[53];
  out.N_value_param_plant = parameters_R[54];
  return(out);
};


// Initalisation function for the parameters

// TODO: go through the parameters when I have found them!
parameters parameters_initalise() {
  parameters parameters_in;
  parameters_in.microbe_turnover = 0.016906; // Preeven 2013, Symphony model paper, rate of microbes going to SOM
  parameters_in.NC_in_root_opt = (0.001*2875)/1881; // Helmisaari 1995: Nutroent cycling in Pinus sylvestris stands in eastern Finland, belowground component
  parameters_in.NC_fungal_opt = 0.025; // Meyer 2010, CN ratio of 40 fungal
  parameters_in.NC_microbe_opt = 0.0909091; // Perveen 2013, N:C ratio in SOM and in decomposers
  // TODO: this should be modified for the multiple FOM, and a value from from Helmusaari in the end
  parameters_in.percentage_C_biomass = 0.4; // Value for wood, TODO; this should be for each of the biomass catagories!
  parameters_in.N_limits_plant = {parameters_R[6], parameters_R[7], parameters_R[8]}; // Arbitrarty - Bayesian Callibration
  parameters_in.N_limits_fungal = {parameters_R[9], parameters_R[10], parameters_R[11]}; // Arbitrarty - Bayesian Callibration
  parameters_in.N_limits_microbes = {parameters_R[12], parameters_R[13], parameters_R[14]}; // Arbitrarty - Bayesian Callibration
  parameters_in.N_k_plant = {parameters_R[15], parameters_R[16], parameters_R[17]}; // Arbitrarty - Bayesian Callibration
  parameters_in.N_k_fungal = {parameters_R[18], parameters_R[19], parameters_R[20]}; // Arbitrarty - Bayesian Callibration
  parameters_in.N_k_microbes = {parameters_R[21], parameters_R[22], parameters_R[23]}; // Arbitrarty - Bayesian Callibration
  parameters_in.SWC_k_plant = {parameters_R[24], parameters_R[25], parameters_R[26]}; // Arbitrarty - Bayesian Callibration
  parameters_in.SWC_k_fungal = {parameters_R[27], parameters_R[28], parameters_R[29]}; // Arbitrarty - Bayesian Callibration
  parameters_in.SWC_k_microbes = {parameters_R[30], parameters_R[31], parameters_R[32]}; // Arbitrarty - Bayesian Callibration
  parameters_in.NH4_on_NO3 = {parameters_R[33], parameters_R[34], parameters_R[35]}; // Arbitrarty - Bayesian Callibration
  parameters_in.respiration_params = {0.00958, 2.5575, 0.00958, 2.5575, 0.00958, 2.5575}; // TODO: CASSIA values from roots
  parameters_in.optimal_root_fungal_biomass_ratio = 0.9; // Heinonsalo ?
  parameters_in.turnover_mantle = 1/625; // ERM turnover ranges between 10 days to 100 days, while mantle turnover time is 625 days
  parameters_in.turnover_ERM = 1/50;
  parameters_in.turnover_roots = 1/365; // Meyer, 2010 Non-mycorrhizal roots have a turnover (dr_r) of 365 days TODO: CASSIA contrast
  parameters_in.turnover_roots_mycorrhized = 1/625; // Meyer, 2010
  out.carbon_use = growth_C = 0.2; // Arbitrarty - TODO
  out.nitrogen_use = growth_N = 0.2; // Arbitrarty - TODO
  out.C_value_param_myco = 4; // Arbitrarty - Bayesian Callibration
  out.N_value_param_myco = 4; // Arbitrarty- Bayesian Callibration
  out.C_value_param_plant = 4; // Arbitrarty- Bayesian Callibration
  out.N_value_param_plant = 4; // Arbitrarty- Bayesian Callibration
  return(parameters_in);
};

// [[Rcpp::export]]
Rcpp::List Toy_Model(double year,
                     double C_roots, 
                     double N_roots, 
                     double C_fungal, 
                     double N_fungal,
                     Rcpp::DataFrame Hyde_weather,
                     std::vector<double> parameters_R)
{
  /*
   * Weather data formatted for the code
   */
  
  std::vector<double> PAR = Hyde_weather["PAR"];
  std::vector<double> TAir = Hyde_weather["TAir"];
  std::vector<double> VPD = Hyde_weather["VPD"];
  std::vector<double> CO2 = Hyde_weather["CO2"];
  std::vector<double> Tmb = Hyde_weather["Tmb"];
  std::vector<double> Tma = Hyde_weather["Tma"]; // TODO: what should this be?
  std::vector<double> fAPAR = Hyde_weather["fAPAR"];
  std::vector<double> SWC = Hyde_weather["SWC"];
  
  /*
   * Initialisation
   */
  
  // Should either be from CASSIA or this whole function should be in the CASSIA function
  
  CASSIA_output CASSIA_out;
  CASSIA_out.C_roots = C_roots;
  CASSIA_out.N_roots = N_roots;
  
  MYCOFON_output MYTCOFON_out;
  MYTCOFON_out.C_fungal = C_fungal;
  MYTCOFON_out.N_fungal = N_fungal;
  
  /*
   * Parameteres from R to parameter form
   */
  parameters parameters_in = parameters_initalise_test(parameters_R);
  // As C_fungal: 50:50 mantle and ERM, Meyer 2010
  parameters_in.mantle_mass = MYTCOFON_out.C_fungal/2; // Meyer 2010
  parameters_in.ERM_mass = MYTCOFON_out.C_fungal/2; // Meyer 2010

  for (int days = 0; days <= leap_year(year)-1; ++days) { 
    // Import the N values form the last iteration
    N_balence N_in_soil_R; // import from symphony somehow
    
    // DECUSION STAGE
    DECISION_output DECISION_out;
    
    
    SYMPHONY_output SYMPHONY_out;
    
    // Photosynthesis from PRELES
    double Photosynthesis = 1;
    
    // CASSIA to do the tree stage'
    // TODO: add the variables here as place holders, should go through the code to make this make sense
    CASSIA_output CASSIA_out;
    
    // MYCOFON
      // TODO: add the model here!
    MYCOFON_output MYTCOFON_out;
    
    // TODO: work out which parameters are available here
    Rcpp::List MYCOFON_out =  mycofon_balence(CASSIA_out.C_roots, CASSIA_out.N_roots,
                                              parameters_in.percentage_C_biomass, parameters_in.optimal_root_fungal_biomass_ratio,
                                              MYTCOFON_out.C_fungal, MYTCOFON_out.N_fungal,
                                              parameters_in.turnover_roots, parameters_in.turnover_roots_mycorrhized, parameters_in.turnover_fungal,
                                              parameters_in.respiration_params,
                                              SYMPHONY_out.NH4, SYMPHONY_out.NO3, SYMPHONY_out.N_FOM,
                                              parameters_in.NC_in_root_opt,
                                              parameters_in.NC_fungal_opt,
                                              TAir[days], Tmb[days], SWC[days],
                                              parameters_in.N_limits_plant,
                                              parameters_in.N_k_plant,
                                              parameters_in.SWC_k_plant,
                                              parameters_in.N_limits_fungal,
                                              parameters_in.N_k_fungal,
                                              parameters_in.SWC_k_fungal,
                                              parameters_in.mantle_mass, 
                                              parameters_in.ERM_mass,
                                              parameters_in.NH4_on_NO3,
                                              parameters_in.growth_C,
                                              parameters_in.growth_N,
                                              parameters_in.C_value_param_myco,
                                              parameters_in.N_value_param_myco,
                                              parameters_in.C_value_param_plant,
                                              parameters_in.N_value_param_plant);
    
    // Soil model symphony
    Rcpp::List Soil_All = symphony_multiple_FOM_daily(Tmb[days], SWC[days],
                                                      SYMPHONY_out.C_FOM_needles, SYMPHONY_out.C_FOM_woody, SYMPHONY_out.C_FOM_roots, SYMPHONY_out.C_FOM_mycelium,
                                                      SYMPHONY_out.C_SOM, SYMPHONY_out.N_SOM,
                                                      SYMPHONY_out.C_decompose_FOM, SYMPHONY_out.C_decompose_SOM,
                                                      SYMPHONY_out.N_decompose_FOM, SYMPHONY_out.N_decompose_SOM,
                                                      CASSIA_out.Litter_needles, CASSIA_out.Litter_woody, CASSIA_out.Litter_roots, CASSIA_out.Litter_mycelium, 
                                                      SYMPHONY_out.NH4, SYMPHONY_out.NO3,
                                                      SYMPHONY_out.N_FOM_needles, SYMPHONY_out.N_FOM_woody,
                                                      SYMPHONY_out.N_FOM_roots, SYMPHONY_out.N_FOM_mycelium,
                                                      DECISION_out.NH4_used_Plant, DECISION_out.NH4_used_Fungal,  // TODO think about this!
                                                      DECISION_out.NO3_used_Plant, DECISION_out.NO3_used_Fungal,
                                                      DECISION_out.FOM_Norg_used_Plant, DECISION_out.FOM_Norg_used_Fungal, SYMPHONY_out.SOM_Norg_used, 
                                                      parameters_in.respiration_params, parameters_in.N_limits_microbes, parameters_in.N_k_microbes, parameters_in.SWC_k_microbes,
                                                      parameters_in.NC_microbe_opt, parameters_in.microbe_turnover);

  }
  
  return(0);
}