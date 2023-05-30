#include <Rcpp.h>
#include "mycomodel.h"


// Initalisation function for the parameters
parameters parameters_initalise(std::vector<double> parameters_R) {
  parameters out;
  out.m = parameters_R[0];
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
  out.turnover_roots = parameters_R[43];
  out.turnover_roots_mycorrhized = parameters_R[44];
  out.turnover_fungal = parameters_R[45];
  out.mantle_mass = parameters_R[46];
  out.ERM_mass = parameters_R[47];
  out.carbon_use = parameters_R[48];
  out.nitrogen_use = parameters_R[49];
  return(out);
};

// [[Rcpp::export]]
Rcpp::List Toy_Model(double year, 
                     Rcpp::DataFrame Hyde_weather,
                     std::vector<double> parameters_R)
{
  /*
   * Parameteres from R to parameter form
   */
  
  parameters parameters_in = parameters_initalise(parameters_R);
  
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
   * Although the final model should probably work with CASSIA as the driver of the yearly behaviour - use this as a test
   */
  
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
    
    // Soil model symphony
    Rcpp::List Soil_All = symphony_multiple_FOM_daily(Tmb[days], SWC[days],
                                                       SYMPHONY_out.C_FOM_needles, SYMPHONY_out.C_FOM_woody, SYMPHONY_out.C_FOM_roots, SYMPHONY_out.C_FOM_mycelium,
                                                       SYMPHONY_out.C_SOM, 
                                                       SYMPHONY_out.C_decompose_FOM, SYMPHONY_out.C_decompose_SOM,
                                                       SYMPHONY_out.N_decompose_FOM, SYMPHONY_out.N_decompose_SOM,
                                                       CASSIA_out.Litter_needles, CASSIA_out.Litter_woody, CASSIA_out.Litter_roots, CASSIA_out.Litter_mycelium, 
                                                       SYMPHONY_out.NH4, SYMPHONY_out.NO3,
                                                       DECISION_out.NH4_used_Plant, DECISION_out.NH4_used_Fungal,  // TODO think about this!
                                                       DECISION_out.NO3_used_Plant, DECISION_out.NO3_used_Fungal,
                                                       DECISION_out.FOM_Norg_used_Plant, DECISION_out.FOM_Norg_used_Fungal, SYMPHONY_out.SOM_Norg_used, 
                                                       parameters_in.respiration_params, parameters_in.N_limits_microbes, parameters_in.N_k_microbes, parameters_in.SWC_k_microbes,
                                                       parameters_in.NC_microbe_opt, parameters_in.microbe_turnover);
    
    // TODO: decision function shouold be here or in the mycofon balance
    
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
                                             parameters_in.NC_fungal_opt,
                                             parameters_in.mantle_mass, 
                                             parameters_in.ERM_mass,
                                             parameters_in.NH4_on_NO3,
                                             parameters_in.carbon_use,
                                             parameters_in.nitrogen_use);
    
    
  }
  
  return(0);
}