#include "mycomodel.h"

// [[Rcpp::export]]
Rcpp::List symphony_multiple_FOM_daily(double Tmb,                  // UNITS: 'C
                                       double SWC,                  // UNITS; mm
                                       double C_FOM_needles,        // UNITS: C kg, FROM: CASSIA
                                       double C_FOM_woody,          // UNITS: C kg, FROM: CASSIA
                                       double C_FOM_roots,          // UNITS: C kg, FROM: CASSIA
                                       double C_FOM_mycelium,       // UNITS: C kg, FROM: CASSIA
                                       double C_SOM,                // UNITS: C kg, FROM: CASSIA
                                       double C_decompose_FOM,      // UNITS: C kg, FROM: here, last iteration
                                       double C_decompose_SOM,      // UNITS: C kg, FROM: here, last iteration
                                       double N_decompose_FOM,      // UNITS: C kg, FROM: here, last iteration
                                       double N_decompose_SOM,      // UNITS: C kg, FROM: here, last iteration
                                       double Litter_needles,       // UNITS: C kg, FROM: CASSIA
                                       double Litter_woody,         // UNITS: C kg, FROM: CASSIA
                                       double Litter_roots,         // UNITS: C kg, FROM: CASSIA
                                       double Litter_mycelium,      // UNITS: C kg, FROM: Mycofon
                                       double NH4,                  // UNITS: C kg, FROM: here, last iteration
                                       double NO3,                  // UNITS: C kg, FROM: here, last iteration
                                       double NH4_used_Plant,       // UNITS: C kg, FROM: n_uptake
                                       double NH4_used_Fungal,      // UNITS: C kg, FROM: n_uptake
                                       double NO3_used_Plant,       // UNITS: C kg, FROM: n_uptake
                                       double NO3_used_Fungal,      // UNITS: C kg, FROM: n_uptake
                                       double FOM_Norg_used_Plant,  // UNITS: C kg, FROM: n_uptake
                                       double FOM_Norg_used_Fungal, // UNITS: C kg, FROM: n_uptake
                                       double SOM_Norg_used,        // UNITS: C kg, FROM: n_uptake
                                       std::vector<double> respiration_microbes_params,
                                       std::vector<double> N_limits_R,
                                       std::vector<double> N_k_R,
                                       std::vector<double> SWC_k_R,
                                       double NC_microbe_opt,       // Assume that this is the same as the above number for the moment 
                                       double microbe_turnover)
{
  
  /*
   * Initialisation or declaration
   */
  
  // INITILISATION FROM VECTORS
  // Parameters into symphony_parameters format
    // TODO: do I need to initialise anything
  
  // SOIL INITIALISATION
  double C_FOM = C_FOM_needles + C_FOM_woody + C_FOM_roots + C_FOM_mycelium; // C kg
  
  /*
   * Nitrogen processes
   */
  
  // STEP 1: Set the values, considering the other models
  NH4 = NH4 - NH4_used_Plant - NH4_used_Fungal;             // C kg
  NO3 = NO3 - NO3_used_Plant - NO3_used_Fungal;             // C kg
  C_FOM = C_FOM - FOM_Norg_used_Plant - FOM_Norg_used_Fungal;    // C kg
  C_SOM = C_SOM;                            // C kg
  
  // STEP 2: consider the uptake functions, for FOM and SOM, 
  // this includes the decomposition and mineralisation / immobilisation
  // Apart from the biomass that the microbes specialise in, the second N uptake is considered to be equally driven between the types
  
  // FOM
  Rcpp::List FOM_after_microbe_activity_list = Microbe_Uptake(C_decompose_FOM, N_decompose_FOM, NC_microbe_opt, NH4, NO3, C_FOM, Tmb, SWC, N_limits_R, N_k_R, SWC_k_R, false, C_FOM);
  N_balence FOM_after_microbe_activity = list_to_N_balence(FOM_after_microbe_activity_list);    // C kg
  // SOM
  Rcpp::List SOM_after_microbe_activity_list = Microbe_Uptake(C_decompose_SOM, N_decompose_SOM, NC_microbe_opt, NH4, NO3, C_SOM, Tmb, SWC, N_limits_R, N_k_R, SWC_k_R, true, C_FOM);
  N_balence SOM_after_microbe_activity = list_to_N_balence(SOM_after_microbe_activity_list);    // C kg
  
  // Update pure inorganic pools
  NH4 = NH4 - C_decompose_FOM*FOM_after_microbe_activity.NH4 - C_decompose_SOM*SOM_after_microbe_activity.NH4;    // C kg
  NO3 = NO3 - C_decompose_FOM*FOM_after_microbe_activity.NO3 - C_decompose_SOM*SOM_after_microbe_activity.NO3;    // C kg
  
  /*
   * Carbon processes
   */
  
  // TODO: add the N uptake!
  
  // STEP 1: Update the FOM mass
  double total_decomposition = C_decompose_FOM*FOM_after_microbe_activity.C + C_decompose_SOM*SOM_after_microbe_activity.Norg_FOM;   // C kg
  // the nitrogen transfer should be by N and litter type
  C_FOM_needles = C_FOM_needles + Litter_needles - (C_FOM_needles/C_FOM)*total_decomposition;             // C kg
  C_FOM_woody = C_FOM_woody + Litter_woody - (C_FOM_woody/C_FOM)*total_decomposition;                     // C kg
  C_FOM_roots = C_FOM_roots + Litter_roots - (C_FOM_roots/C_FOM)*total_decomposition;                     // C kg
  C_FOM_mycelium = C_FOM_mycelium + Litter_mycelium - (C_FOM_mycelium/C_FOM)*total_decomposition;         // C kg
  
  // STEP 2: Update the SOM mass
  C_SOM = C_SOM - C_decompose_SOM*SOM_after_microbe_activity.C + microbe_turnover*(C_decompose_FOM + C_decompose_SOM);   // C kg
  
  // STEP 3: Update the microbes
  C_decompose_FOM = (1 + 
    C_decompose_FOM*FOM_after_microbe_activity.C - 
    respiration(Tmb, respiration_microbes_params[1], respiration_microbes_params[2]) - 
    microbe_turnover) * C_decompose_FOM;   // C kg
  
  C_decompose_SOM = (1 + 
    C_decompose_SOM*SOM_after_microbe_activity.C - 
    respiration(Tmb, respiration_microbes_params[1], respiration_microbes_params[2]) - 
    microbe_turnover) * C_decompose_SOM;   // C kg
  
  // Output
  return Rcpp::List::create(Rcpp::_["C_decompose_FOM"] = C_decompose_FOM,          // C kg
                            Rcpp::_["C_decompose_SOM"] = C_decompose_SOM,          // C kg
                            Rcpp::_["C_FOM_needles"] = C_FOM_needles,              // C kg
                            Rcpp::_["C_FOM_woody"] = C_FOM_woody,                  // C kg
                            Rcpp::_["C_FOM_roots"] = C_FOM_roots,                  // C kg
                            Rcpp::_["C_FOM_mycelium"] = C_FOM_mycelium,            // C kg
                            Rcpp::_["C_SOM"] = C_SOM,                              // C kg
                            Rcpp::_["NH4"] = NH4,                                  // C kg
                            Rcpp::_["NO3"] = NO3,                                  // C kg
                            Rcpp::_["Microbe_respiration"] = respiration(Tmb, respiration_microbes_params[1], respiration_microbes_params[2]));    // C kg
}


