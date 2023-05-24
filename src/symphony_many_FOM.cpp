#include "mycomodel.h"


// [[Rcpp::export]]
Rcpp::List symphony_multiple_FOM_daily(std::vector<double> params, // TODO: need to clean this of old variables and include the newer variables if possible
                                       double Tmb,
                                       double SWC,
                                       double C_plant, // TODO: this should come from CASSIA
                                       double C_FOM_needles, // TODO: this will change
                                       double C_FOM_woody, // TODO: this will change
                                       double C_FOM_roots, // TODO: this will change
                                       double C_FOM_mycelium,
                                       double C_SOM,
                                       double C_decompose_FOM,
                                       double C_decompose_SOM,
                                       double Litter_needles, // TODO: these should come from CASSIA
                                       double Litter_woody, // TODO: these should come from CASSIA
                                       double Litter_roots, // TODO: these should come from CASSIA
                                       double Litter_mycelium, // TODO: should come from Mycofon / gorwth
                                       double NH4, // TODO: this should be from the last iteration
                                       double NO3,
                                       double NH4_used, // TODO: this should be from the last iteration
                                       double NO3_used, // TODO: this should be from the last iteration
                                       double FOM_Norg_used, // TODO: this should be from the last iteration
                                       double SOM_Norg_used, // TODO: this should be from the last iteration
                                       std::vector<double> respiration_microbes_params,
                                       std::vector<double> N_in_soil_R,
                                       std::vector<double> N_limits_R,
                                       std::vector<double> N_k_R,
                                       std::vector<double> SWC_k_R,
                                       double CN_microbe, // Assume that the CN ratio is constant
                                       double NC_microbe_opt, // Assume that this is the same as the above number for the moment 
                                       double microbe_turnover)
{
  
  /*
   * Initialisation or declaration
   */
  
  // INITILISATION FROM VECTORS
  // Parameters into symphony_parameters format
  symphony_parameters symphony_params = vector_to_symphony(params);
  // alpha and beta should be more precise and the same as the parameters in the microbe uptake 
  
  // SOIL INITIALISATION
  double C_FOM = C_FOM_needles + C_FOM_woody + C_FOM_roots + C_FOM_mycelium;
  
  /*
   * Nitrogen processes
   */
  
  // STEP 1: Set the values, considering the other models
  // TODO: check that the N things are correct in terms of in and out - does this calculation really need to be here? Units!
  NH4 = NH4 - NH4_used;
  NO3 = NO3 - NO3_used;
  C_FOM = C_FOM - FOM_Norg_used;
  C_SOM = C_SOM - SOM_Norg_used;
  
  // STEP 2: consider the uptake functions, for FOM and SOM, 
      // this includes the decomposition and mineralisation / immobilisation
      // Apart from the biomass that the microbes specialise in, the second N uptake is considered to be equally driven between the types
      
  // FOM
  Rcpp::List FOM_after_microbe_activity_list = Microbe_Uptake(C_decompose_FOM, CN_microbe*C_decompose_FOM, NC_microbe_opt, NH4, NO3, C_FOM, Tmb, SWC, N_limits_R, N_k_R, SWC_k_R, false, C_FOM);
  N_balence FOM_after_microbe_activity = list_to_N_balence(FOM_after_microbe_activity_list);
  // SOM
  Rcpp::List SOM_after_microbe_activity_list = Microbe_Uptake(C_decompose_SOM, CN_microbe*C_decompose_SOM, NC_microbe_opt, NH4, NO3, C_SOM, Tmb, SWC, N_limits_R, N_k_R, SWC_k_R, true, C_FOM);
  N_balence SOM_after_microbe_activity = list_to_N_balence(SOM_after_microbe_activity_list);
  
  // Update pure inorganic pools
  NH4 = NH4 - C_decompose_FOM*FOM_after_microbe_activity.NH4 - C_decompose_SOM*SOM_after_microbe_activity.NH4; // TODO: check units
  NO3 = NO3 - C_decompose_FOM*FOM_after_microbe_activity.NO3 - C_decompose_SOM*SOM_after_microbe_activity.NO3;
  
  /*
   * Carbon processes
   */
  // STEP 1: Update the FOM mass
  // TODO: SOM part should be in the right units! no multiplier currently
  double total_decomposition = (C_decompose_FOM*FOM_after_microbe_activity.C + C_decompose_SOM*SOM_after_microbe_activity.Norg_FOM);
  // the nitrogen transfer should be by N and litter type
  C_FOM_needles = C_FOM_needles + Litter_needles - (C_FOM_needles/C_FOM)*total_decomposition;
  C_FOM_woody = C_FOM_woody + Litter_woody - (C_FOM_woody/C_FOM)*total_decomposition;
  C_FOM_roots = C_FOM_roots + Litter_roots - (C_FOM_roots/C_FOM)*total_decomposition;
  C_FOM_mycelium = C_FOM_mycelium + Litter_mycelium - (C_FOM_mycelium/C_FOM)*total_decomposition;
  
  // STEP 2: Update the SOM mass
  C_SOM = C_SOM - C_decompose_SOM*SOM_after_microbe_activity.C + microbe_turnover*(C_decompose_FOM + C_decompose_SOM);
  
  // STEP 3: Update the microbes
  C_decompose_FOM = (1 + C_decompose_FOM*FOM_after_microbe_activity.C - respiration(Tmb, respiration_microbes_params[1], respiration_microbes_params[2]) - microbe_turnover) * C_decompose_FOM;
  
  C_decompose_SOM = (1 + C_decompose_SOM*SOM_after_microbe_activity.C - respiration(Tmb, respiration_microbes_params[1], respiration_microbes_params[2]) - microbe_turnover) * C_decompose_SOM;
  
  // Output
  return Rcpp::List::create(Rcpp::_["C_decompose_FOM"] = C_decompose_FOM,
                            Rcpp::_["C_decompose_SOM"] = C_decompose_SOM,
                            Rcpp::_["C_FOM_needles"] = C_FOM_needles,
                            Rcpp::_["C_FOM_woody"] = C_FOM_woody,
                            Rcpp::_["C_FOM_roots"] = C_FOM_roots,
                            Rcpp::_["C_FOM_mycelium"] = C_FOM_mycelium,
                            Rcpp::_["C_SOM"] = C_SOM,
                            Rcpp::_["NH4"] = NH4,
                            Rcpp::_["NO3"] = NO3,
                            Rcpp::_["Microbe_respiration"] = respiration(Tmb, respiration_microbes_params[1], respiration_microbes_params[2]));
}


