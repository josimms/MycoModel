#include "mycomodel.h"

// [[Rcpp::export]]
Rcpp::List symphony_multiple_FOM_daily(double Tmb,
                                       double SWC,
                                       double C_FOM_needles,
                                       double C_FOM_woody,
                                       double C_FOM_roots,
                                       double C_FOM_mantle,
                                       double C_FOM_ERM,
                                       double C_SOM,
                                       double N_SOM,
                                       double C_decompose_FOM,
                                       double C_decompose_SOM,
                                       double N_decompose_FOM,
                                       double N_decompose_SOM,
                                       double Litter_needles,
                                       double Litter_woody,
                                       double Litter_roots,
                                       double Litter_mantle,
                                       double Litter_ERM,
                                       double imobilisation,
                                       double assimilation,
                                       double NH4,
                                       double NO3,
                                       double NC_needles,
                                       double NC_woody,
                                       double NC_roots,
                                       double NC_mantle,
                                       double NC_ERM,
                                       double NH4_used_Plant,
                                       double NH4_used_Fungal,
                                       double NO3_used_Plant,
                                       double NO3_used_Fungal,
                                       double FOM_Norg_used_Plant,
                                       double FOM_Norg_used_Fungal,
                                       double SOM_Norg_used,
                                       std::vector<double> respiration_microbes_params,
                                       std::vector<double> N_limits_R,
                                       std::vector<double> N_k_R,
                                       std::vector<double> SWC_k_R,
                                       double NC_microbe_opt,
                                       double microbe_turnover)
{
  
  /*
   * Initialisation or declaration
   */
  
  // INITILISATION FROM VECTORS
  // Parameters into symphony_parameters format
  
  // SOIL INITIALISATION NEW INPUT!
  C_FOM_needles = C_FOM_needles + Litter_needles;             // C kg
  C_FOM_woody = C_FOM_woody + Litter_woody;                   // C kg
  C_FOM_roots = C_FOM_roots + Litter_roots;                   // C kg
  C_FOM_mantle = C_FOM_mantle + Litter_mantle;                // C kg
  C_FOM_ERM = C_FOM_ERM + Litter_ERM;                         // C kg
  
  // AGGREGATION
  double C_FOM = C_FOM_needles +
    C_FOM_woody + 
    C_FOM_roots +
    C_FOM_mantle +
    C_FOM_ERM; // C kg
  double N_FOM = C_FOM_needles * NC_needles + 
    C_FOM_woody * NC_woody + 
    C_FOM_roots * NC_roots +
    C_FOM_mantle * NC_mantle + 
    C_FOM_ERM * NC_ERM; // C kg

  /*
   * Nitrogen processes
   */
  
  // STEP 1: Set the values, considering the other models
  NH4 = NH4 - NH4_used_Plant - NH4_used_Fungal;             // C kg
  NO3 = NO3 - NO3_used_Plant - NO3_used_Fungal;             // C kg
  N_FOM = N_FOM - FOM_Norg_used_Plant - FOM_Norg_used_Fungal;    // C kg
  N_SOM = N_SOM;                                            // C kg
  
  double NC_Litter = N_FOM/C_FOM;
  
  // TODO: what about the carbon considerations in the N used! 
  
  // STEP 2: consider the uptake functions, for FOM and SOM, MICROBES
  // this includes the decomposition and mineralisation / immobilisation
  // Apart from the biomass that the microbes specialise in, the second N uptake is considered to be equally driven between the types
  
  // FOM
  Rcpp::List FOM_after_microbe_activity_list = Microbe_Uptake(C_decompose_FOM, N_decompose_FOM, C_FOM, NC_microbe_opt, NH4, NO3, N_FOM, Tmb, SWC, NC_Litter, imobilisation, assimilation, N_limits_R, N_k_R, SWC_k_R, false, respiration_microbes_params);
  N_balence FOM_after_microbe_activity = list_to_N_balence(FOM_after_microbe_activity_list);    // C kg
  // SOM
  Rcpp::List SOM_after_microbe_activity_list = Microbe_Uptake(C_decompose_SOM, N_decompose_SOM, C_SOM, NC_microbe_opt, NH4, NO3, N_FOM, Tmb, SWC, NC_Litter, imobilisation, assimilation, N_limits_R, N_k_R, SWC_k_R, true, respiration_microbes_params);
  N_balence SOM_after_microbe_activity = list_to_N_balence(SOM_after_microbe_activity_list);    // C kg
  
  // Update pure inorganic pools
  NO3 = NO3 - C_decompose_FOM*FOM_after_microbe_activity.NO3 - C_decompose_SOM*SOM_after_microbe_activity.NO3;    // C kg eq
  NH4 = NH4 - C_decompose_FOM*FOM_after_microbe_activity.NH4 - C_decompose_SOM*SOM_after_microbe_activity.NH4;    // C kg eq
  
  N_FOM = N_FOM - C_decompose_FOM*FOM_after_microbe_activity.Norg - C_decompose_SOM*SOM_after_microbe_activity.Norg_FOM;    // C kg eq
  N_SOM = N_SOM - C_decompose_SOM*SOM_after_microbe_activity.Norg;    // C kg eq 

  /*
   * Carbon processes
   */
  
  // STEP 1: Update the FOM mass
  double total_decomposition = C_decompose_FOM*FOM_after_microbe_activity.C + C_decompose_SOM*SOM_after_microbe_activity.Norg_FOM;   // C kg
  // TODO: the nitrogen transfer should be by N and litter type rather than size of pool!
  C_FOM_needles = C_FOM_needles - (C_FOM_needles/C_FOM)*total_decomposition;            // C kg
  C_FOM_woody = C_FOM_woody - (C_FOM_woody/C_FOM)*total_decomposition;                  // C kg
  C_FOM_roots = C_FOM_roots - (C_FOM_roots/C_FOM)*total_decomposition;                  // C kg
  C_FOM_mantle = C_FOM_mantle - (C_FOM_mantle/C_FOM)*total_decomposition;               // C kg
  C_FOM_ERM = C_FOM_ERM - (C_FOM_ERM/C_FOM)*total_decomposition;                        // C kg
  
  // STEP 2: Update the SOM mass
  C_SOM = C_SOM - C_decompose_SOM*SOM_after_microbe_activity.C + microbe_turnover*(C_decompose_FOM + C_decompose_SOM);   // C kg
  
  // STEP 3: Update the microbes
  C_decompose_FOM = microbe_turnover + (1 + 
    FOM_after_microbe_activity.C - 
    respiration(Tmb, respiration_microbes_params[1], respiration_microbes_params[2])) * C_decompose_FOM;   // C kg
  
  C_decompose_SOM = microbe_turnover + (1 + 
    SOM_after_microbe_activity.C - 
    respiration(Tmb, respiration_microbes_params[1], respiration_microbes_params[2])) * C_decompose_SOM;   // C kg
  
  // Output
  return Rcpp::List::create(Rcpp::_["C_decompose_FOM"] = C_decompose_FOM,          // C kg
                            Rcpp::_["C_decompose_SOM"] = C_decompose_SOM,          // C kg
                            Rcpp::_["C_FOM_needles"] = C_FOM_needles,              // C kg
                            Rcpp::_["C_FOM_woody"] = C_FOM_woody,                  // C kg
                            Rcpp::_["C_FOM_roots"] = C_FOM_roots,                  // C kg
                            Rcpp::_["C_FOM_mantle"] = C_FOM_mantle,                // C kg
                            Rcpp::_["C_FOM_ERM"] = C_FOM_ERM,                      // C kg
                            Rcpp::_["C_SOM"] = C_SOM,                              // C kg
                            Rcpp::_["NH4"] = NH4,                                  // C kg
                            Rcpp::_["NO3"] = NO3,                                  // C kg
                            Rcpp::_["Microbe_respiration"] = respiration(Tmb, respiration_microbes_params[1], respiration_microbes_params[2]));    // C kg
}


