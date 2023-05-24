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
                                       double NH4_avaliable_in, // TODO: this should be from the last iteration
                                       double NO3_avaliable_in,
                                       double Norg_avaliable_in,
                                       double NH4_used_in, // TODO: this should be from the last iteration
                                       double NO3_used_in, // TODO: this should be from the last iteration
                                       double Norg_used_in, // TODO: this should be from the last iteration
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
  
  // DECLAIRATIONS
  double NH4_avaliable_out;
  double NO3_avaliable_out;
  double Norg_avaliable_out;
  
  /*
   * Nitrogen processes
   */
  
  // N in
  // TODO: check that the N things are correct in terms of in and out, as well as types and units!
  NH4_avaliable_out = NH4_avaliable_in - NH4_used_in;
  NO3_avaliable_out = NO3_avaliable_in - NO3_used_in;
  Norg_avaliable_out = Norg_avaliable_in - Norg_used_in;
  
  // N immobilisation, mineralisation
  // This is linked to the uptake in the Microbe upatek function, where the C limitation is considered against the n limitation of avaliablity, now simply multiply by the mass
  // Assume that the microbes are a group that can take up all of the N types fairly evenly
  // TODO: currently assuming that the decomposition isn't FOM type based, although the types do exist later in the code
  double NH4_micorbe_uptake_FOM = Microbe_Uptake(C_decompose_FOM, CN_microbe*C_decompose_FOM, NC_microbe_opt, NH4_avaliable_out, NO3_avaliable_out, Norg_avaliable_out, Tmb, SWC, N_in_soil_R, N_limits_R, N_k_R, SWC_k_R)[1];
  double NO3_micorbe_uptake_FOM = Microbe_Uptake(C_decompose_FOM, CN_microbe*C_decompose_FOM, NC_microbe_opt, NH4_avaliable_out, NO3_avaliable_out, Norg_avaliable_out, Tmb, SWC, N_in_soil_R, N_limits_R, N_k_R, SWC_k_R)[2];
  double Norg_micorbe_uptake_FOM = Microbe_Uptake(C_decompose_FOM, CN_microbe*C_decompose_FOM, NC_microbe_opt, NH4_avaliable_out, NO3_avaliable_out, Norg_avaliable_out, Tmb, SWC, N_in_soil_R, N_limits_R, N_k_R, SWC_k_R)[3];
  
  NH4_avaliable_out = NH4_avaliable_out - C_decompose_FOM*NH4_micorbe_uptake_FOM; // TODO: check units
  NO3_avaliable_out = NO3_avaliable_out - C_decompose_FOM*NO3_micorbe_uptake_FOM;
  Norg_avaliable_out = Norg_avaliable_out - C_decompose_FOM*Norg_micorbe_uptake_FOM;
  
  /*
   * Carbon processes
   */
  // Decomposition / uptake
  double C_micorbe_uptake_FOM = Microbe_Uptake(C_decompose_FOM, CN_microbe*C_decompose_FOM, NC_microbe_opt, NH4_avaliable_out, NO3_avaliable_out, Norg_avaliable_out, Tmb, SWC, N_in_soil_R, N_limits_R, N_k_R, SWC_k_R)[4];
  double C_micorbe_uptake_SOM = Microbe_Uptake(C_decompose_SOM, CN_microbe*C_decompose_SOM, NC_microbe_opt, NH4_avaliable_out, NO3_avaliable_out, Norg_avaliable_out, Tmb, SWC, N_in_soil_R, N_limits_R, N_k_R, SWC_k_R)[4];
  
  
  // Carbon balance calculations
  double total_FOM_before = C_FOM_needles + C_FOM_woody + C_FOM_roots + C_FOM_mycelium;
  
  // Update the FOM mass
  C_FOM_needles = C_FOM_needles + Litter_needles - (C_FOM_needles/total_FOM_before)*C_decompose_FOM*C_micorbe_uptake_FOM; // the nitrogen transfer should be by N and litter type
  C_FOM_woody = C_FOM_woody + Litter_woody - (C_FOM_woody/total_FOM_before)*C_decompose_FOM*C_micorbe_uptake_FOM;
  C_FOM_roots = C_FOM_roots + Litter_roots - (C_FOM_roots/total_FOM_before)*C_decompose_FOM*C_micorbe_uptake_FOM;
  C_FOM_mycelium = C_FOM_needles + Litter_mycelium - (C_FOM_mycelium/total_FOM_before)*C_decompose_FOM*C_micorbe_uptake_FOM;
  
  // Update the SOM mass
  C_SOM = C_SOM + microbe_turnover*(C_decompose_FOM + C_decompose_SOM); // TODO: doesn't have the emergency turnover any more - in uptake?
  
  // Update the microbes
  C_decompose_FOM = (1 + C_micorbe_uptake_FOM - respiration(Tmb, respiration_microbes_params[1], respiration_microbes_params[2]) - microbe_turnover) * C_decompose_FOM;
  
  C_decompose_SOM = (1 - C_micorbe_uptake_SOM - respiration(Tmb, respiration_microbes_params[1], respiration_microbes_params[2]) - microbe_turnover) * C_decompose_SOM;
  
  // Output
  return Rcpp::List::create(Rcpp::_["C_decompose_FOM"] = C_decompose_FOM,
                            Rcpp::_["C_decompose_SOM"] = C_decompose_SOM,
                            Rcpp::_["C_FOM_needles"] = C_FOM_needles,
                            Rcpp::_["C_FOM_woody"] = C_FOM_woody,
                            Rcpp::_["C_FOM_roots"] = C_FOM_roots,
                            Rcpp::_["C_FOM_mycelium"] = C_FOM_mycelium,
                            Rcpp::_["C_SOM"] = C_SOM,
                            Rcpp::_["NH4"] = NH4_avaliable_out,
                            Rcpp::_["NO3"] = NO3_avaliable_out,
                            Rcpp::_["Norg"] = Norg_avaliable_out,
                            Rcpp::_["Microbe_respiration"] = respiration(Tmb, respiration_microbes_params[1], respiration_microbes_params[2]));
}


