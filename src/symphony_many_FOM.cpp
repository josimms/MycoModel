#include "mycomodel.h"


// [[Rcpp::export]]
Rcpp::List symphony_multiple_FOM_daily(std::vector<double> params,
                                       double Tmb,
                                       double SWC,
                                       double C_plant, // TODO: this should come from CASSIA
                                       double C_FOM, // TODO: this will change
                                       double Litter_needles, // TODO: these should come from CASSIA
                                       double Litter_wood, // TODO: these should come from CASSIA
                                       double Litter_roots, // TODO: these should come from CASSIA
                                       double N_avaliable_in, // TODO: this should be from the last iteration
                                       double N_used_in, // TODO: this should be from the last iteration
                                       double N, // TODO: should be from different types of N
                                       std::vector<double> respiration_microbes_params,
                                       std::vector<double> N_in_soil_R,
                                       std::vector<double> N_limits_R,
                                       std::vector<double> N_k_R,
                                       std::vector<double> SWC_k_R,
                                       double CN_microbe, // Assume that the CN ratio is constant
                                       double NC_microbe_opt) // Assume that this is the same as the above number for the moment 
{
  
  /*
   * Initialisation or declaration
   */
  
  // INITILISATION FROM VECTORS
  // Parameters into symphony_parameters format
  symphony_parameters symphony_params = vector_to_symphony(params);
  
  // DECLAIRATIONS
  double N_avaliable_out;
  
  double psi_ims = 1;
  double psi_imf = 1;
  double psi_f = 1;
  double psi_d = 1;
  double C_decompose_SOM = 1;
  double C_decompose_FOM = 1;
  double C_SOM = 1;
  
  double Photosynthesis_min;
  double psi_d_min;
  double psi_f_min;
  
  // alpha and beta should maybe be reset each year, so I need to add them!
  
  // N in
  N_avaliable_out = N_avaliable_in - N_used_in;
  // N immobilisation, mineralisation
  psi_ims = symphony_params.alpha * respiration(Tmb, respiration_microbes_params[1], respiration_microbes_params[2]) * C_decompose_SOM + (symphony_params.beta - symphony_params.alpha)*psi_d;
  // N immobilisation,
  psi_imf = symphony_params.alpha * respiration(Tmb, respiration_microbes_params[1], respiration_microbes_params[2]) * C_decompose_FOM + (symphony_params.beta - symphony_params.alpha)*psi_f;
  // N decomposition flux
  psi_d_min = std::min(symphony_params.y*C_FOM, (symphony_params.i*N + symphony_params.alpha * respiration(Tmb, respiration_microbes_params[1], respiration_microbes_params[2]) * C_decompose_SOM)/(symphony_params.alpha - symphony_params.beta));
  psi_d = psi_d_min;
  psi_f_min = std::min(symphony_params.u*C_FOM, (symphony_params.i*N + symphony_params.alpha * respiration(Tmb, respiration_microbes_params[1], respiration_microbes_params[2]) * C_decompose_FOM)/(symphony_params.alpha - symphony_params.beta));
  psi_f = psi_d_min;
  
  // C_plant = C_plant + Photosynthesis - C_plant*(symphony_params.r_p + symphony_params.m_p + symphony_params.e_p);
  C_FOM = C_FOM + symphony_params.m_p * C_plant - psi_d - psi_f;
  
  C_decompose_FOM = C_decompose_FOM + 
    psi_f - 
    (Microbe_Uptake(C_decompose_FOM, CN_microbe*C_decompose_FOM, NC_microbe_opt, N_avaliable_out, Tmb, SWC, N_in_soil_R, N_limits_R, N_k_R, SWC_k_R) + respiration(Tmb, respiration_microbes_params[1], respiration_microbes_params[2])) * C_decompose_FOM;
  
  C_decompose_SOM = C_decompose_SOM + 
    (symphony_params.A - Microbe_Uptake(C_decompose_FOM, CN_microbe*C_decompose_FOM, NC_microbe_opt, N_avaliable_out, Tmb, SWC, N_in_soil_R, N_limits_R, N_k_R, SWC_k_R) - respiration(Tmb, respiration_microbes_params[1], respiration_microbes_params[2]))*C_decompose_SOM + 
    psi_d;
  
  // The N_available_out replaces the Psi_p, or the N that should go into the plant
  N = N + 
    symphony_params.psi_i - 
    symphony_params.l*N - 
    N_avaliable_out + 
    psi_ims + 
    psi_imf; 
  
  C_SOM = C_SOM + 
    (Microbe_Uptake(C_decompose_FOM, CN_microbe*C_decompose_FOM, NC_microbe_opt, N_avaliable_out, Tmb, SWC, N_in_soil_R, N_limits_R, N_k_R, SWC_k_R) - symphony_params.A) * C_decompose_SOM;
  
  
  // Output
  return Rcpp::List::create(Rcpp::_["C_decompose_FOM"] = C_decompose_FOM,
                            Rcpp::_["C_decompose_SOM"] = C_decompose_SOM,
                            Rcpp::_["C_plant"] = C_plant,
                            Rcpp::_["C_FOM"] = C_FOM,
                            Rcpp::_["C_SOM"] = C_SOM,
                            Rcpp::_["N"] = N,
                            Rcpp::_["Microbe_respiration"] = respiration(Tmb, respiration_microbes_params[1], respiration_microbes_params[2]));
}


