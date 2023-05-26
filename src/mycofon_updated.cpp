#include <Rcpp.h>
#include "mycomodel.h"
using namespace Rcpp;

// TODO: for the export to work, maybe make this a list?
// [[Rcpp::export]]
Rcpp::List mycofon_balence(double Allocation_C_CASSIA,
                           double N_to_CASSIA,
                           double root_mass,
                           double fungal_mass,
                           double optimal_root_fungal_biomass_ratio,
                           double C_roots,
                           double C_roots_biomass,
                           double N_roots,
                           double C_fungal,
                           double N_fungal,
                           double turnover_roots,
                           double turnover_fungal,
                           double a_roots,
                           double b_roots,
                           double a_myco,
                           double b_myco,
                           double C_allocation,
                           double N_allocation,
                           std::vector<double> N_in_root_R,
                           double NH4,
                           double NO3,
                           double FOM_Norg,
                           double C_in_root,
                           double NC_in_root_opt,
                           double T,
                           double Tsb,
                           double SWC,
                           std::vector<double> N_limits_R,
                           std::vector<double> N_k_R,
                           std::vector<double> SWC_k_R,
                           double NC_fungal_opt,
                           double mantle_mass,
                           double ERM_mass,
                           double N_avaliable,
                           std::vector<double> parameters_NH4_on_NO3) {
  
  // Mycorrhizal rate
  double m = fungal_mass / (root_mass * optimal_root_fungal_biomass_ratio);
  if (m > 1) {
    std::cout << "Warning:\nThe mycorhization value (m) is more than 1 - check the fungal_mass, root_mass and optimal_root_fungal_biomass_ratio values"; 
  }
  // dC^r/dt TODO: this need to be linked with the CASSIA C sections
  C_roots = C_roots + Allocation_C_CASSIA - (1 - m)*C_roots*turnover_roots - m*C_roots*turnover_fungal - respiration(Tsb, a_roots, b_roots)*C_roots - C_allocation;
  // dC^f/dt
  C_fungal = C_fungal + C_allocation - turnover_fungal*fungal_mass - respiration(Tsb, a_myco, b_myco)*fungal_mass;
  
  // Nitrogen!
  // TODO: should the N in the roots in different forms be differentiated - or can it be grouped at this stage?
  double root_NC_ratio = N_roots / C_roots;
  double fungal_NC_ratio = N_fungal / C_fungal;
  
  
  //dN^r/dt
  double uptake_plant = Plant_N_Uptake(NC_in_root_opt, 
                                       T,
                                       SWC,
                                       m,
                                       NH4,    // TODO: make this have in input that works with the output of the soil function
                                       NO3,
                                       FOM_Norg,
                                       N_limits_R,
                                       N_k_R,
                                       SWC_k_R,
                                       C_roots,
                                       N_roots,
                                       C_roots_biomass,
                                       N_allocation,
                                       parameters_NH4_on_NO3)[1];

  N_roots = N_roots + N_allocation + uptake_plant - 
                     turnover_roots*C_roots*root_NC_ratio - 
                     N_to_CASSIA;
  
  //dN^f/dt
  double uptake_fungal = Fungal_N_Uptake(C_fungal,
                                         N_fungal,
                                         NC_fungal_opt,
                                         mantle_mass,
                                         ERM_mass,
                                         C_roots_biomass,
                                         T,
                                         SWC,
                                         NH4,
                                         NO3,
                                         FOM_Norg,
                                         N_limits_R,
                                         N_k_R,
                                         SWC_k_R)[1];
  
  N_fungal = N_fungal + uptake_fungal - turnover_fungal*C_fungal*fungal_NC_ratio - N_allocation;

  return Rcpp::List::create(Rcpp::_["C_roots"] = C_roots,
                            Rcpp::_["C_fungal"] = C_fungal,
                            Rcpp::_["N_roots"] = N_roots,
                            Rcpp::_["N_fungal"] = N_fungal);
}




