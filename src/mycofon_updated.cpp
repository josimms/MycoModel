#include <Rcpp.h>
using namespace Rcpp;

// TODO: for the export to work, maybe make this a list?
// [[Rcpp::export]]
Rcpp::List mycofon_balence(double Allocation_C_CASSIA,
                           double N_to_CASSIA,
                           double root_mass,
                           double fungal_mass,
                           double optimal_root_fungal_biomass_ratio,
                           double C_roots,
                           double N_roots,
                           double C_fungal,
                           double N_fungal,
                           double turnover_roots,
                           double turnover_fungal,
                           double respiration,
                           double C_allocation,
                           double N_allocation){
  
  // Mycorrhizal rate
  double m = fungal_mass / (root_mass * optimal_root_fungal_biomass_ratio);
  if (m > 1) {
    std::cout << "Warning: The mycorhization value (m) is more than 1 - check the fungal_mass, root_mass and optimal_root_fungal_biomass_ratio values"; 
  }
  // dC^r/dt TODO: this need to be linked with the CASSIA C sections
  C_roots = C_roots + Allocation_C_CASSIA - (1 - m)*C_roots*turnover_fungal - m*C_roots*turnover_roots - respiration*C_roots - C_allocation;
  // dC^f/dt
  C_fungal = C_fungal + C_allocation - turnover_fungal*fungal_mass - respiration*fungal_mass;
  
  // Nitrogen!
  // TODO: should the N in the roots in different forms be differentiated - or can it be grouped at this stage?
  double root_NC_ratio = N_roots / C_roots;
  double fungal_NC_ratio = N_fungal / C_fungal;
  //dN^r/dt
  N_roots = N_allocation + N_roots - turnover_roots*C_roots*root_NC_ratio - N_to_CASSIA;
  //dN^f/dt
  N_fungal = N_fungal - turnover_fungal*C_fungal*fungal_NC_ratio - N_allocation;
  
  return Rcpp::List::create(Rcpp::_["C_roots"] = C_roots,
                            Rcpp::_["C_fungal"] = C_fungal,
                            Rcpp::_["N_roots"] = N_roots,
                            Rcpp::_["N_fungal"] = N_fungal);
}




