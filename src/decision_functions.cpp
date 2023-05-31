#include "mycomodel.h"

// [[Rcpp::export]]
Rcpp::List plant_decision(double C_roots,
                          double N_roots,
                          double NC_in_root_opt) {
    double out;
    if (C_roots/N_roots > NC_in_root_opt) {
      out = 1;
    } else {
      out = 0;
    }
    
    double demand = 1;
    
    return(Rcpp::List::create(Rcpp::_["demand"] = demand,
                              Rcpp::_["C_given"] = out));
}

// [[Rcpp::export]]
Rcpp::List myco_decision(double C_fungal,
                         double N_fungal,
                         double NC_fungal_opt,
                         double mantle_mass,
                         double ERM_mass,
                         double percentage_C_biomass,
                         double C_value_param,
                         double N_value_param) {
  
  double out;
  if (C_fungal/N_fungal > NC_fungal_opt) {
    out = 1;
  } else {
    out = 0;
  }
  
  double NC_in_fungal = C_fungal/N_fungal;
  double demand = (C_fungal/percentage_C_biomass)*(mantle_mass/ERM_mass)*(1 - (NC_in_fungal)/(NC_fungal_opt)); // TODO: check the formula;
  double C_value = C_value_param*C_fungal;
  double N_value = N_value_param*N_fungal;
  
  return(Rcpp::List::create(Rcpp::_["demand"] = demand,
                            Rcpp::_["C_value"] = C_value,
                            Rcpp::_["N_value"] = N_value,
                            Rcpp::_["N_given"] = out));
}