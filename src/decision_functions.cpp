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
                         double NC_in_fungal_opt) {
  double out;
  if (C_fungal/N_fungal > NC_in_fungal_opt) {
    out = 1;
  } else {
    out = 0;
  }
  
  double demand = 1;
  
  return(Rcpp::List::create(Rcpp::_["demand"] = demand,
                            Rcpp::_["N_given"] = out));
}