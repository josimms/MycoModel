#include "mycomodel.h"

// [[Rcpp::export]]
Rcpp::List myco_growth(double C_fungal,
                       double N_fungal,
                       double a,
                       double b)
{
  
  double carbon_used;
  double nitrogen_used;
  double out = std::min(a*C_fungal, b*N_fungal); // The growth is just a experiential function, limited by the element
  if (out == a*C_fungal) {
    carbon_used = a*C_fungal;
    nitrogen_used = b*N_fungal*(a*C_fungal)/(b*N_fungal);
  } else if (out == b*N_fungal) {
    carbon_used = a*C_fungal*(b*N_fungal)/(a*C_fungal);
    nitrogen_used = b*N_fungal;
  }
  
  return Rcpp::List::create(Rcpp::_["growth"] = out,
                            Rcpp::_["carbon_used"] = carbon_used,
                            Rcpp::_["nitrogen_used"] = nitrogen_used);
}