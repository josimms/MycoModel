#include "mycomodel.h"

// [[Rcpp::export]]
Rcpp::List myco_growth(double C_fungal_in,
                       double N_fungal_in,
                       double a,
                       double b)
{
  
  double carbon_used;
  double nitrogen_used;
  double out = std::min(a*C_fungal_in, b*N_fungal_in); // The growth is just a experiential function, limited by the element
  if (out == a*C_fungal_in) {
    carbon_used = a*C_fungal_in;
    nitrogen_used = b*N_fungal_in*(a*C_fungal_in)/(b*N_fungal_in);
  } else if (out == b*N_fungal_in) {
    carbon_used = a*C_fungal_in*(b*N_fungal_in)/(a*C_fungal_in);
    nitrogen_used = b*N_fungal_in;
  }
  
  return Rcpp::List::create(Rcpp::_["growth"] = out,
                            Rcpp::_["carbon_used"] = carbon_used,
                            Rcpp::_["nitrogen_used"] = nitrogen_used);
}