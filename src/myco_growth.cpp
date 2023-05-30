#include "mycomodel.h"

// [[Rcpp::export]]
double myco_growth(double C_fungal,
                   double N_fungal,
                   double a,
                   double b)
{
  
  double out = std::min(a*C_fungal, b*N_fungal); // The growth is just a experiential function, limited by the element
  
  return(out);
}