#include "mycomodel.h"

// [[Rcpp::export]]
double respiration(double Tmb, double a, double b)
{
  double out = a * exp(b*Tmb);
  return(out);
}