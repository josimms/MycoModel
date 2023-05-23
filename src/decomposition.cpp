#include "mycomodel.h"

// [[Rcpp::export]]
double c_uptake_micobes(double C, double T, double C_limit, double k) {
  // Aimed for the microbes
  double u = k * power(C, 8) / (power(C_limit, 8) + power(C, 8));
  return(u);
}