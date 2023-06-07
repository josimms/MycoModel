#include "mycomodel.h"

// [[Rcpp::export]]
double respiration(double Tmb, double Rm, double Q10)
{
  /*
   * CASSIA FUNCTION FOR ROOT RESPIRATION
   */
   double respiration = Rm * (exp(Q10/10 * Tmb) - exp(-log(Q10) / 2));
   
   double out;
   if (out <= 0) {
     out = 0;
   } else {
     out = respiration;
   }
  
  return(out);
}