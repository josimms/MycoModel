#include "mycomodel.h"

// [[Rcpp::export]]
double respiration(double Tmb, double Rm, double Q10)
{
  /*
   * CASSIA FUNCTION FOR ROOT RESPIRATION
   */
   double respiration = Rm * exp(Q10/10 * Tmb) - exp(-log(Q10)/2);
   
   double out;
   if (respiration > 0) {
     out = respiration;
   } else {
     out = 0;
   }
  
  return(out);
}