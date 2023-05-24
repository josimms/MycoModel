// [[Rcpp::export]]
double myco_growth(double C_fungal,
                   double N_fungal,
                   double sugar, 
                   double N,
                   double Tmb)
{
  
  double out = sugar + N; // TODO: put some actual formula here!
  
  return(out);
}