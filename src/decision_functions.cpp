// [[Rcpp::export]]
double plant_decision(double C_roots,
                      double N_roots,
                      double NC_in_root_opt) {
  double out;
  if (C_roots/N_roots > NC_in_root_opt) {
    out = 1;
  } else {
    out = 0;
  }
  return(out);
}

// [[Rcpp::export]]
double myco_decision(double C_fungal,
                     double N_fungal,
                     double NC_in_fungal_opt) {
  double out;
  if (C_fungal/N_fungal > NC_in_fungal_opt) {
    out = 1;
  } else {
    out = 0;
  }
  return(out);
}