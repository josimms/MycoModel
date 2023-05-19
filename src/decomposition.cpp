/*
// [[Rcpp::export]]
double c_uptake_micobes(double C, double T, double C_limit, double k) {
  // Aimed for the microbes
  double u = k * power(C, 8) / (power(C_limit, 8) + power(C, 8));
  return(u);
}

// [[Rcpp::export]]
double c_uptake_mycorrhiza(double C, 
                           double T,
                           double C_limit, 
                           double k, 
                           double NC_in_mycorrhiza_opt, 
                           double allocation,
                           double C_in_mycorrhiza,
                           N_balence N_in_mycorrhiza,
                           N_balence N_limits) {
  // Aimed for the microbes
  double NC_in_mycorrhiza = (N_in_mycorrhiza.Norg + N_in_mycorrhiza.NH4 + N_in_mycorrhiza.NO3)/C_in_mycorrhiza;
  // Allocation should be from CASSIA
  double u = allocation + (1 - NC_in_mycorrhiza/NC_in_mycorrhiza_opt)*(uptake_organic_N(N_in_mycorrhiza.Norg, T, N_limits.Norg) + 
                                                                       uptake_NH4(N_in_mycorrhiza.NH4, T, N_limits.NH4) + 
                                                                       uptake_NO3(N_in_mycorrhiza.NO3, T, N_limits.NO3));
  return(u);
}
 */