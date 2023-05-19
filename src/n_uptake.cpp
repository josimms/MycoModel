#include <Rcpp.h>
using namespace Rcpp;

struct N_balence
{
  double NH4;
  double NO3;
  double Norg;
};

// Input R values in the correct structure - N_balence
N_balence vector_to_N_balence(std::vector<double> input) {
  N_balence params;
  
  params.NH4 = input[0];
  params.NO3 = input[1];
  params.Norg = input[2];
  
  return(params);
}


// [[Rcpp::export]]
double uptake_organic_N(double N_org, double T, double N_org_limit, double k, double SWC, double SWC_k) {
  // Concentration
  double u = k * pow(N_org, 8) / (pow(N_org_limit, 8) + pow(N_org, 8));
  // Temperature
  double u_t = (T+20)/55;
  // Water
  double u_w = SWC_k * pow(SWC, 8) / (pow(0.5, 8) + pow(SWC, 8));
  double all = u * u_t * u_w;
  return(all);
}


// [[Rcpp::export]]
double uptake_NH4(double NH4, double T, double NH4_limit, double k, double SWC, double SWC_k) {
  double u = k * pow(NH4, 8) / (pow(NH4_limit, 8) + pow(NH4, 8));
  // Temperature
  double u_t = (T+20)/55;
  // Water
  double u_w = SWC_k * pow(SWC, 8) / (pow(0.5, 8) + pow(SWC, 8));
  double all = u * u_t * u_w;
  return(all);
}


// [[Rcpp::export]]
double uptake_NO3(double NO3, double T, double NO3_limit, double k, double SWC, double SWC_k) {
  // TODO: Uptake from the plant at least should depend on the lack of NH4 not the concentration of NH3 completely!
  double u = k * pow(NO3, 8) / (pow(NO3_limit, 8) + pow(NO3, 8)) - k;
  // Temperature
  double u_t = (T+20)/55;
  // Water
  double u_w = SWC_k * pow(SWC, 8) / (pow(0.5, 8) + pow(SWC, 8));
  double all = u * u_t * u_w;
  return(all);
}

// [[Rcpp::export]]
double Plant_N_Uptake(std::vector<double> N_in_root_R, 
                      std::vector<double> N_in_soil_R, 
                      double C_in_root, 
                      double NC_in_root_opt, 
                      double T,
                      double SWC,
                      std::vector<double> N_limits_R,
                      std::vector<double> N_k_R,
                      std::vector<double> SWC_k_R) {
  
  // Input the parameters!
  N_balence N_in_root = vector_to_N_balence(N_in_root_R);
  N_balence N_in_soil = vector_to_N_balence(N_in_soil_R);
  N_balence N_limits = vector_to_N_balence(N_limits_R);
  N_balence N_k = vector_to_N_balence(N_k_R);
  N_balence SWC_k = vector_to_N_balence(SWC_k_R);
  
  double NC_in_root = (N_in_root.Norg + N_in_root.NH4 + N_in_root.NO3)/C_in_root;
  
  double demand = 1 - NC_in_root/NC_in_root_opt;
  if (demand < 0) {
    std::cout << "Warning: demand is negative, rethink optimal root value (NC_in_root_opt) value. \n";
  }
  
  // Uptake of the inorganic has no interactions together
  double N_to_root = demand * (uptake_organic_N(N_in_soil.Norg, T, N_limits.Norg, N_k.Norg, SWC, SWC_k.Norg) + 
                                uptake_NH4(N_in_soil.NH4, T, N_limits.NH4, N_k.NH4, SWC, SWC_k.NH4) + 
                                uptake_NO3(N_in_soil.NO3, T, N_limits.NO3, N_k.NO3, SWC, SWC_k.NO3));
  return(N_to_root);
}


// [[Rcpp::export]]
double Fungal_N_Uptake(double C_roots,
                       double N_roots,
                       double C_fungal,
                       double N_fungal,
                       double NC_fungal_opt,
                       double mantle_mass,
                       double ERM_mass,
                       double N_avaliable,
                       double N_to_CASSIA,
                       double T,
                       double SWC,
                       std::vector<double> N_in_soil_R,
                       std::vector<double> N_limits_R,
                       std::vector<double> N_k_R,
                       std::vector<double> SWC_k_R) {
  
  // TODO: make the N_available link to the uptake equations that I have made!
  
  N_balence N_in_soil = vector_to_N_balence(N_in_soil_R);
  N_balence N_limits = vector_to_N_balence(N_limits_R);
  N_balence N_k = vector_to_N_balence(N_k_R);
  N_balence SWC_k = vector_to_N_balence(SWC_k_R);
  
  double NC_in_fungal = N_fungal/C_fungal;
  // TODO: Parallel or intercecting?
  double N_fungal_uptake = (uptake_organic_N(N_in_soil.Norg, T, N_limits.Norg, N_k.Norg, SWC, SWC_k.Norg) + 
                            uptake_NH4(N_in_soil.NH4, T, N_limits.NH4, N_k.NH4, SWC, SWC_k.NH4) + 
                            uptake_NO3(N_in_soil.NO3, T, N_limits.NO3, N_k.NO3, SWC, SWC_k.NO3))*
                            (C_fungal/(C_roots))*(mantle_mass/ERM_mass)*N_avaliable*(1 - (NC_in_fungal)/(NC_fungal_opt));
  
  return(N_fungal_uptake);
}







