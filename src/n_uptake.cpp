#include <Rcpp.h>
#include "mycomodel.h"
using namespace Rcpp;

// Input R values in the correct structure - N_balence
N_balence vector_to_N_balence(std::vector<double> input) {
  N_balence params;
  
  params.NH4 = input[0];
  params.NO3 = input[1];
  params.Norg = input[2];
  params.C = input[3];
  
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
double uptake_C(double C, double T, double C_limit, double k, double SWC, double SWC_k) {
  // TODO: Uptake from the plant at least should depend on the lack of NH4 not the concentration of NH3 completely!
  double u = k * pow(C, 8) / (pow(C_limit, 8) + pow(C, 8)) - k;
  // Temperature
  double u_t = (T+20)/55;
  // Water
  double u_w = SWC_k * pow(SWC, 8) / (pow(0.5, 8) + pow(SWC, 8));
  double all = u * u_t * u_w;
  return(all);
}

// [[Rcpp::export]]
double Plant_N_Uptake(double C_in_root, 
                      double NC_in_root_opt, 
                      double T,
                      double SWC,
                      double m,
                      std::vector<double> N_in_root_R, 
                      std::vector<double> N_in_soil_R,
                      std::vector<double> N_limits_R,
                      std::vector<double> N_k_R,
                      std::vector<double> SWC_k_R,
                      double C_roots,
                      double C_roots_biomass,
                      double N_avaliable,
                      double N_allocation) {
  
  
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
  double N_to_root = (uptake_organic_N(N_in_soil.Norg, T, N_limits.Norg, N_k.Norg, SWC, SWC_k.Norg) + 
                      uptake_NH4(N_in_soil.NH4, T, N_limits.NH4, N_k.NH4, SWC, SWC_k.NH4) + 
                      uptake_NO3(N_in_soil.NO3, T, N_limits.NO3, N_k.NO3, SWC, SWC_k.NO3));
  
  double N_to_root_max = (uptake_organic_N(N_in_soil.Norg, T, N_limits.Norg, N_k.Norg, SWC, SWC_k.Norg) + 
                          uptake_NH4(N_in_soil.NH4, T, N_limits.NH4, N_k.NH4, SWC, SWC_k.NH4) + 
                          uptake_NO3(N_in_soil.NO3, T, N_limits.NO3, N_k.NO3, SWC, SWC_k.NO3)) *
                          (C_roots/(C_roots/C_roots_biomass)) *
                          (1 - m) *
                          N_avaliable;
  
  double out = std::min(N_to_root_max, C_roots*(NC_in_root_opt - NC_in_root - N_allocation)*N_avaliable);
  
  return(out);
}


// [[Rcpp::export]]
double Fungal_N_Uptake(double C_roots,
                       double N_roots,
                       double C_fungal,
                       double N_fungal,
                       double NC_fungal_opt,
                       double mantle_mass,
                       double ERM_mass,
                       double C_roots_biomass,
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
  double Cr_biomass = C_roots/C_roots_biomass;
  // TODO: Parallel or intercecting?
  double N_fungal_uptake = (uptake_organic_N(N_in_soil.Norg, T, N_limits.Norg, N_k.Norg, SWC, SWC_k.Norg) + 
                            uptake_NH4(N_in_soil.NH4, T, N_limits.NH4, N_k.NH4, SWC, SWC_k.NH4) + 
                            uptake_NO3(N_in_soil.NO3, T, N_limits.NO3, N_k.NO3, SWC, SWC_k.NO3))*
                            (C_fungal/Cr_biomass)*(mantle_mass/ERM_mass)*N_avaliable*(1 - (NC_in_fungal)/(NC_fungal_opt));
  
  return(N_fungal_uptake);
}


// [[Rcpp::export]]
Rcpp::List Microbe_Uptake(double C_microbe,
                      double N_micorbe,
                      double NC_microbe_opt,
                      double NH4_avaliable,
                      double NO3_avaliable,
                      double Norg_avaliable,
                      double T,
                      double SWC,
                      std::vector<double> N_in_soil_R,
                      std::vector<double> N_limits_R,
                      std::vector<double> N_k_R,
                      std::vector<double> SWC_k_R) {
  
  /*
   * Nitrogen limitation // Mass limitation is in the soil model!
   */
  
  N_balence N_in_soil = vector_to_N_balence(N_in_soil_R);
  N_balence N_limits = vector_to_N_balence(N_limits_R);
  N_balence N_k = vector_to_N_balence(N_k_R);
  N_balence SWC_k = vector_to_N_balence(SWC_k_R);
  
  double NH4_uptaken;
  double NO3_uptaken;
  double Norg_uptaken;
  double C_uptaken;
  
  double NC_in_micorbe = N_micorbe/C_microbe;
  double N_micorbe_uptake = (Norg_avaliable*uptake_organic_N(N_in_soil.Norg, T, N_limits.Norg, N_k.Norg, SWC, SWC_k.Norg) + 
                             NH4_avaliable*uptake_NH4(N_in_soil.NH4, T, N_limits.NH4, N_k.NH4, SWC, SWC_k.NH4) + 
                             NO3_avaliable*uptake_NO3(N_in_soil.NO3, T, N_limits.NO3, N_k.NO3, SWC, SWC_k.NO3))*
                             (1 - (NC_in_micorbe)/(NC_microbe_opt)); 
  
  /*
   * Carbon limitation // Mass limitation is in the soil model!
   */
  
  double C_microbe_uptake = uptake_C(N_in_soil.C, T, N_limits.C, N_k.C, SWC, SWC_k.C);
  
  /*
   * Uptake
   */
  
  double out = std::min(N_micorbe_uptake, C_microbe_uptake); // TODO: need to find the mass relationship here - also for the following code to work need to assume that they are in the same units
  
  if (out == N_micorbe_uptake) {
    NH4_uptaken = NH4_avaliable*uptake_NH4(N_in_soil.NH4, T, N_limits.NH4, N_k.NH4, SWC, SWC_k.NH4);
    NO3_uptaken = NO3_avaliable*uptake_NO3(N_in_soil.NO3, T, N_limits.NO3, N_k.NO3, SWC, SWC_k.NO3);
    Norg_uptaken = Norg_avaliable*uptake_organic_N(N_in_soil.Norg, T, N_limits.Norg, N_k.Norg, SWC, SWC_k.Norg);
    C_uptaken = C_microbe_uptake - (C_microbe_uptake - N_micorbe_uptake);
  } else if (out == C_microbe_uptake) {
    NH4_uptaken = NH4_avaliable*uptake_NH4(N_in_soil.NH4, T, N_limits.NH4, N_k.NH4, SWC, SWC_k.NH4)*(C_microbe_uptake/N_micorbe_uptake);
    NO3_uptaken = NO3_avaliable*uptake_NO3(N_in_soil.NO3, T, N_limits.NO3, N_k.NO3, SWC, SWC_k.NO3)*(C_microbe_uptake/N_micorbe_uptake);
    Norg_uptaken = Norg_avaliable*uptake_organic_N(N_in_soil.Norg, T, N_limits.Norg, N_k.Norg, SWC, SWC_k.Norg)*(C_microbe_uptake/N_micorbe_uptake);
    C_uptaken = C_microbe_uptake;
  } else {
    std::cout << "Warning:\nOutput of Microbe_Uptake doesn't make sense!";
  }
  
  return(Rcpp::List::create(Rcpp::_["NH4_uptaken"] = NH4_uptaken,
                            Rcpp::_["NO3_uptaken"] = NO3_uptaken,
                            Rcpp::_["Norg_uptaken"] = Norg_uptaken,
                            Rcpp::_["C_uptaken"] = C_uptaken));
}






