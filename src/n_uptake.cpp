#include <Rcpp.h>
#include <math.h>
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
double uptake_organic_N(double N_org, double T, double N_org_limit, double k) {
  double u = k*pow(N_org, 8) / (pow(N_org_limit, 8) + pow(N_org, 8));
  return(u);
}


// [[Rcpp::export]]
double uptake_NH4(double NH4, double T, double NH4_limit, double k) {
  double u = k * pow(NH4, 8) / (pow(NH4_limit, 8) + pow(NH4, 8));
  return(u);
}


// [[Rcpp::export]]
double uptake_NO3(double NO3, double T, double NO3_limit, double k) {
  double u = k * pow(NO3, 8) / (pow(NO3_limit, 8) + pow(NO3, 8));
  return(u);
}


// [[Rcpp::export]]
double Plant_N_Uptake(std::vector<double> N_in_root_R, 
                      std::vector<double> N_into_plant_R, 
                      double C_in_root, 
                      double NC_in_root_opt, 
                      double T, 
                      std::vector<double> N_limits_R) {
  
  // Input the parameters!
  N_balence N_in_root = vector_to_N_balence(N_in_root_R);
  N_balence N_into_plant = vector_to_N_balence(N_into_plant_R);
  N_balence N_limits = vector_to_N_balence(N_limits_R);
  
  double NC_in_root = (N_in_root.Norg + N_in_root.NH4 + N_in_root.NO3)/C_in_root;
  // Uptake of the inorganic has no interactions together
  double N_to_root = (1 - NC_in_root/NC_in_root_opt)*(uptake_organic_N(N_into_plant.Norg, T, N_limits.Norg, 1) + 
                                                      uptake_NH4(N_into_plant.NH4, T, N_limits.NH4, 1) + 
                                                      uptake_NO3(N_into_plant.NO3, T, N_limits.NO3, 1));
  return(N_to_root);
}



