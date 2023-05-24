/*
 * HEADERFILES
 */
#include <vector>
#include <cmath>
#include <Rcpp.h>

/*
 * STURCTURES DEFINED HERE
 */

struct organism
{
  std::vector<double> transfer_timeseries;
  std::vector<double> total_n;
  std::vector<double> total_c;
  std::vector<double> input;
  std::vector<double> transfer_rate; // This is from literature
  double value_lower_bound; // REM: could make this a vector later
  double baseline_lower_bound; // REM: could make this a vector later
};

struct ecosystem
{
  organism tree;
  organism myco;
};

struct N_balence
{
  double NH4;
  double NO3;
  double Norg;
  double C;
  double Norg_FOM;
};

struct symphony_parameters{
  double A;
  double s;
  double r;
  double m_p;
  double e_p;
  double r_p;
  double k;
  double alpha;
  double beta;
  double i;
  double l;
  double y;
  double u;
  double e;
  double Ca;
  double psi_i;
};

struct soil_balence{
  std::vector<double> C_p;
  std::vector<double> C_f;
  std::vector<double> C_df;
  std::vector<double> C_ds;
  std::vector<double> N;
  std::vector<double> C_s;
};

/*
 * FUNCTIONS DEFINED HERE - WHEN THEY ARE NEEDED BETWEEN FILES (reference above them)
 */

// FILE: general_functions.cpp

#ifndef PKG_leap_year_H
#define PKG_leap_year_H

int leap_year(int year);

#endif

// FILE: respiration.cpp

#ifndef PKG_respiration_H
#define PKG_respiration_H

double respiration(double Tmb, double a, double b);

#endif

// FILE: myco_growth.cpp

double myco_growth(double sugar, double Tmb, double N);

// FILE: n_uptake.cpp

#ifndef PKG_vector_to_N_balence_H
#define PKG_vector_to_N_balence_H

N_balence vector_to_N_balence(std::vector<double> input);

#endif

#ifndef PKG_Plant_N_Uptake_H
#define PKG_Plant_N_Uptake_H

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
                      double N_allocation);
#endif

#ifndef PKG_Fungal_N_Uptake_H
#define PKG_Fungal_N_Uptake_H

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
                       std::vector<double> SWC_k_R);

#endif

#ifndef PKG_Microbe_Uptake_H
#define PKG_Microbe_Uptake_H

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
                          std::vector<double> SWC_k_R,
                          bool SOM_decomposers,
                          double Norg_avaliable_FOM);
  
#endif

// FILE: symphony_model_plus.cpp

#ifndef PKG_vector_to_symphony_H
#define PKG_vector_to_symphony_H

symphony_parameters vector_to_symphony(std::vector<double> input);

#endif
