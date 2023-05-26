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
  std::vector<double> respiration_microbes_paramsM;
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

struct parameters // A collection of parameters that I want as input function - but these should probably be reordered at some point
{
  double m;
  double microbe_turnover;
  double NC_in_root_opt;
  double NC_fungal_opt;
  double NC_microbe_opt;
  double percentage_C_biomass;
  std::vector<double> N_limits_plant;
  std::vector<double> N_limits_fungal;
  std::vector<double> N_limits_microbes;
  std::vector<double> N_k_plant;
  std::vector<double> N_k_fungal;
  std::vector<double> N_k_microbes;
  std::vector<double> SWC_k_plant;
  std::vector<double> SWC_k_fungal;
  std::vector<double> SWC_k_microbes;
  std::vector<double> NH4_on_NO3;
  std::vector<double> respiration_microbes_params;
};

struct DECISION_output
{
  double NH4_used_Plant;
  double NO3_used_Plant;
  double FOM_Norg_used_Plant;
  double NH4_used_Fungal;
  double NO3_used_Fungal;
  double FOM_Norg_used_Fungal;
  double N_used_Plant_total;
};

struct CASSIA_output
{
  double C_roots;
  double N_roots;
  double Litter_needles;
  double Litter_woody;
  double Litter_roots;
  double Litter_mycelium; 
};

struct MYCOFON_output {
  double C_fungal;
  double N_fungal;
  double mantle_mass;
  double ERM_mass;
};

struct SYMPHONY_output {
  double NH4;
  double NO3;
  double N_FOM;
  double C_FOM_needles;
  double C_FOM_woody;
  double C_FOM_roots;
  double C_FOM_mycelium;
  double C_SOM;
  double C_decompose_FOM;
  double C_decompose_SOM;
  double N_decompose_FOM;
  double N_decompose_SOM;
  double SOM_Norg_used;
};


/*
 * FUNCTIONS DEFINED HERE - WHEN THEY ARE NEEDED BETWEEN FILES (reference above them)
 */

// FILE: general_functions.cpp

#ifndef PKG_leap_year_H
#define PKG_leap_year_H

int leap_year(int year);

#endif

// FILE: Toy_Model.cpp

#ifndef PKG_Toy_Model_H
#define PKG_Toy_Model_H

parameters parameters_initalise(std::vector<double> parameters_R);

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

#ifndef PKG_list_to_N_balence_H
#define PKG_list_to_N_balence_H

N_balence list_to_N_balence(Rcpp::List input);
  
#endif 

#ifndef PKG_Plant_N_Uptake_H
#define PKG_Plant_N_Uptake_H

Rcpp::List Plant_N_Uptake(double NC_in_root_opt, 
                          double T,                           // UNITS: 'C
                          double SWC,                         // UNITS: %
                          double m,                           // UNITS: %
                          double NH4_in,    // TODO: make this have in input that works with the output of the soil function
                          double NO3_in,
                          double FOM_in,
                          std::vector<double> N_limits_R,
                          std::vector<double> N_k_R,
                          std::vector<double> SWC_k_R,
                          double C_roots,                     // UNITS: C kg
                          double N_roots,
                          double percentage_C_biomass,        // UNITS: %
                          double N_allocation,                // UNITS: C kg
                          std::vector<double> parameters);

#endif

#ifndef PKG_Fungal_N_Uptake_H
#define PKG_Fungal_N_Uptake_H

Rcpp::List Fungal_N_Uptake(double C_fungal,
                           double N_fungal,
                           double NC_fungal_opt,
                           double mantle_mass,
                           double ERM_mass,
                           double percentage_C_biomass,
                           double T,
                           double SWC,
                           double NH4,
                           double NO3,
                           double FOM_Norg,
                           std::vector<double> N_limits_R,
                           std::vector<double> N_k_R,
                           std::vector<double> SWC_k_R);

#endif

#ifndef PKG_Microbe_Uptake_H
#define PKG_Microbe_Uptake_H

Rcpp::List Microbe_Uptake(double C_microbe,                   // UNITS: C kg
                          double N_micorbe,                   // UNITS: C kg
                          double NC_microbe_opt,              // UNITS: %
                          double NH4_avaliable,               // UNITS: C kg
                          double NO3_avaliable,               // UNITS: C kg
                          double Norg_avaliable,              // UNITS: C kg
                          double T,                           // UNITS: 'C
                          double SWC,                         // UNITS: %
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


#ifndef PKG_symphony_multiple_FOM_daily_H
#define PKG_symphony_multiple_FOM_daily_H

Rcpp::List symphony_multiple_FOM_daily(double Tmb,                  // UNITS: 'C
                                       double SWC,                  // UNITS; mm
                                       double C_FOM_needles,        // UNITS: C kg, FROM: CASSIA
                                       double C_FOM_woody,          // UNITS: C kg, FROM: CASSIA
                                       double C_FOM_roots,          // UNITS: C kg, FROM: CASSIA
                                       double C_FOM_mycelium,       // UNITS: C kg, FROM: CASSIA
                                       double C_SOM,                // UNITS: C kg, FROM: CASSIA
                                       double C_decompose_FOM,      // UNITS: C kg, FROM: here, last iteration
                                       double C_decompose_SOM,      // UNITS: C kg, FROM: here, last iteration
                                       double N_decompose_FOM,      // UNITS: C kg, FROM: here, last iteration
                                       double N_decompose_SOM,      // UNITS: C kg, FROM: here, last iteration
                                       double Litter_needles,       // UNITS: C kg, FROM: CASSIA
                                       double Litter_woody,         // UNITS: C kg, FROM: CASSIA
                                       double Litter_roots,         // UNITS: C kg, FROM: CASSIA
                                       double Litter_mycelium,      // UNITS: C kg, FROM: Mycofon
                                       double NH4,                  // UNITS: C kg, FROM: here, last iteration
                                       double NO3,                  // UNITS: C kg, FROM: here, last iteration
                                       double NH4_used_Plant,       // UNITS: C kg, FROM: n_uptake
                                       double NH4_used_Fungal,      // UNITS: C kg, FROM: n_uptake
                                       double NO3_used_Plant,       // UNITS: C kg, FROM: n_uptake
                                       double NO3_used_Fungal,      // UNITS: C kg, FROM: n_uptake
                                       double FOM_Norg_used_Plant,  // UNITS: C kg, FROM: n_uptake
                                       double FOM_Norg_used_Fungal, // UNITS: C kg, FROM: n_uptake
                                       double SOM_Norg_used,        // UNITS: C kg, FROM: n_uptake
                                       std::vector<double> respiration_microbes_params,
                                       std::vector<double> N_limits_R,
                                       std::vector<double> N_k_R,
                                       std::vector<double> SWC_k_R,
                                       double NC_microbe_opt,       // Assume that this is the same as the above number for the moment 
                                       double microbe_turnover);

#endif