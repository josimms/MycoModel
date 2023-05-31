/*
 * HEADERFILES
 */

#include <vector>
#include <cmath>
#include <Rcpp.h>

struct N_balence
{
  double NH4;
  double NO3;
  double Norg;
  double C;
  double Norg_FOM;
};

struct respiration_parameters
{
  double plant_a;
  double plant_b;
  double fungal_a;
  double fungal_b;
  double micorbe_a;
  double micorbe_b;
};


#ifndef PKG_plant_decision_H
#define PKG_plant_decision_H

Rcpp::List plant_decision(double C_roots,
                          double N_roots,
                          double NC_in_root_opt,
                          double C_value_param,
                          double N_value_param);

#endif

#ifndef PKG_myco_decision_H
#define PKG_myco_decision_H

Rcpp::List myco_decision(double C_fungal,
                         double N_fungal,
                         double NC_fungal_opt,
                         double mantle_mass,
                         double ERM_mass,
                         double percentage_C_biomass,
                         double C_value_param,
                         double N_value_param);

#endif
