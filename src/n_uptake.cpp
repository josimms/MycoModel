#include "mycomodel.h"


// Input R values in the correct structure - N_balence
N_balence vector_to_N_balence(std::vector<double> input) {
  N_balence params;
  
  params.NH4 = input[0];
  params.NO3 = input[1];
  params.Norg = input[2];
  params.C = input[3];
  params.Norg_FOM = input[4];
  
  return(params);
}

// Input R values in the correct structure - N_balence
N_balence list_to_N_balence(Rcpp::List input) {
  N_balence params;
  
  params.NH4 = input[0];
  params.NO3 = input[1];
  params.Norg = input[2];
  params.C = input[3];
  params.Norg_FOM = input[4];
  
  return(params);
}

// [[Rcpp::export]]
double uptake_N(double N,   // UNITS: C kg
                double T,       // UNITS: 'C
                double N_limit, 
                double k, 
                double SWC,     // UNITS: %
                double SWC_k) {

  // Concentration
  double u = k * pow(N, 8) / (pow(N_limit, 8) + pow(N, 8));
  // Temperature
  double u_t = (T+20)/55;
  // Water
  double u_w = SWC_k * pow(SWC, 8) / (pow(0.5, 8) + pow(SWC, 8));
  double all = u * u_t * u_w;
  return(all);
}


// [[Rcpp::export]]
double uptake_C(double C,     // UNITS: C kg 
                double T,     // UNITS: 'C
                double C_limit, 
                double k, 
                double SWC,   // UNITS: %
                double SWC_k) {
  
  double u = k * pow(C, 8) / (pow(C_limit, 8) + pow(C, 8)) - k;
  // Temperature
  double u_t = (T+20)/55;
  // Water
  double u_w = SWC_k * pow(SWC, 8) / (pow(0.5, 8) + pow(SWC, 8));
  double all = u * u_t * u_w;
  return(all);
}

// [[Rcpp::export]]
Rcpp::List Plant_N_Uptake(double NC_in_root_opt, 
                          double T,
                          double SWC,
                          double m, 
                          double NH4_in,
                          double NO3_in,
                          double FOM_in,
                          std::vector<double> N_limits_R,
                          std::vector<double> N_k_R,
                          std::vector<double> SWC_k_R,
                          double C_roots,
                          double N_roots,
                          double C_fungal,
                          double percentage_C_biomass,
                          std::vector<double> parameters,
                          double C_value_param,
                          double N_value_param) {
  
  // Input the parameters!
  N_balence N_limits = vector_to_N_balence(N_limits_R);
  N_balence N_k = vector_to_N_balence(N_k_R);
  N_balence SWC_k = vector_to_N_balence(SWC_k_R);
  
  // STEP 1: Pure uptake
  // Assume that the organic and inorganic uptake is parallel
  // Inorganic NH4 affects NO3, in this function as it is a specific plant effect
  double NH4_effect_on_NO3 = parameters[0] * pow(NH4_in, 8) / (pow(parameters[1], 8) + pow(NH4_in, 8));
  
  double demand = plant_decision(C_roots, N_roots, NC_in_root_opt, C_value_param, N_value_param)[0];
  
  // All possible N to root with NH4 modifier for NO3
  double N_to_root = (uptake_N(FOM_in, T, N_limits.Norg, N_k.Norg, SWC, SWC_k.Norg) + 
    uptake_N(NH4_in, T, N_limits.NH4, N_k.NH4, SWC, SWC_k.NH4) + 
    NH4_effect_on_NO3*uptake_N(NO3_in, T, N_limits.NO3, N_k.NO3, SWC, SWC_k.NO3));
  
  // STEP 2: Uptake with demand
  double N_to_root_max = N_to_root * (C_fungal/percentage_C_biomass) * (1 - m) * demand;
  
  // STEP 3: 
  double NC_in_root = 1;
  double N_to_plant = std::min(N_to_root_max, (NC_in_root_opt - NC_in_root)*N_to_root_max); // TODO: reconsider the conditions here
  double reduction_factor;
  if (N_to_plant == N_to_root_max) {
    reduction_factor = 1;
  } else if (N_to_plant == (NC_in_root_opt - NC_in_root)*N_to_root_max) {
    reduction_factor = (NC_in_root_opt - NC_in_root)*N_to_root_max/N_to_root_max;
  } else {
    std::cout << "Warning:\nReduction factor not working in Planr_N_Intake\n";
  }
  
  
  return(Rcpp::List::create(Rcpp::_["N_to_plant"] = N_to_plant,
                            Rcpp::_["NH4_used"] = uptake_N(NH4_in, T, N_limits.NH4, N_k.NH4, SWC, SWC_k.NH4)*reduction_factor,
                            Rcpp::_["NO3_used"] = NH4_effect_on_NO3*uptake_N(NO3_in, T, N_limits.NO3, N_k.NO3, SWC, SWC_k.NO3)*reduction_factor,
                            Rcpp::_["Norg_used"] = uptake_N(FOM_in, T, N_limits.Norg, N_k.Norg, SWC, SWC_k.Norg)*reduction_factor));
}

   

// [[Rcpp::export]]
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
                           std::vector<double> SWC_k_R,
                           double C_value_param,
                           double N_value_param) {
  

  N_balence N_limits = vector_to_N_balence(N_limits_R);
  N_balence N_k = vector_to_N_balence(N_k_R);
  N_balence SWC_k = vector_to_N_balence(SWC_k_R);
  
  double NC_in_fungal = N_fungal/C_fungal;
  // Demand
  double demand = myco_decision(C_fungal,
                                N_fungal,
                                NC_fungal_opt,
                                mantle_mass,
                                ERM_mass,
                                percentage_C_biomass,
                                C_value_param,
                                N_value_param)[0];
  
  double N_fungal_uptake = (uptake_N(FOM_Norg, T, N_limits.Norg, N_k.Norg, SWC, SWC_k.Norg) + 
                            uptake_N(NH4, T, N_limits.NH4, N_k.NH4, SWC, SWC_k.NH4) + 
                            uptake_N(NO3, T, N_limits.NO3, N_k.NO3, SWC, SWC_k.NO3))*demand;
  
  return Rcpp::List::create(Rcpp::_["N_to_fungal"] = N_fungal_uptake,
                            Rcpp::_["NH4_used"] = uptake_N(NH4, T, N_limits.NH4, N_k.NH4, SWC, SWC_k.NH4)*demand,
                            Rcpp::_["NO3_used"] = uptake_N(NO3, T, N_limits.NO3, N_k.NO3, SWC, SWC_k.NO3)*demand,
                            Rcpp::_["Norg_used"] = uptake_N(FOM_Norg, T, N_limits.Norg, N_k.Norg, SWC, SWC_k.Norg)*demand);
}


// [[Rcpp::export]]
Rcpp::List Microbe_Uptake(double C_microbe,                   // UNITS: C kg
                          double N_micorbe,                   // UNITS: C kg eq
                          double NC_microbe_opt,              // UNITS: %
                          double NH4_avaliable,               // UNITS: C kg eq
                          double NO3_avaliable,               // UNITS: C kg eq
                          double Norg_avaliable,              // UNITS: C kg eq
                          double T,                           // UNITS: 'C
                          double SWC,                         // UNITS: %
                          std::vector<double> N_limits_R,
                          std::vector<double> N_k_R,
                          std::vector<double> SWC_k_R,
                          bool SOM_decomposers,
                          double Norg_avaliable_FOM,
                          std::vector<double> respiration_microbes_params) {
  

  /*
   * Nitrogen limitation // Mass limitation is in the soil model!
   */
  
  N_balence N_limits = vector_to_N_balence(N_limits_R);
  N_balence N_k = vector_to_N_balence(N_k_R);
  N_balence SWC_k = vector_to_N_balence(SWC_k_R);
  
  double NH4_uptaken;     // UNITS: C kg
  double NO3_uptaken;     // UNITS: C kg
  double Norg_uptaken;    // UNITS: C kg
  double C_uptaken;       // UNITS: C kg
  
  // Working out the current N:C ratio
  double NC_in_micorbe = N_micorbe/C_microbe;        // UNITS: C kg eq
  
  // purely organic uptake
  double N_micorbe_uptake = uptake_N(Norg_avaliable, T, N_limits.Norg, N_k.Norg, SWC, SWC_k.Norg)*(1 - (NC_in_micorbe)/(NC_microbe_opt));           // UNITS: C kg eq
  
  /*
   * Carbon limitation // Mass limitation is in the soil model!
   */
  
  // The respiration is taken into account here so the microbes don't run out of C
  double C_microbe_uptake = uptake_C(Norg_avaliable, T, N_limits.C, N_k.C, SWC, SWC_k.C) - respiration(T, respiration_microbes_params[1], respiration_microbes_params[2]);       // UNITS: C kg
  
  /*
   * Uptake organic
   */
  
  // TODO: need to find the mass relationship here - also for the following code to work need to assume that they are in the same units
  double organic_limitation = std::min(N_micorbe_uptake, C_microbe_uptake);        // UNITS: C kg
  
  if (organic_limitation == N_micorbe_uptake) {
    Norg_uptaken = uptake_N(Norg_avaliable, T, N_limits.Norg, N_k.Norg, SWC, SWC_k.Norg)*(1 - (NC_in_micorbe)/(NC_microbe_opt));
    C_uptaken = C_microbe_uptake - (C_microbe_uptake - N_micorbe_uptake);       // UNITS: C kg
  } else if (organic_limitation == C_microbe_uptake) {
    Norg_uptaken = uptake_N(Norg_avaliable, T, N_limits.Norg, N_k.Norg, SWC, SWC_k.Norg)*(C_microbe_uptake/N_micorbe_uptake)*(1 - (NC_in_micorbe)/(NC_microbe_opt));
    C_uptaken = C_microbe_uptake;       // UNITS: C kg
  } else {
    std::cout << "Warning:\nOrganic uptake of Microbe_Uptake doesn't match either limitation!\n";
  }
  
  /*
   * Uptake Inorganic
   */
  
  // Uptake NC ratio
  NC_in_micorbe = (N_micorbe+Norg_uptaken)/(C_microbe+C_uptaken); 
  
  // uptake enough inorganic to balance their internal NC ratio
  // As there is not an update here, accept that this is an overestimate here - could add a dapening factor if need too
  // Not having an update means that they will be considered equal.
  NH4_uptaken = uptake_N(NH4_avaliable, T, N_limits.NH4, N_k.NH4, SWC, SWC_k.NH4)*(1 - (NC_in_micorbe)/(NC_microbe_opt));        // UNITS: C kg
  NO3_uptaken = uptake_N(NO3_avaliable, T, N_limits.NO3, N_k.NO3, SWC, SWC_k.NO3)*(1 - (NC_in_micorbe)/(NC_microbe_opt));        // UNITS: C kg
  
  /*
   * Organic Uptake Again if SOM
   */
  
  double Norg_uptaken_extra_FOM;
  if (SOM_decomposers) {
    Norg_uptaken_extra_FOM = uptake_N(Norg_avaliable_FOM, T, N_limits.Norg_FOM, N_k.Norg_FOM, SWC, SWC_k.Norg_FOM)*(1 - (NC_in_micorbe)/(NC_microbe_opt));        // UNITS: C kg
  }
  
  /*
   * Output
   */
  
  return(Rcpp::List::create(Rcpp::_["NH4_uptaken"] = NH4_uptaken,                                  // UNITS: C kg
                            Rcpp::_["NO3_uptaken"] = NO3_uptaken,                                  // UNITS: C kg
                            Rcpp::_["Norg_uptaken"] = Norg_uptaken,                                // UNITS: C kg
                            Rcpp::_["C_uptaken"] = C_uptaken,                                      // UNITS: C kg
                            Rcpp::_["Norg_uptaken_extra_FOM"] = Norg_uptaken_extra_FOM));          // UNITS: C kg
}






