#include "mycomodel.h"

respiration_parameters respiration_vector_to_struct(std::vector<double> input) {
  respiration_parameters out;
  out.plant_a = input[0];
  out.plant_b = input[1];
  out.fungal_a = input[2];
  out.fungal_b = input[3];
  out.micorbe_a = input[4];
  out.micorbe_b = input[5];
  return(out);
}

// [[Rcpp::export]]
Rcpp::List mycofon_balence(double C_roots,
                           double N_roots,
                           double percentage_C_biomass,
                           double optimal_root_fungal_biomass_ratio,
                           double C_fungal,
                           double N_fungal,
                           double turnover_roots,
                           double turnover_roots_mycorrhized,
                           double turnover_mantle,
                           double turnover_ERM,
                           std::vector<double> respiration_parameters_R,
                           double NH4,
                           double NO3,
                           double FOM_Norg,
                           double NC_in_root_opt,
                           double NC_in_fungai_opt,
                           double T,
                           double Tsb,
                           double SWC,
                           std::vector<double> N_limits_Plant,
                           std::vector<double> N_k_Plant,
                           std::vector<double> SWC_k_Plant,
                           std::vector<double> N_limits_Fungal,
                           std::vector<double> N_k_Fungal,
                           std::vector<double> SWC_k_Fungal,
                           double mantle_mass,
                           double ERM_mass,
                           std::vector<double> parameters_NH4_on_NO3,
                           double growth_C,
                           double growth_N,
                           double C_value_param_myco,
                           double N_value_param_myco,
                           double C_value_param_plant,
                           double N_value_param_plant) {
  
  /*
   * Initialise parameters
   */
  respiration_parameters respiration_params = respiration_vector_to_struct(respiration_parameters_R);
  
  // Mycorrhizal rate
    // although this should be multiplied by the biomass this is taken out at this stage, as the values are assumed to be the same and therefore there is no point
  double m = N_roots / (C_roots * optimal_root_fungal_biomass_ratio); 
  if (m > 1) {
    std::cout << "Warning:\nThe mycorhization value (m) is more than 1 - check the fungal_mass, root_mass and optimal_root_fungal_biomass_ratio values"; 
  }
  
  // Plant decision, this needs to be decided before the C_roots and N_roots is recalculated
  double C_given = plant_decision(C_roots, N_roots, NC_in_root_opt, C_value_param_plant, N_value_param_plant)[3];
  double to_CASSIA = 0.1*C_roots; // could change this to be a function that would work with the gradient that is happening in CASSIA
  
  // dC^r/dt TODO: this need to be linked with the CASSIA C sections, also link the amount going to CASSIA
  C_roots = C_roots + 
    to_CASSIA - 
    (1 - m)*C_roots*turnover_roots - 
    m*C_roots*turnover_roots_mycorrhized - 
    respiration(Tsb, respiration_params.plant_a, respiration_params.plant_b)*C_roots - 
    C_given;
  
  double myco_growth_C = myco_growth(C_fungal, N_fungal, growth_C, growth_N)[1]; // TODO: currently C_roots rather than sugar as the model isn't connected in that way
  double myco_growth_N = myco_growth(C_fungal, N_fungal, growth_C, growth_N)[2];
  
  // dC^f/dt
  // TODO: what is the percentage_C_biomass doing?
  C_fungal = C_fungal + 
    C_given - 
    myco_growth_C -
    turnover_mantle*0.5*C_fungal - turnover_ERM*0.5*C_fungal -
    respiration(Tsb, respiration_params.fungal_a, respiration_params.fungal_b)*C_fungal;
  
  // Nitrogen!
  double root_NC_ratio = N_roots / C_roots;
  double fungal_NC_ratio = N_fungal / C_fungal;
  
  //dN^r/dt
  double uptake_plant = Plant_N_Uptake(NC_in_root_opt, 
                                       T,
                                       SWC,
                                       m,
                                       NH4,
                                       NO3,
                                       FOM_Norg,
                                       N_limits_Plant,
                                       N_k_Plant,
                                       SWC_k_Plant,
                                       C_roots,
                                       N_roots,
                                       C_fungal,
                                       percentage_C_biomass,
                                       parameters_NH4_on_NO3,
                                       C_value_param_plant,
                                       N_value_param_plant)[1];

  double N_given = myco_decision(C_fungal, N_fungal, NC_in_fungai_opt, mantle_mass,
                                 ERM_mass, percentage_C_biomass, C_value_param_myco, N_value_param_myco)[3];
  
  N_roots = N_roots +
    N_given + 
    uptake_plant*C_roots - // This should be by the biomass, so decided that it is multiplied by C^r rather than N^r, maybe should actually be a surface area equation
    (1 - m)*N_roots*turnover_roots*root_NC_ratio - 
    m*N_roots*turnover_roots_mycorrhized*root_NC_ratio -
    0.1*N_roots;
  
  //dN^f/dt
  double uptake_fungal = Fungal_N_Uptake(C_fungal,
                                         N_fungal,
                                         NC_in_fungai_opt,
                                         mantle_mass,
                                         ERM_mass,
                                         percentage_C_biomass,
                                         T,
                                         SWC,
                                         NH4,
                                         NO3,
                                         FOM_Norg,
                                         N_limits_Fungal,
                                         N_k_Fungal,
                                         SWC_k_Fungal,
                                         C_value_param_myco,
                                         N_value_param_myco)[1];
  
  N_fungal = N_fungal + 
    uptake_fungal*C_fungal - 
    myco_growth_N -
    (0.5*turnover_mantle + 0.5*turnover_ERM)*C_fungal*fungal_NC_ratio - 
    N_given;
  
  return Rcpp::List::create(Rcpp::_["C_roots"] = C_roots,
                            Rcpp::_["C_fungal"] = C_fungal,
                            Rcpp::_["N_roots"] = N_roots,
                            Rcpp::_["N_fungal"] = N_fungal,
                            Rcpp::_["uptake_plant"] = uptake_plant*C_roots,
                            Rcpp::_["uptake_fungal"] = uptake_fungal*N_roots,
                            Rcpp::_["to_CASSIA"] = to_CASSIA);

}


