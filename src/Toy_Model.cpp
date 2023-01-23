#include <iostream>
#include <vector>
#include <RcppCommon.h>


// [Rccp::export]
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

// [Rccp::export]
struct ecosystem
{
  organism tree;
  organism myco;
};

namespace Rcpp {
  template <> SEXP wrap(const organism&);
}

#include <Rcpp.h>

Rcpp::List org_to_R(organism org) {
  return Rcpp::List::create(Rcpp::_["transder_timeseries"] = org.transfer_timeseries,
                            Rcpp::_["total_n"] = org.total_n,
                            Rcpp::_["total_c"] = org.total_c,
                            Rcpp::_["input"] = org.input,
                            Rcpp::_["transfer_rate"] = org.transfer_rate,
                            Rcpp::_["value_lower_bound"] = org.value_lower_bound,
                            Rcpp::_["baseline_lower_bound"] = org.baseline_lower_bound);
}

Rcpp::List eco_to_R(ecosystem eco) {
  return Rcpp::List::create(Rcpp::_["transder_timeseries_myco"] = eco.myco.transfer_timeseries,
                            Rcpp::_["total_n_myco"] = eco.myco.total_n,
                            Rcpp::_["total_c_myco"] = eco.myco.total_c,
                            Rcpp::_["input_myco"] = eco.myco.input,
                            Rcpp::_["transfer_rate_myco"] = eco.myco.transfer_rate,
                            Rcpp::_["value_lower_bound_myco"] = eco.myco.value_lower_bound,
                            Rcpp::_["baseline_lower_bound_myco"] = eco.myco.baseline_lower_bound,
                            Rcpp::_["transder_timeseries_tree"] = eco.tree.transfer_timeseries,
                            Rcpp::_["total_n_tree"] = eco.tree.total_n,
                            Rcpp::_["total_c_tree"] = eco.tree.total_c,
                            Rcpp::_["input_tree"] = eco.tree.input,
                            Rcpp::_["transfer_rate_tree"] = eco.tree.transfer_rate,
                            Rcpp::_["value_lower_bound_tree"] = eco.tree.value_lower_bound,
                            Rcpp::_["baseline_lower_bound_tree"] = eco.tree.baseline_lower_bound);
}

int leap_year(int year)
{
  if (year % 4 != 0) {
    return 365;
  }
  else {
    return 366;
  }
}

// [[Rcpp::export]]
std::vector<double> myco_respiration(std::vector<double> years, std::vector<double> Tmb)
{
  /*
   INITIALISATION
   */
  std::vector<double> respiration;
  double a = 0.5; // TODO: Find a better value here
  double b = 0.5; // TOOD: Find a better value here
  
  for (auto year = years.begin(); year != years.end(); ++year) { // FOR YEARS
    for (int day = 0; day <= leap_year(*year); day++){ // FOR DAYS
      respiration.push_back(a * exp(b*Tmb[day]));
    }
  }
  
  return(respiration);
}

// [[Rcpp::export]]
std::vector<double> myco_growth(std::vector<double> years, std::vector<double> sugar, std::vector<double> Tmb)
{
  /*
   Initialisation!
   */
  
  std::vector<double> biomass; // g m-2
  std::vector<double> yield_coefficient; // g m-2, per biomass constricted
  std::vector<double> respiration = myco_respiration(years, Tmb);
  std::vector<double> nitrogen_uptake; 
  std::vector<double> nitrogen_immobilisation;
  std::vector<double> nitrogen_export;
  std::vector<double> ratio_NC;
  ratio_NC.push_back(1); // TODO: replace with a better value!
  
  double lifespan = 1.1; // average lifespan of the fungi TODO: replace with a better value
  double nitrogen_availability = 0.5; // TODO: replace with a better value, also was this meant to be a vector or a conststant? Could make this temeprature dependent as well!
  double max_N_uptake_rate = 0.5; // TODO: replace with a better value!s
  
  /*
   Mycorrhizal growth and nitrogen export as in Nasholm, 2013 - TODO: take the parameters from here as well?
   - limited by C allocation
   - equilibrium with turnover
   */
  for (auto year = years.begin(); year != years.end(); ++year) { // FOR YEARS
    for (int day = 0; day <= leap_year(*year); day++){ // FOR DAYS
      
      yield_coefficient.push_back(1 - respiration[day]); // RESPIRATION
      
      biomass.push_back(yield_coefficient[day] *sugar[day] * lifespan); // BIOMASS
      
      if (day > 0) {
        ratio_NC.push_back(nitrogen_uptake[day-1]/biomass[day-1]);
      }
      
      nitrogen_immobilisation.push_back(yield_coefficient[day] * sugar[day] * ratio_NC[day]); // NITRGEN IMMOBALISATION
      
      nitrogen_uptake.push_back((nitrogen_availability * biomass[day] * max_N_uptake_rate) / (1 + biomass[day] * max_N_uptake_rate)); // NITROGEN UPTAKE
      
      nitrogen_export.push_back(nitrogen_uptake[day] - nitrogen_immobilisation[day]);
      
    }
  }
  
  
  // export this as a part of the organism structure!
  
  return(nitrogen_export);
}

int fixed_ratio_choice(int day, organism& tree, organism& myco)
{
  // PROCESS
  bool enough_N = (myco.total_n[day-1] - myco.input[day-1] * myco.transfer_rate[day-1]) > myco.baseline_lower_bound;
  
  if (enough_N) { // TODO: day-2, but starts from 1
    tree.transfer_timeseries.push_back(myco.input[day-1]*myco.transfer_rate[day-1]);
    tree.total_n.push_back(tree.total_n[day-2] + myco.input[day-1]*myco.transfer_rate[day-1]);
    myco.total_n.push_back(myco.total_n[day-2] + myco.input[day-1]*(1-myco.transfer_rate[day-1]));
    // REM: currently the code is repeated here, can I change the transfer rate instead of calculating twice?
  }
  else {
    tree.transfer_timeseries.push_back(0);
    tree.total_n.push_back(tree.total_n[day-2]);
    myco.total_n.push_back(myco.total_n[day-2] + myco.input[day-1]);
  }
  
  // TODO: should the carbon and nitrogen be dependant somehow?
  bool enough_C = (tree.total_c[day-1] - tree.input[day-1]*tree.transfer_rate[day-1]) > tree.baseline_lower_bound;
  
  if (enough_C) {
    myco.transfer_timeseries.push_back(tree.input[day-1]*tree.transfer_rate[day-1]);
    myco.total_c.push_back(myco.total_c[day-2] + tree.input[day-1]*tree.transfer_rate[day-1]);
    tree.total_c.push_back(tree.total_c[day-2] + tree.input[day-1]*(1 - tree.transfer_rate[day-1]));
  }
  else {
    myco.transfer_timeseries.push_back(0);
    myco.total_c.push_back(myco.total_n[day-2]);
    tree.total_c.push_back(tree.total_n[day-2] + tree.input[day-1]);
  }
  
  return 0;
}


int optimisation_choice(double day, organism& tree_o, organism& myco_o)
{
  // VALUES CHOSEN AS BASELINES // TODO: find some actual values for these
  
  // VALUE FUNCTION TREE
  // This would be the value if the nitrogen is transfered
  double transfer_value_tree = 2*myco_o.input[day-1]*myco_o.transfer_rate[day-1] + 2*tree_o.total_n[day-2] + tree_o.input[day-1]; // TODO: arbitrary here
  
  // This would be the value if the nitrogen would not be transfered
  double value_tree = 2*tree_o.total_n[day-2] + tree_o.input[day-1]; // TODO: arbitrary here
  
  // VALUE FUNCTION myco_oRHIZZA
  // This would be the value if the carbon is transfered
  double transfer_value_myco = myco_o.input[day-1] + 2*myco_o.total_c[day-2] + 2*tree_o.transfer_rate[day-1]*tree_o.input[day-1]; // TODO: arbitrary here
  
  // This would be the value if the nitrogen would not be transfered
  double value_myco = myco_o.input[day-1] + 2*myco_o.total_c[day-2]; // TODO: arbitrary here
  
  
  // RESULTS
  // TODO: currently need both to decided to transfer for a transfer
  
  if (transfer_value_tree - value_tree > tree_o.value_lower_bound) {
    if (transfer_value_myco - value_myco > myco_o.value_lower_bound) {
      tree_o.transfer_timeseries.push_back(myco_o.input[day-1]*myco_o.transfer_rate[day-1]);
      myco_o.transfer_timeseries.push_back(tree_o.transfer_rate[day-1]*tree_o.input[day-1]);
      tree_o.total_n.push_back(tree_o.total_n[day-2] + myco_o.input[day-1]*myco_o.transfer_rate[day-1]);
      myco_o.total_n.push_back(myco_o.total_n[day-2] + myco_o.input[day-1]*(1-myco_o.transfer_rate[day-1]));
      myco_o.total_c.push_back(myco_o.total_c[day-2] + tree_o.input[day-1]*tree_o.transfer_rate[day-1]);
      tree_o.total_c.push_back(tree_o.total_c[day-2] + tree_o.input[day-1]*(1 - tree_o.transfer_rate[day-1]));
      // REM: currently the code is repeated here, can I change the transfer rate instead of calculating twice?
    }
    else {
      tree_o.transfer_timeseries.push_back(0);
      myco_o.transfer_timeseries.push_back(0);
      myco_o.total_c.push_back(myco_o.total_n[day-2]);
      tree_o.total_c.push_back(tree_o.total_n[day-2] + tree_o.input[day-1]);
      tree_o.total_n.push_back(tree_o.total_n[day-2]);
      myco_o.total_n.push_back(myco_o.total_n[day-2] + myco_o.input[day-1]);
    }
  }
  else {
    tree_o.transfer_timeseries.push_back(0);
    myco_o.transfer_timeseries.push_back(0);
    myco_o.total_c.push_back(myco_o.total_n[day-2]);
    tree_o.total_c.push_back(tree_o.total_n[day-2] + tree_o.input[day-1]);
    tree_o.total_n.push_back(tree_o.total_n[day-2]);
    myco_o.total_n.push_back(myco_o.total_n[day-2] + myco_o.input[day-1]);
  }
  
  
  return 0; // TODO: return data structure
}

int game_theory_choice(double day, organism& tree, organism& myco)
{
  std::cout << "No function yet! \n";
  return 3; // TODO: return data structure
}

// [[Rcpp::export]]
Rcpp::List Toy_Model(std::vector<double> years, std::vector<double> sugar, std::vector<double> Tmb)
{
  // INITIAL CONDITIONS
  // REM: have created everything twice, this isn't very beautiful...
  
  organism tree;
  organism myco;
  organism tree_o;
  organism myco_o;
  
  std::vector<double> days;
  for (auto year = years.begin(); year != years.end(); ++year)
  {
    days.push_back(leap_year(*year));
  }
  int length = accumulate(days.begin(), days.end(), 0);
  
  // storage
  tree.total_n.assign(length, 1);
  tree.total_c.assign(length, 1);
  myco.total_n.assign(length, 1);
  myco.total_c.assign(length, 1);
  tree_o.total_n.assign(length, 1);
  tree_o.total_c.assign(length, 1);
  myco_o.total_n.assign(length, 1);
  myco_o.total_c.assign(length, 1);
  
  // INPUTED ELEMENTAL DYNAMICS FROM OTHER MODELS
  
  
  /* WHAT I WANT!
  
  photosynthesis = PRELES;
  
  sugar = CASSIA; // TODO: inport this value from R
  
  */
  
  tree.input = tree_o.input = sugar;
  
  myco.input = myco_o.input = myco_growth(years, sugar, Tmb);
  
  // VALUES FROM LITERATURE
  tree.transfer_rate.assign(length, 0.3); // REM: usually said to be 30% of photosynthesates
  myco.transfer_rate.assign(length, 0.5); // TODO: any values for this?
  tree_o.transfer_rate.assign(length, 0.3); // REM: usually said to be 30% of photosynthesates
  myco_o.transfer_rate.assign(length, 0.5); // TODO: any values for this?
  
  tree.value_lower_bound = tree_o.value_lower_bound = 0.5; // REM: find some things for this!
  myco.value_lower_bound = myco_o.value_lower_bound = 0.5;
  
  tree.baseline_lower_bound = tree_o.baseline_lower_bound = 0.24; // TODO: minimum C in tree
  myco.baseline_lower_bound = myco_o.baseline_lower_bound = 0.24; // TODO: minimum N in myco
  
  for (auto year = years.begin(); year != years.end(); ++year) { // FOR YEARS
     for (int day = 1; day <= leap_year(*year); day++)
     {
     fixed_ratio_choice(day, tree, myco);
     
     optimisation_choice(day, tree_o, myco_o);
     
     // REM: add game theory when the first two work and interact properly
     //game_theory_choice(day, tree, myco);
     //std::cout << "Fixed ratio value for day " << day << ", N: " << tree.transfer_timeseries[day-1] << ", C: " << myco.transfer_timeseries[day-1] << "\n";
     }
  }

  ecosystem boreal;
  boreal.myco = myco;
  boreal.tree = tree;
   
  Rcpp::List ahh = eco_to_R(boreal);

  return ahh;
}