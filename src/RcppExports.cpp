// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// Toy_Model
Rcpp::List Toy_Model(double year, Rcpp::DataFrame Hyde_weather, std::vector<double> parameters_R);
RcppExport SEXP _MycoModel_Toy_Model(SEXP yearSEXP, SEXP Hyde_weatherSEXP, SEXP parameters_RSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type year(yearSEXP);
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type Hyde_weather(Hyde_weatherSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type parameters_R(parameters_RSEXP);
    rcpp_result_gen = Rcpp::wrap(Toy_Model(year, Hyde_weather, parameters_R));
    return rcpp_result_gen;
END_RCPP
}
// plant_decision
double plant_decision(double C_roots, double N_roots, double NC_in_root_opt);
RcppExport SEXP _MycoModel_plant_decision(SEXP C_rootsSEXP, SEXP N_rootsSEXP, SEXP NC_in_root_optSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type C_roots(C_rootsSEXP);
    Rcpp::traits::input_parameter< double >::type N_roots(N_rootsSEXP);
    Rcpp::traits::input_parameter< double >::type NC_in_root_opt(NC_in_root_optSEXP);
    rcpp_result_gen = Rcpp::wrap(plant_decision(C_roots, N_roots, NC_in_root_opt));
    return rcpp_result_gen;
END_RCPP
}
// myco_decision
double myco_decision(double C_fungal, double N_fungal, double NC_in_fungal_opt);
RcppExport SEXP _MycoModel_myco_decision(SEXP C_fungalSEXP, SEXP N_fungalSEXP, SEXP NC_in_fungal_optSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type C_fungal(C_fungalSEXP);
    Rcpp::traits::input_parameter< double >::type N_fungal(N_fungalSEXP);
    Rcpp::traits::input_parameter< double >::type NC_in_fungal_opt(NC_in_fungal_optSEXP);
    rcpp_result_gen = Rcpp::wrap(myco_decision(C_fungal, N_fungal, NC_in_fungal_opt));
    return rcpp_result_gen;
END_RCPP
}
// myco_growth
double myco_growth(double C_fungal, double N_fungal, double sugar, double N, double Tmb);
RcppExport SEXP _MycoModel_myco_growth(SEXP C_fungalSEXP, SEXP N_fungalSEXP, SEXP sugarSEXP, SEXP NSEXP, SEXP TmbSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type C_fungal(C_fungalSEXP);
    Rcpp::traits::input_parameter< double >::type N_fungal(N_fungalSEXP);
    Rcpp::traits::input_parameter< double >::type sugar(sugarSEXP);
    Rcpp::traits::input_parameter< double >::type N(NSEXP);
    Rcpp::traits::input_parameter< double >::type Tmb(TmbSEXP);
    rcpp_result_gen = Rcpp::wrap(myco_growth(C_fungal, N_fungal, sugar, N, Tmb));
    return rcpp_result_gen;
END_RCPP
}
// mycofon_balence
Rcpp::List mycofon_balence(double Allocation_C_CASSIA, double N_to_CASSIA, double root_mass, double fungal_mass, double optimal_root_fungal_biomass_ratio, double C_roots, double C_roots_biomass, double N_roots, double C_fungal, double N_fungal, double turnover_roots, double turnover_fungal, double a_roots, double b_roots, double a_myco, double b_myco, double C_allocation, double N_allocation, std::vector<double> N_in_root_R, double NH4, double NO3, double FOM_Norg, double C_in_root, double NC_in_root_opt, double T, double Tsb, double SWC, std::vector<double> N_limits_R, std::vector<double> N_k_R, std::vector<double> SWC_k_R, double NC_fungal_opt, double mantle_mass, double ERM_mass, double N_avaliable, std::vector<double> parameters_NH4_on_NO3);
RcppExport SEXP _MycoModel_mycofon_balence(SEXP Allocation_C_CASSIASEXP, SEXP N_to_CASSIASEXP, SEXP root_massSEXP, SEXP fungal_massSEXP, SEXP optimal_root_fungal_biomass_ratioSEXP, SEXP C_rootsSEXP, SEXP C_roots_biomassSEXP, SEXP N_rootsSEXP, SEXP C_fungalSEXP, SEXP N_fungalSEXP, SEXP turnover_rootsSEXP, SEXP turnover_fungalSEXP, SEXP a_rootsSEXP, SEXP b_rootsSEXP, SEXP a_mycoSEXP, SEXP b_mycoSEXP, SEXP C_allocationSEXP, SEXP N_allocationSEXP, SEXP N_in_root_RSEXP, SEXP NH4SEXP, SEXP NO3SEXP, SEXP FOM_NorgSEXP, SEXP C_in_rootSEXP, SEXP NC_in_root_optSEXP, SEXP TSEXP, SEXP TsbSEXP, SEXP SWCSEXP, SEXP N_limits_RSEXP, SEXP N_k_RSEXP, SEXP SWC_k_RSEXP, SEXP NC_fungal_optSEXP, SEXP mantle_massSEXP, SEXP ERM_massSEXP, SEXP N_avaliableSEXP, SEXP parameters_NH4_on_NO3SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type Allocation_C_CASSIA(Allocation_C_CASSIASEXP);
    Rcpp::traits::input_parameter< double >::type N_to_CASSIA(N_to_CASSIASEXP);
    Rcpp::traits::input_parameter< double >::type root_mass(root_massSEXP);
    Rcpp::traits::input_parameter< double >::type fungal_mass(fungal_massSEXP);
    Rcpp::traits::input_parameter< double >::type optimal_root_fungal_biomass_ratio(optimal_root_fungal_biomass_ratioSEXP);
    Rcpp::traits::input_parameter< double >::type C_roots(C_rootsSEXP);
    Rcpp::traits::input_parameter< double >::type C_roots_biomass(C_roots_biomassSEXP);
    Rcpp::traits::input_parameter< double >::type N_roots(N_rootsSEXP);
    Rcpp::traits::input_parameter< double >::type C_fungal(C_fungalSEXP);
    Rcpp::traits::input_parameter< double >::type N_fungal(N_fungalSEXP);
    Rcpp::traits::input_parameter< double >::type turnover_roots(turnover_rootsSEXP);
    Rcpp::traits::input_parameter< double >::type turnover_fungal(turnover_fungalSEXP);
    Rcpp::traits::input_parameter< double >::type a_roots(a_rootsSEXP);
    Rcpp::traits::input_parameter< double >::type b_roots(b_rootsSEXP);
    Rcpp::traits::input_parameter< double >::type a_myco(a_mycoSEXP);
    Rcpp::traits::input_parameter< double >::type b_myco(b_mycoSEXP);
    Rcpp::traits::input_parameter< double >::type C_allocation(C_allocationSEXP);
    Rcpp::traits::input_parameter< double >::type N_allocation(N_allocationSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type N_in_root_R(N_in_root_RSEXP);
    Rcpp::traits::input_parameter< double >::type NH4(NH4SEXP);
    Rcpp::traits::input_parameter< double >::type NO3(NO3SEXP);
    Rcpp::traits::input_parameter< double >::type FOM_Norg(FOM_NorgSEXP);
    Rcpp::traits::input_parameter< double >::type C_in_root(C_in_rootSEXP);
    Rcpp::traits::input_parameter< double >::type NC_in_root_opt(NC_in_root_optSEXP);
    Rcpp::traits::input_parameter< double >::type T(TSEXP);
    Rcpp::traits::input_parameter< double >::type Tsb(TsbSEXP);
    Rcpp::traits::input_parameter< double >::type SWC(SWCSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type N_limits_R(N_limits_RSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type N_k_R(N_k_RSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type SWC_k_R(SWC_k_RSEXP);
    Rcpp::traits::input_parameter< double >::type NC_fungal_opt(NC_fungal_optSEXP);
    Rcpp::traits::input_parameter< double >::type mantle_mass(mantle_massSEXP);
    Rcpp::traits::input_parameter< double >::type ERM_mass(ERM_massSEXP);
    Rcpp::traits::input_parameter< double >::type N_avaliable(N_avaliableSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type parameters_NH4_on_NO3(parameters_NH4_on_NO3SEXP);
    rcpp_result_gen = Rcpp::wrap(mycofon_balence(Allocation_C_CASSIA, N_to_CASSIA, root_mass, fungal_mass, optimal_root_fungal_biomass_ratio, C_roots, C_roots_biomass, N_roots, C_fungal, N_fungal, turnover_roots, turnover_fungal, a_roots, b_roots, a_myco, b_myco, C_allocation, N_allocation, N_in_root_R, NH4, NO3, FOM_Norg, C_in_root, NC_in_root_opt, T, Tsb, SWC, N_limits_R, N_k_R, SWC_k_R, NC_fungal_opt, mantle_mass, ERM_mass, N_avaliable, parameters_NH4_on_NO3));
    return rcpp_result_gen;
END_RCPP
}
// uptake_organic_N
double uptake_organic_N(double N_org, double T, double N_org_limit, double k, double SWC, double SWC_k);
RcppExport SEXP _MycoModel_uptake_organic_N(SEXP N_orgSEXP, SEXP TSEXP, SEXP N_org_limitSEXP, SEXP kSEXP, SEXP SWCSEXP, SEXP SWC_kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type N_org(N_orgSEXP);
    Rcpp::traits::input_parameter< double >::type T(TSEXP);
    Rcpp::traits::input_parameter< double >::type N_org_limit(N_org_limitSEXP);
    Rcpp::traits::input_parameter< double >::type k(kSEXP);
    Rcpp::traits::input_parameter< double >::type SWC(SWCSEXP);
    Rcpp::traits::input_parameter< double >::type SWC_k(SWC_kSEXP);
    rcpp_result_gen = Rcpp::wrap(uptake_organic_N(N_org, T, N_org_limit, k, SWC, SWC_k));
    return rcpp_result_gen;
END_RCPP
}
// uptake_NH4
double uptake_NH4(double NH4, double T, double NH4_limit, double k, double SWC, double SWC_k);
RcppExport SEXP _MycoModel_uptake_NH4(SEXP NH4SEXP, SEXP TSEXP, SEXP NH4_limitSEXP, SEXP kSEXP, SEXP SWCSEXP, SEXP SWC_kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type NH4(NH4SEXP);
    Rcpp::traits::input_parameter< double >::type T(TSEXP);
    Rcpp::traits::input_parameter< double >::type NH4_limit(NH4_limitSEXP);
    Rcpp::traits::input_parameter< double >::type k(kSEXP);
    Rcpp::traits::input_parameter< double >::type SWC(SWCSEXP);
    Rcpp::traits::input_parameter< double >::type SWC_k(SWC_kSEXP);
    rcpp_result_gen = Rcpp::wrap(uptake_NH4(NH4, T, NH4_limit, k, SWC, SWC_k));
    return rcpp_result_gen;
END_RCPP
}
// uptake_NO3
double uptake_NO3(double NO3, double T, double NO3_limit, double k, double SWC, double SWC_k);
RcppExport SEXP _MycoModel_uptake_NO3(SEXP NO3SEXP, SEXP TSEXP, SEXP NO3_limitSEXP, SEXP kSEXP, SEXP SWCSEXP, SEXP SWC_kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type NO3(NO3SEXP);
    Rcpp::traits::input_parameter< double >::type T(TSEXP);
    Rcpp::traits::input_parameter< double >::type NO3_limit(NO3_limitSEXP);
    Rcpp::traits::input_parameter< double >::type k(kSEXP);
    Rcpp::traits::input_parameter< double >::type SWC(SWCSEXP);
    Rcpp::traits::input_parameter< double >::type SWC_k(SWC_kSEXP);
    rcpp_result_gen = Rcpp::wrap(uptake_NO3(NO3, T, NO3_limit, k, SWC, SWC_k));
    return rcpp_result_gen;
END_RCPP
}
// uptake_C
double uptake_C(double C, double T, double C_limit, double k, double SWC, double SWC_k);
RcppExport SEXP _MycoModel_uptake_C(SEXP CSEXP, SEXP TSEXP, SEXP C_limitSEXP, SEXP kSEXP, SEXP SWCSEXP, SEXP SWC_kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type C(CSEXP);
    Rcpp::traits::input_parameter< double >::type T(TSEXP);
    Rcpp::traits::input_parameter< double >::type C_limit(C_limitSEXP);
    Rcpp::traits::input_parameter< double >::type k(kSEXP);
    Rcpp::traits::input_parameter< double >::type SWC(SWCSEXP);
    Rcpp::traits::input_parameter< double >::type SWC_k(SWC_kSEXP);
    rcpp_result_gen = Rcpp::wrap(uptake_C(C, T, C_limit, k, SWC, SWC_k));
    return rcpp_result_gen;
END_RCPP
}
// Plant_N_Uptake
Rcpp::List Plant_N_Uptake(double NC_in_root_opt, double T, double SWC, double m, double NH4_in, double NO3_in, double FOM_in, std::vector<double> N_limits_R, std::vector<double> N_k_R, std::vector<double> SWC_k_R, double C_roots, double N_roots, double percentage_C_biomass, double N_allocation, std::vector<double> parameters);
RcppExport SEXP _MycoModel_Plant_N_Uptake(SEXP NC_in_root_optSEXP, SEXP TSEXP, SEXP SWCSEXP, SEXP mSEXP, SEXP NH4_inSEXP, SEXP NO3_inSEXP, SEXP FOM_inSEXP, SEXP N_limits_RSEXP, SEXP N_k_RSEXP, SEXP SWC_k_RSEXP, SEXP C_rootsSEXP, SEXP N_rootsSEXP, SEXP percentage_C_biomassSEXP, SEXP N_allocationSEXP, SEXP parametersSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type NC_in_root_opt(NC_in_root_optSEXP);
    Rcpp::traits::input_parameter< double >::type T(TSEXP);
    Rcpp::traits::input_parameter< double >::type SWC(SWCSEXP);
    Rcpp::traits::input_parameter< double >::type m(mSEXP);
    Rcpp::traits::input_parameter< double >::type NH4_in(NH4_inSEXP);
    Rcpp::traits::input_parameter< double >::type NO3_in(NO3_inSEXP);
    Rcpp::traits::input_parameter< double >::type FOM_in(FOM_inSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type N_limits_R(N_limits_RSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type N_k_R(N_k_RSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type SWC_k_R(SWC_k_RSEXP);
    Rcpp::traits::input_parameter< double >::type C_roots(C_rootsSEXP);
    Rcpp::traits::input_parameter< double >::type N_roots(N_rootsSEXP);
    Rcpp::traits::input_parameter< double >::type percentage_C_biomass(percentage_C_biomassSEXP);
    Rcpp::traits::input_parameter< double >::type N_allocation(N_allocationSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type parameters(parametersSEXP);
    rcpp_result_gen = Rcpp::wrap(Plant_N_Uptake(NC_in_root_opt, T, SWC, m, NH4_in, NO3_in, FOM_in, N_limits_R, N_k_R, SWC_k_R, C_roots, N_roots, percentage_C_biomass, N_allocation, parameters));
    return rcpp_result_gen;
END_RCPP
}
// Fungal_N_Uptake
Rcpp::List Fungal_N_Uptake(double C_fungal, double N_fungal, double NC_fungal_opt, double mantle_mass, double ERM_mass, double percentage_C_biomass, double T, double SWC, double NH4, double NO3, double FOM_Norg, std::vector<double> N_limits_R, std::vector<double> N_k_R, std::vector<double> SWC_k_R);
RcppExport SEXP _MycoModel_Fungal_N_Uptake(SEXP C_fungalSEXP, SEXP N_fungalSEXP, SEXP NC_fungal_optSEXP, SEXP mantle_massSEXP, SEXP ERM_massSEXP, SEXP percentage_C_biomassSEXP, SEXP TSEXP, SEXP SWCSEXP, SEXP NH4SEXP, SEXP NO3SEXP, SEXP FOM_NorgSEXP, SEXP N_limits_RSEXP, SEXP N_k_RSEXP, SEXP SWC_k_RSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type C_fungal(C_fungalSEXP);
    Rcpp::traits::input_parameter< double >::type N_fungal(N_fungalSEXP);
    Rcpp::traits::input_parameter< double >::type NC_fungal_opt(NC_fungal_optSEXP);
    Rcpp::traits::input_parameter< double >::type mantle_mass(mantle_massSEXP);
    Rcpp::traits::input_parameter< double >::type ERM_mass(ERM_massSEXP);
    Rcpp::traits::input_parameter< double >::type percentage_C_biomass(percentage_C_biomassSEXP);
    Rcpp::traits::input_parameter< double >::type T(TSEXP);
    Rcpp::traits::input_parameter< double >::type SWC(SWCSEXP);
    Rcpp::traits::input_parameter< double >::type NH4(NH4SEXP);
    Rcpp::traits::input_parameter< double >::type NO3(NO3SEXP);
    Rcpp::traits::input_parameter< double >::type FOM_Norg(FOM_NorgSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type N_limits_R(N_limits_RSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type N_k_R(N_k_RSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type SWC_k_R(SWC_k_RSEXP);
    rcpp_result_gen = Rcpp::wrap(Fungal_N_Uptake(C_fungal, N_fungal, NC_fungal_opt, mantle_mass, ERM_mass, percentage_C_biomass, T, SWC, NH4, NO3, FOM_Norg, N_limits_R, N_k_R, SWC_k_R));
    return rcpp_result_gen;
END_RCPP
}
// Microbe_Uptake
Rcpp::List Microbe_Uptake(double C_microbe, double N_micorbe, double NC_microbe_opt, double NH4_avaliable, double NO3_avaliable, double Norg_avaliable, double T, double SWC, std::vector<double> N_limits_R, std::vector<double> N_k_R, std::vector<double> SWC_k_R, bool SOM_decomposers, double Norg_avaliable_FOM);
RcppExport SEXP _MycoModel_Microbe_Uptake(SEXP C_microbeSEXP, SEXP N_micorbeSEXP, SEXP NC_microbe_optSEXP, SEXP NH4_avaliableSEXP, SEXP NO3_avaliableSEXP, SEXP Norg_avaliableSEXP, SEXP TSEXP, SEXP SWCSEXP, SEXP N_limits_RSEXP, SEXP N_k_RSEXP, SEXP SWC_k_RSEXP, SEXP SOM_decomposersSEXP, SEXP Norg_avaliable_FOMSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type C_microbe(C_microbeSEXP);
    Rcpp::traits::input_parameter< double >::type N_micorbe(N_micorbeSEXP);
    Rcpp::traits::input_parameter< double >::type NC_microbe_opt(NC_microbe_optSEXP);
    Rcpp::traits::input_parameter< double >::type NH4_avaliable(NH4_avaliableSEXP);
    Rcpp::traits::input_parameter< double >::type NO3_avaliable(NO3_avaliableSEXP);
    Rcpp::traits::input_parameter< double >::type Norg_avaliable(Norg_avaliableSEXP);
    Rcpp::traits::input_parameter< double >::type T(TSEXP);
    Rcpp::traits::input_parameter< double >::type SWC(SWCSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type N_limits_R(N_limits_RSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type N_k_R(N_k_RSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type SWC_k_R(SWC_k_RSEXP);
    Rcpp::traits::input_parameter< bool >::type SOM_decomposers(SOM_decomposersSEXP);
    Rcpp::traits::input_parameter< double >::type Norg_avaliable_FOM(Norg_avaliable_FOMSEXP);
    rcpp_result_gen = Rcpp::wrap(Microbe_Uptake(C_microbe, N_micorbe, NC_microbe_opt, NH4_avaliable, NO3_avaliable, Norg_avaliable, T, SWC, N_limits_R, N_k_R, SWC_k_R, SOM_decomposers, Norg_avaliable_FOM));
    return rcpp_result_gen;
END_RCPP
}
// respiration
double respiration(double Tmb, double a, double b);
RcppExport SEXP _MycoModel_respiration(SEXP TmbSEXP, SEXP aSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type Tmb(TmbSEXP);
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    Rcpp::traits::input_parameter< double >::type b(bSEXP);
    rcpp_result_gen = Rcpp::wrap(respiration(Tmb, a, b));
    return rcpp_result_gen;
END_RCPP
}
// symphony_multiple_FOM_daily
Rcpp::List symphony_multiple_FOM_daily(double Tmb, double SWC, double C_FOM_needles, double C_FOM_woody, double C_FOM_roots, double C_FOM_mycelium, double C_SOM, double C_decompose_FOM, double C_decompose_SOM, double N_decompose_FOM, double N_decompose_SOM, double Litter_needles, double Litter_woody, double Litter_roots, double Litter_mycelium, double NH4, double NO3, double NH4_used_Plant, double NH4_used_Fungal, double NO3_used_Plant, double NO3_used_Fungal, double FOM_Norg_used_Plant, double FOM_Norg_used_Fungal, double SOM_Norg_used, std::vector<double> respiration_microbes_params, std::vector<double> N_limits_R, std::vector<double> N_k_R, std::vector<double> SWC_k_R, double NC_microbe_opt, double microbe_turnover);
RcppExport SEXP _MycoModel_symphony_multiple_FOM_daily(SEXP TmbSEXP, SEXP SWCSEXP, SEXP C_FOM_needlesSEXP, SEXP C_FOM_woodySEXP, SEXP C_FOM_rootsSEXP, SEXP C_FOM_myceliumSEXP, SEXP C_SOMSEXP, SEXP C_decompose_FOMSEXP, SEXP C_decompose_SOMSEXP, SEXP N_decompose_FOMSEXP, SEXP N_decompose_SOMSEXP, SEXP Litter_needlesSEXP, SEXP Litter_woodySEXP, SEXP Litter_rootsSEXP, SEXP Litter_myceliumSEXP, SEXP NH4SEXP, SEXP NO3SEXP, SEXP NH4_used_PlantSEXP, SEXP NH4_used_FungalSEXP, SEXP NO3_used_PlantSEXP, SEXP NO3_used_FungalSEXP, SEXP FOM_Norg_used_PlantSEXP, SEXP FOM_Norg_used_FungalSEXP, SEXP SOM_Norg_usedSEXP, SEXP respiration_microbes_paramsSEXP, SEXP N_limits_RSEXP, SEXP N_k_RSEXP, SEXP SWC_k_RSEXP, SEXP NC_microbe_optSEXP, SEXP microbe_turnoverSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type Tmb(TmbSEXP);
    Rcpp::traits::input_parameter< double >::type SWC(SWCSEXP);
    Rcpp::traits::input_parameter< double >::type C_FOM_needles(C_FOM_needlesSEXP);
    Rcpp::traits::input_parameter< double >::type C_FOM_woody(C_FOM_woodySEXP);
    Rcpp::traits::input_parameter< double >::type C_FOM_roots(C_FOM_rootsSEXP);
    Rcpp::traits::input_parameter< double >::type C_FOM_mycelium(C_FOM_myceliumSEXP);
    Rcpp::traits::input_parameter< double >::type C_SOM(C_SOMSEXP);
    Rcpp::traits::input_parameter< double >::type C_decompose_FOM(C_decompose_FOMSEXP);
    Rcpp::traits::input_parameter< double >::type C_decompose_SOM(C_decompose_SOMSEXP);
    Rcpp::traits::input_parameter< double >::type N_decompose_FOM(N_decompose_FOMSEXP);
    Rcpp::traits::input_parameter< double >::type N_decompose_SOM(N_decompose_SOMSEXP);
    Rcpp::traits::input_parameter< double >::type Litter_needles(Litter_needlesSEXP);
    Rcpp::traits::input_parameter< double >::type Litter_woody(Litter_woodySEXP);
    Rcpp::traits::input_parameter< double >::type Litter_roots(Litter_rootsSEXP);
    Rcpp::traits::input_parameter< double >::type Litter_mycelium(Litter_myceliumSEXP);
    Rcpp::traits::input_parameter< double >::type NH4(NH4SEXP);
    Rcpp::traits::input_parameter< double >::type NO3(NO3SEXP);
    Rcpp::traits::input_parameter< double >::type NH4_used_Plant(NH4_used_PlantSEXP);
    Rcpp::traits::input_parameter< double >::type NH4_used_Fungal(NH4_used_FungalSEXP);
    Rcpp::traits::input_parameter< double >::type NO3_used_Plant(NO3_used_PlantSEXP);
    Rcpp::traits::input_parameter< double >::type NO3_used_Fungal(NO3_used_FungalSEXP);
    Rcpp::traits::input_parameter< double >::type FOM_Norg_used_Plant(FOM_Norg_used_PlantSEXP);
    Rcpp::traits::input_parameter< double >::type FOM_Norg_used_Fungal(FOM_Norg_used_FungalSEXP);
    Rcpp::traits::input_parameter< double >::type SOM_Norg_used(SOM_Norg_usedSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type respiration_microbes_params(respiration_microbes_paramsSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type N_limits_R(N_limits_RSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type N_k_R(N_k_RSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type SWC_k_R(SWC_k_RSEXP);
    Rcpp::traits::input_parameter< double >::type NC_microbe_opt(NC_microbe_optSEXP);
    Rcpp::traits::input_parameter< double >::type microbe_turnover(microbe_turnoverSEXP);
    rcpp_result_gen = Rcpp::wrap(symphony_multiple_FOM_daily(Tmb, SWC, C_FOM_needles, C_FOM_woody, C_FOM_roots, C_FOM_mycelium, C_SOM, C_decompose_FOM, C_decompose_SOM, N_decompose_FOM, N_decompose_SOM, Litter_needles, Litter_woody, Litter_roots, Litter_mycelium, NH4, NO3, NH4_used_Plant, NH4_used_Fungal, NO3_used_Plant, NO3_used_Fungal, FOM_Norg_used_Plant, FOM_Norg_used_Fungal, SOM_Norg_used, respiration_microbes_params, N_limits_R, N_k_R, SWC_k_R, NC_microbe_opt, microbe_turnover));
    return rcpp_result_gen;
END_RCPP
}
// symphony
Rcpp::List symphony(std::vector<double> params);
RcppExport SEXP _MycoModel_symphony(SEXP paramsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<double> >::type params(paramsSEXP);
    rcpp_result_gen = Rcpp::wrap(symphony(params));
    return rcpp_result_gen;
END_RCPP
}
// symphony_plus
Rcpp::List symphony_plus(std::vector<double> params, std::vector<double> Photosynthesis);
RcppExport SEXP _MycoModel_symphony_plus(SEXP paramsSEXP, SEXP PhotosynthesisSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<double> >::type params(paramsSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type Photosynthesis(PhotosynthesisSEXP);
    rcpp_result_gen = Rcpp::wrap(symphony_plus(params, Photosynthesis));
    return rcpp_result_gen;
END_RCPP
}
// symphony_plus_daily
Rcpp::List symphony_plus_daily(std::vector<double> params, double Photosynthesis, double C_plant, double C_FOM, double N);
RcppExport SEXP _MycoModel_symphony_plus_daily(SEXP paramsSEXP, SEXP PhotosynthesisSEXP, SEXP C_plantSEXP, SEXP C_FOMSEXP, SEXP NSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<double> >::type params(paramsSEXP);
    Rcpp::traits::input_parameter< double >::type Photosynthesis(PhotosynthesisSEXP);
    Rcpp::traits::input_parameter< double >::type C_plant(C_plantSEXP);
    Rcpp::traits::input_parameter< double >::type C_FOM(C_FOMSEXP);
    Rcpp::traits::input_parameter< double >::type N(NSEXP);
    rcpp_result_gen = Rcpp::wrap(symphony_plus_daily(params, Photosynthesis, C_plant, C_FOM, N));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_MycoModel_Toy_Model", (DL_FUNC) &_MycoModel_Toy_Model, 3},
    {"_MycoModel_plant_decision", (DL_FUNC) &_MycoModel_plant_decision, 3},
    {"_MycoModel_myco_decision", (DL_FUNC) &_MycoModel_myco_decision, 3},
    {"_MycoModel_myco_growth", (DL_FUNC) &_MycoModel_myco_growth, 5},
    {"_MycoModel_mycofon_balence", (DL_FUNC) &_MycoModel_mycofon_balence, 35},
    {"_MycoModel_uptake_organic_N", (DL_FUNC) &_MycoModel_uptake_organic_N, 6},
    {"_MycoModel_uptake_NH4", (DL_FUNC) &_MycoModel_uptake_NH4, 6},
    {"_MycoModel_uptake_NO3", (DL_FUNC) &_MycoModel_uptake_NO3, 6},
    {"_MycoModel_uptake_C", (DL_FUNC) &_MycoModel_uptake_C, 6},
    {"_MycoModel_Plant_N_Uptake", (DL_FUNC) &_MycoModel_Plant_N_Uptake, 15},
    {"_MycoModel_Fungal_N_Uptake", (DL_FUNC) &_MycoModel_Fungal_N_Uptake, 14},
    {"_MycoModel_Microbe_Uptake", (DL_FUNC) &_MycoModel_Microbe_Uptake, 13},
    {"_MycoModel_respiration", (DL_FUNC) &_MycoModel_respiration, 3},
    {"_MycoModel_symphony_multiple_FOM_daily", (DL_FUNC) &_MycoModel_symphony_multiple_FOM_daily, 30},
    {"_MycoModel_symphony", (DL_FUNC) &_MycoModel_symphony, 1},
    {"_MycoModel_symphony_plus", (DL_FUNC) &_MycoModel_symphony_plus, 2},
    {"_MycoModel_symphony_plus_daily", (DL_FUNC) &_MycoModel_symphony_plus_daily, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_MycoModel(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
