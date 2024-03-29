# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

Toy_Model <- function(year, C_roots, N_roots, C_fungal, N_fungal, Litter_mantle, Litter_ERM, Hyde_weather, parameters_R) {
    .Call(`_MycoModel_Toy_Model`, year, C_roots, N_roots, C_fungal, N_fungal, Litter_mantle, Litter_ERM, Hyde_weather, parameters_R)
}

plant_decision <- function(C_roots, N_roots, C_fungal, optimal_root_funga_biomass_ratio, N_allo, max_C_allocation_CASSIA) {
    .Call(`_MycoModel_plant_decision`, C_roots, N_roots, C_fungal, optimal_root_funga_biomass_ratio, N_allo, max_C_allocation_CASSIA)
}

myco_decision <- function(C_fungal, N_fungal, C_roots, N_roots, NC_fungal_opt, growth_C, growth_N) {
    .Call(`_MycoModel_myco_decision`, C_fungal, N_fungal, C_roots, N_roots, NC_fungal_opt, growth_C, growth_N)
}

myco_growth <- function(C_fungal, N_fungal, a, b) {
    .Call(`_MycoModel_myco_growth`, C_fungal, N_fungal, a, b)
}

mycofon_balence <- function(C_roots, N_roots, optimal_root_fungal_biomass_ratio, C_fungal, N_fungal, turnover_roots, turnover_roots_mycorrhized, turnover_mantle, turnover_ERM, respiration_parameters_R, NH4, NO3, FOM_Norg, NC_in_fungal_opt, T, Tsb, SWC, N_limits_Plant, N_k_Plant, SWC_k_Plant, N_limits_Fungal, N_k_Fungal, SWC_k_Fungal, mantle_mass, ERM_mass, parameters_NH4_on_NO3, growth_C, growth_N, max_C_allocation_CASSIA, allocation_N_to_rest_of_plant, mycofon_stratergy) {
    .Call(`_MycoModel_mycofon_balence`, C_roots, N_roots, optimal_root_fungal_biomass_ratio, C_fungal, N_fungal, turnover_roots, turnover_roots_mycorrhized, turnover_mantle, turnover_ERM, respiration_parameters_R, NH4, NO3, FOM_Norg, NC_in_fungal_opt, T, Tsb, SWC, N_limits_Plant, N_k_Plant, SWC_k_Plant, N_limits_Fungal, N_k_Fungal, SWC_k_Fungal, mantle_mass, ERM_mass, parameters_NH4_on_NO3, growth_C, growth_N, max_C_allocation_CASSIA, allocation_N_to_rest_of_plant, mycofon_stratergy)
}

uptake_N <- function(N, T, N_limit, k, SWC, SWC_sat) {
    .Call(`_MycoModel_uptake_N`, N, T, N_limit, k, SWC, SWC_sat)
}

uptake_C <- function(C, T, C_limit, k, SWC, SWC_k) {
    .Call(`_MycoModel_uptake_C`, C, T, C_limit, k, SWC, SWC_k)
}

Plant_N_Uptake <- function(T, SWC, m, NH4_in, NO3_in, FOM_in, N_limits_R, N_k_R, SWC_k_R, parameters, demand) {
    .Call(`_MycoModel_Plant_N_Uptake`, T, SWC, m, NH4_in, NO3_in, FOM_in, N_limits_R, N_k_R, SWC_k_R, parameters, demand)
}

Fungal_N_Uptake <- function(T, SWC, NH4, NO3, FOM_Norg, N_limits_R, N_k_R, SWC_k_R, demand) {
    .Call(`_MycoModel_Fungal_N_Uptake`, T, SWC, NH4, NO3, FOM_Norg, N_limits_R, N_k_R, SWC_k_R, demand)
}

Microbe_Uptake <- function(C_microbe, N_micorbe, C_soil_compartment, NC_microbe_opt, NH4_avaliable, NO3_avaliable, Norg_avaliable, T, SWC, NC_Litter, imobilisation, assimilation, N_limits_R, N_k_R, SWC_k_R, SOM_decomposers, respiration_microbes_params) {
    .Call(`_MycoModel_Microbe_Uptake`, C_microbe, N_micorbe, C_soil_compartment, NC_microbe_opt, NH4_avaliable, NO3_avaliable, Norg_avaliable, T, SWC, NC_Litter, imobilisation, assimilation, N_limits_R, N_k_R, SWC_k_R, SOM_decomposers, respiration_microbes_params)
}

respiration <- function(Tmb, Rm, Q10) {
    .Call(`_MycoModel_respiration`, Tmb, Rm, Q10)
}

symphony_multiple_FOM_daily <- function(Tmb, SWC, C_FOM_needles_old, C_FOM_woody_old, C_FOM_roots_old, C_FOM_mantle_old, C_FOM_ERM_old, C_SOM_old, N_SOM_old, C_decompose_FOM, C_decompose_SOM, N_decompose_FOM, N_decompose_SOM, Litter_needles, Litter_woody, Litter_roots, Litter_mantle, Litter_ERM, imobilisation, assimilation, NH4_old, NO3_old, NC_needles, NC_woody, NC_roots, NC_mantle, NC_ERM, NH4_used_Plant, NH4_used_Fungal, NO3_used_Plant, NO3_used_Fungal, FOM_Norg_used_Plant, FOM_Norg_used_Fungal, SOM_Norg_used, respiration_microbes_params, N_limits_R, N_k_R, SWC_k_R, NC_microbe_opt, microbe_turnover) {
    .Call(`_MycoModel_symphony_multiple_FOM_daily`, Tmb, SWC, C_FOM_needles_old, C_FOM_woody_old, C_FOM_roots_old, C_FOM_mantle_old, C_FOM_ERM_old, C_SOM_old, N_SOM_old, C_decompose_FOM, C_decompose_SOM, N_decompose_FOM, N_decompose_SOM, Litter_needles, Litter_woody, Litter_roots, Litter_mantle, Litter_ERM, imobilisation, assimilation, NH4_old, NO3_old, NC_needles, NC_woody, NC_roots, NC_mantle, NC_ERM, NH4_used_Plant, NH4_used_Fungal, NO3_used_Plant, NO3_used_Fungal, FOM_Norg_used_Plant, FOM_Norg_used_Fungal, SOM_Norg_used, respiration_microbes_params, N_limits_R, N_k_R, SWC_k_R, NC_microbe_opt, microbe_turnover)
}

symphony <- function(params) {
    .Call(`_MycoModel_symphony`, params)
}

symphony_plus <- function(params, Photosynthesis) {
    .Call(`_MycoModel_symphony_plus`, params, Photosynthesis)
}

symphony_plus_daily <- function(params, Photosynthesis, C_plant, C_FOM, N) {
    .Call(`_MycoModel_symphony_plus_daily`, params, Photosynthesis, C_plant, C_FOM, N)
}

