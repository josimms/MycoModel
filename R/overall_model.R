symphony_plus_vs_original_plot <- function() {
  symphony_parameters = c(0.0317917, # A
                          0.016906, # s
                          0.0368857, # r
                          0.00505757, # m_p
                          3.98799e-4, # e_p
                          0.00369772, # r_p
                          0.0121216, # k
                          0.0909091, # alpha
                          0.0142857, # beta
                          0.0110068, # i
                          0.00262647, # l
                          4.22868e-4, # y
                          0.00929094, # u
                          0.0289652, # e
                          400, # Ca
                          0.0627704) # psi i
  
  Photosynthesis = Rprebasso::PRELES(data_format$PAR, 
                                     data_format$T, 
                                     data_format$VPD, 
                                     data_format$Rain, 
                                     data_format$CO2, 
                                     data_format$fAPAR, 
                                     rep(1, length = nrow(data_format)))$GPP
  
  symphony_results = symphony(symphony_parameters)
  symphony_plus_results = symphony_plus(symphony_parameters, Photosynthesis)
  
  par(mfrow = c(3, 2))
  names = c("Carbon Decomposers FOM", "Carbon Decomposers SOM", "Carbon Plant", "Carbon FOM", "Mineral N in Soil", "Carbon in SOM")
  for (i in 1:length(symphony_plus_results)) {
    axis_min = min(symphony_plus_results[[i]], symphony_results[[i]])
    axis_max = max(symphony_plus_results[[i]], symphony_results[[i]])
    plot(symphony_plus_results[[i]], col = "blue", 
         main = names[i],
         xlab = "Days", ylim = c(axis_min, axis_max), 
         ylab = names(symphony_plus_results)[i], type = "l")
    lines(symphony_results[[i]], col = "black")
  }
}

nitrogen_graphs_plant <- function(NC_in_root_opt, Temperature, SoilWater, micorization, NH4, NO3, Norg, N_limits_R, N_k_R, SWC_k_R, C_roots, N_roots, C_fungal, percentage_C_biomass, parameters, C_value_param, N_value_param) {
  NH4_medium = 0.5*NH4
  NH4_low = 0.25*NH4
  
  # Indervidual functions should follow this pattern
  temp_change_N_Uptake <- SWC_change_N_Uptake <- conc_change_N_Uptake <- NULL
  temp_change_N_Uptake_Fungal <- SWC_change_N_Uptake_Fungal <- conc_change_N_Uptake_Fungal <- conc_change_Norg_Uptake_Fungal <- conc_change_NO3_Uptake_Fungal <- conc_change_NH4_Uptake_Fungal <- NULL
  temp_change_N_Uptake_Plant <- SWC_change_N_Uptake_Plant <- conc_change_NH4_Uptake_Plant_NH4_out <- conc_change_NH4_Uptake_Plant_NO3_out <- conc_change_NH4_Uptake_Plant_Norg_out <- NULL
  conc_change_Norg_Uptake_Plant <- conc_change_NO3_Uptake_Plant_NH4_10 <- conc_change_NO3_Uptake_Plant_NH4_5 <- conc_change_NO3_Uptake_Plant_NH4_2 <- conc_change_NH4_Uptake_Plant <- NULL
  conc_change_N_Uptake_Plant_NH4_out <- conc_change_N_Uptake_Plant_NO3_out <- conc_change_N_Uptake_Plant_Norg_out <- NULL
  conc_change_NH4_Uptake_Plant_NH4_out <- conc_change_NH4_Uptake_Plant_NO3_out <- conc_change_NH4_Uptake_Plant_Norg_out <- NULL
  conc_change_NO3_Uptake_Plant_NH4_out <- conc_change_NO3_Uptake_Plant_NO3_out <- conc_change_NO3_Uptake_Plant_Norg_out <- NULL
  conc_change_NO3_Uptake_Plant_NH4_10_NH4_out <- conc_change_NO3_Uptake_Plant_NH4_10_NO3_out <- conc_change_NO3_Uptake_Plant_NH4_10_Norg_out <- NULL
  conc_change_Norg_Uptake_Plant_NH4_out <- conc_change_Norg_Uptake_Plant_NO3_out <- conc_change_Norg_Uptake_Plant_Norg_out <- NULL
  temperature_range = -20:20
  count = 1
  for (Temp in temperature_range) {
    temp_change_N_Uptake[count] = uptake_N(10, Temp, 10, 0.2, 0.8, 0.5)
    temp_change_N_Uptake_Plant[count] = Plant_N_Uptake(NC_in_root_opt, Temp, SoilWater, micorization, NH4, NO3, Norg, N_limits_R, N_k_R, SWC_k_R, C_roots, N_roots, C_fungal, percentage_C_biomass, parameters, C_value_param, N_value_param)$N_to_plant
    count = count + 1
  }
  concentration_range = seq(0, 20, by = 0.01)
  count = 1
  for (conc in concentration_range) {
    conc_change_N_Uptake[count] <- uptake_N(conc, Temperature, 10, 0.2, SoilWater, 0.5)
    conc_change_N_Uptake_Plant[count] = Plant_N_Uptake(NC_in_root_opt, Temperature, SoilWater, micorization, conc, conc, conc, N_limits_R, N_k_R, SWC_k_R, C_roots, N_roots, C_fungal, percentage_C_biomass, parameters, C_value_param, N_value_param)$N_to_plant
    conc_change_N_Uptake_Plant_NH4_out[count] = Plant_N_Uptake(NC_in_root_opt, Temperature, SoilWater, micorization, conc, conc, conc, N_limits_R, N_k_R, SWC_k_R, C_roots, N_roots, C_fungal, percentage_C_biomass, parameters, C_value_param, N_value_param)$NH4_used
    conc_change_N_Uptake_Plant_NO3_out[count] = Plant_N_Uptake(NC_in_root_opt, Temperature, SoilWater, micorization, conc, conc, conc, N_limits_R, N_k_R, SWC_k_R, C_roots, N_roots, C_fungal, percentage_C_biomass, parameters, C_value_param, N_value_param)$NO3_used
    conc_change_N_Uptake_Plant_Norg_out[count] = Plant_N_Uptake(NC_in_root_opt, Temperature, SoilWater, micorization, conc, conc, conc, N_limits_R, N_k_R, SWC_k_R, C_roots, N_roots, C_fungal, percentage_C_biomass, parameters, C_value_param, N_value_param)$Norg_used
    conc_change_Norg_Uptake_Plant[count] = Plant_N_Uptake(NC_in_root_opt, Temperature, SoilWater, micorization, NH4, NO3, conc, N_limits_R, N_k_R, SWC_k_R, C_roots, N_roots, C_fungal, percentage_C_biomass, parameters, C_value_param, N_value_param)$N_to_plant
    conc_change_Norg_Uptake_Plant_NH4_out[count] = Plant_N_Uptake(NC_in_root_opt, Temperature, SoilWater, micorization, NH4, NO3, conc, N_limits_R, N_k_R, SWC_k_R, C_roots, N_roots, C_fungal, percentage_C_biomass, parameters, C_value_param, N_value_param)$N_to_plant
    conc_change_Norg_Uptake_Plant_NO3_out[count] = Plant_N_Uptake(NC_in_root_opt, Temperature, SoilWater, micorization, NH4, NO3, conc, N_limits_R, N_k_R, SWC_k_R, C_roots, N_roots, C_fungal, percentage_C_biomass, parameters, C_value_param, N_value_param)$NH4_used
    conc_change_Norg_Uptake_Plant_Norg_out[count] = Plant_N_Uptake(NC_in_root_opt, Temperature, SoilWater, micorization, NH4, NO3, conc, N_limits_R, N_k_R, SWC_k_R, C_roots, N_roots, C_fungal, percentage_C_biomass, parameters, C_value_param, N_value_param)$NO3_used
    conc_change_NO3_Uptake_Plant_NH4_10[count] = Plant_N_Uptake(NC_in_root_opt, Temperature, SoilWater, micorization, NH4, conc, Norg, N_limits_R, N_k_R, SWC_k_R, C_roots, N_roots, C_fungal, percentage_C_biomass, parameters, C_value_param, N_value_param)$Norg_used
    conc_change_NO3_Uptake_Plant_NH4_10_NH4_out[count] = Plant_N_Uptake(NC_in_root_opt, Temperature, SoilWater, micorization, NH4, conc, Norg, N_limits_R, N_k_R, SWC_k_R, C_roots, N_roots, C_fungal, percentage_C_biomass, parameters, C_value_param, N_value_param)$NH4_used
    conc_change_NO3_Uptake_Plant_NH4_10_NO3_out[count] = Plant_N_Uptake(NC_in_root_opt, Temperature, SoilWater, micorization, NH4, conc, Norg, N_limits_R, N_k_R, SWC_k_R, C_roots, N_roots, C_fungal, percentage_C_biomass, parameters, C_value_param, N_value_param)$NO3_used
    conc_change_NO3_Uptake_Plant_NH4_10_Norg_out[count] = Plant_N_Uptake(NC_in_root_opt, Temperature, SoilWater, micorization, NH4, conc, Norg, N_limits_R, N_k_R, SWC_k_R, C_roots, N_roots, C_fungal, percentage_C_biomass, parameters, C_value_param, N_value_param)$Norg_used
    conc_change_NO3_Uptake_Plant_NH4_5[count] = Plant_N_Uptake(NC_in_root_opt, Temperature, SoilWater, micorization, NH4_medium, conc, Norg, N_limits_R, N_k_R, SWC_k_R, C_roots, N_roots, C_fungal, percentage_C_biomass, parameters, C_value_param, N_value_param)$N_to_plant
    conc_change_NO3_Uptake_Plant_NH4_2[count] = Plant_N_Uptake(NC_in_root_opt, Temperature, SoilWater, micorization, NH4_low, conc, Norg, N_limits_R, N_k_R, SWC_k_R, C_roots, N_roots, C_fungal, percentage_C_biomass, parameters, C_value_param, N_value_param)$N_to_plant
    conc_change_NH4_Uptake_Plant[count] = Plant_N_Uptake(NC_in_root_opt, Temperature, SoilWater, micorization, conc, NO3, Norg, N_limits_R, N_k_R, SWC_k_R, C_roots, N_roots, C_fungal, percentage_C_biomass, parameters, C_value_param, N_value_param)$N_to_plant
    conc_change_NH4_Uptake_Plant_NH4_out[count] = Plant_N_Uptake(NC_in_root_opt, Temperature, SoilWater, micorization, conc, NO3, Norg, N_limits_R, N_k_R, SWC_k_R, C_roots, N_roots, C_fungal, percentage_C_biomass, parameters, C_value_param, N_value_param)$NH4_used
    conc_change_NH4_Uptake_Plant_NO3_out[count] = Plant_N_Uptake(NC_in_root_opt, Temperature, SoilWater, micorization, conc, NO3, Norg, N_limits_R, N_k_R, SWC_k_R, C_roots, N_roots, C_fungal, percentage_C_biomass, parameters, C_value_param, N_value_param)$NO3_used
    conc_change_NH4_Uptake_Plant_Norg_out[count] = Plant_N_Uptake(NC_in_root_opt, Temperature, SoilWater, micorization, conc, NO3, Norg, N_limits_R, N_k_R, SWC_k_R, C_roots, N_roots, C_fungal, percentage_C_biomass, parameters, C_value_param, N_value_param)$Norg_used
    count = count + 1
  }
  SWC_range = seq(0, 1, by = 0.01)
  count = 1
  for (SWC in SWC_range) {
    SWC_change_N_Uptake[count] <- uptake_N(10, 15, 10, 0.2, SWC, 0.5)
    SWC_change_N_Uptake_Plant[count] = Plant_N_Uptake(NC_in_root_opt, Temperature, SWC, micorization, NH4, NO3, Norg, N_limits_R, N_k_R, SWC_k_R, C_roots, N_roots, C_fungal, percentage_C_biomass, parameters, C_value_param, N_value_param)$N_to_plant
    count = count + 1
  }
  
  par(mfrow = c(3, 2))
  plot(concentration_range, conc_change_NH4_Uptake_Plant, xlab = "NH4", ylab = "total N to plant", main = "NH4 response (Plant) \n with temperature 15, SWC 0.8, other N 10")
  lines(concentration_range, conc_change_NH4_Uptake_Plant_NH4_out, col = "black", lty = 2, lwd = 2)
  lines(concentration_range, conc_change_NH4_Uptake_Plant_NO3_out, col = "blue", lty = 2, lwd = 2)
  lines(concentration_range, conc_change_NH4_Uptake_Plant_Norg_out, col = "green", lty = 2, lwd = 2)
  legend(0, max(unlist(c(conc_change_NH4_Uptake_Plant, conc_change_NH4_Uptake_Plant_NH4_out, conc_change_NH4_Uptake_Plant_NO3_out, conc_change_NH4_Uptake_Plant_Norg_out))),
         c("NH4", "No3", "FOM"), col = c("black", "blue", "green"), lty = 2, title = "N Type", bty = "n")
  
  plot(concentration_range, conc_change_Norg_Uptake_Plant, xlab = "Norg", ylab = "total N to plant",
       main = "Norg response (Plant) \n with temperature 15, SWC 0.8, other N 10",
       ylim = c(0, max(conc_change_Norg_Uptake_Plant)))
  lines(concentration_range, conc_change_Norg_Uptake_Plant_NH4_out, col = "black", lty = 2, lwd = 2)
  lines(concentration_range, conc_change_Norg_Uptake_Plant_NO3_out, col = "blue", lty = 2, lwd = 2)
  lines(concentration_range, conc_change_Norg_Uptake_Plant_Norg_out, col = "green", lty = 2, lwd = 2)
  
  plot(concentration_range, conc_change_NO3_Uptake_Plant_NH4_10, xlab = "NO3", ylab = "total N to plant", 
       main = "NO3 response (Plant) \n with temperature 15, SWC 0.8, other 10",
       ylim = c(0,  max(c(conc_change_NO3_Uptake_Plant_NH4_10))))
  lines(concentration_range, conc_change_NO3_Uptake_Plant_NH4_10_NH4_out, lty = 2, lwd = 2)
  lines(concentration_range, conc_change_NO3_Uptake_Plant_NH4_10_NO3_out, lty = 2, lwd = 2, col = "blue")
  lines(concentration_range, conc_change_NO3_Uptake_Plant_NH4_10_Norg_out, lty = 2, lwd = 2, col = "green")
  
  plot(concentration_range, conc_change_NO3_Uptake_Plant_NH4_10, xlab = "NO3", ylab = "total N to plant",
       main = "NO3 response (Plant) \n with temperature 15, SWC 0.8, Organic 10, NH4 variable",
       ylim = c(0,  max(c(conc_change_NO3_Uptake_Plant_NH4_10, conc_change_NO3_Uptake_Plant_NH4_5, conc_change_NO3_Uptake_Plant_NH4_2))))
  points(concentration_range, conc_change_NO3_Uptake_Plant_NH4_5, col = "pink")
  points(concentration_range, conc_change_NO3_Uptake_Plant_NH4_2, col = "red")
  legend(0, max(unlist(c(conc_change_NO3_Uptake_Plant_NH4_10, conc_change_NO3_Uptake_Plant_NH4_5, conc_change_NO3_Uptake_Plant_NH4_2))),
         c("High", "Medium", "Low"), col = c("black", "pink", "red"), pch = 1, title = "NH4", bty = "n")
  
  plot(temperature_range, temp_change_N_Uptake_Plant, xlab = "Temperature", ylab = "total N to plant", main = "Temperature response (Plant) \n with concentation 10, SWC 0.8")
  
  plot(SWC_range, SWC_change_N_Uptake, xlab = "SWC", ylab = "total N to plant", main = "SWC response (Plant) \n with concentation 10, temperature 15")
  
  # TODO: need to add other state variables esim. C_fungal, decision parameters / optimum root parameter
  
}


nitrogen_graphs_fungal <- function(C_fungal, N_fungal, NC_fungal_opt, mantle_mass, ERM_mass, percentage_C_biomass, Temperature, SoilWater,
                                   NH4, NO3, Norg, N_limits_R, N_k_R, SWC_k_R, C_value_param, N_value_param) {
  #####
  ## Generating the values
  #####
  temp_change_N_Uptake_Fungal <- SWC_change_N_Uptake_Fungal <- conc_change_N_Uptake_Fungal <- conc_change_Norg_Uptake_Fungal <- conc_change_NO3_Uptake_Fungal <- conc_change_NH4_Uptake_Fungal <- NULL
  conc_change_N_Uptake_Fungal_NH4_out <- conc_change_N_Uptake_Fungal_NO3_out <- conc_change_N_Uptake_Fungal_Norg_out <- NULL
  conc_change_NO3_Uptake_Fungal_NH4_10 <- conc_change_NO3_Uptake_Fungal_NH4_10_NH4_out <- conc_change_NO3_Uptake_Fungal_NH4_10_NO3_out <- conc_change_NO3_Uptake_Fungal_NH4_10_Norg_out <- NULL
  conc_change_NO3_Uptake_Fungal_NH4_5 <- conc_change_NO3_Uptake_Fungal_NH4_2 <- NULL
  conc_change_NH4_Uptake_Fungal_NH4_out <- conc_change_NH4_Uptake_Fungal_NO3_out <- conc_change_NH4_Uptake_Fungal_Norg_out <- NULL
  conc_change_Norg_Uptake_Fungal_NH4_out <- conc_change_Norg_Uptake_Fungal_NO3_out <- conc_change_Norg_Uptake_Fungal_Norg_out <- NULL
  
  temperature_range = -20:20
  count = 1
  for (Temp in temperature_range) {
    temp_change_N_Uptake_Fungal[count] = Fungal_N_Uptake(C_fungal, N_fungal, NC_fungal_opt, mantle_mass, ERM_mass, percentage_C_biomass, Temp, SoilWater, NH4, NO3, Norg, N_limits_R, N_k_R, SWC_k_R, C_value_param, N_value_param)$N_to_fungal
    count = count + 1
  }
  concentration_range = seq(0, 20, by = 0.01)
  count = 1
  for (conc in concentration_range) {
    conc_change_N_Uptake_Fungal[count] = Fungal_N_Uptake(C_fungal, N_fungal, NC_fungal_opt, mantle_mass, ERM_mass, percentage_C_biomass, Temperature, SoilWater,
                                                         conc, conc, conc, N_limits_R, N_k_R, SWC_k_R, C_value_param, N_value_param)$N_to_fungal
    conc_change_N_Uptake_Fungal_NH4_out[count] = Fungal_N_Uptake(C_fungal, N_fungal, NC_fungal_opt, mantle_mass, ERM_mass, percentage_C_biomass, Temperature, SoilWater,
                                                                 conc, conc, conc, N_limits_R, N_k_R, SWC_k_R, C_value_param, N_value_param)$NH4_used
    conc_change_N_Uptake_Fungal_NO3_out[count] = Fungal_N_Uptake(C_fungal, N_fungal, NC_fungal_opt, mantle_mass, ERM_mass, percentage_C_biomass, Temperature, SoilWater, 
                                                                 conc, conc, conc, N_limits_R, N_k_R, SWC_k_R, C_value_param, N_value_param)$NO3_used
    conc_change_N_Uptake_Fungal_Norg_out[count] = Fungal_N_Uptake(C_fungal, N_fungal, NC_fungal_opt, mantle_mass, ERM_mass, percentage_C_biomass, Temperature, SoilWater,
                                                                  conc, conc, conc, N_limits_R, N_k_R, SWC_k_R, C_value_param, N_value_param)$Norg_used
    conc_change_NO3_Uptake_Fungal_NH4_10[count] = Fungal_N_Uptake(C_fungal, N_fungal, NC_fungal_opt, mantle_mass, ERM_mass, percentage_C_biomass, Temperature, SoilWater,
                                                                  NH4, conc, Norg, N_limits_R, N_k_R, SWC_k_R, C_value_param, N_value_param)$N_to_fungal
    conc_change_NO3_Uptake_Fungal_NH4_10_NH4_out[count] = Fungal_N_Uptake(C_fungal, N_fungal, NC_fungal_opt, mantle_mass, ERM_mass, percentage_C_biomass, Temperature, SoilWater,
                                                                          NH4, conc, Norg, N_limits_R, N_k_R, SWC_k_R, C_value_param, N_value_param)$NH4_used
    conc_change_NO3_Uptake_Fungal_NH4_10_NO3_out[count] = Fungal_N_Uptake(C_fungal, N_fungal, NC_fungal_opt, mantle_mass, ERM_mass, percentage_C_biomass, Temperature, SoilWater,
                                                                          NH4, conc, Norg, N_limits_R, N_k_R, SWC_k_R, C_value_param, N_value_param)$NO3_used
    conc_change_NO3_Uptake_Fungal_NH4_10_Norg_out[count] = Fungal_N_Uptake(C_fungal, N_fungal, NC_fungal_opt, mantle_mass, ERM_mass, percentage_C_biomass, Temperature, SoilWater,
                                                                           NH4, conc, Norg, N_limits_R, N_k_R, SWC_k_R, C_value_param, N_value_param)$Norg_used
    conc_change_NO3_Uptake_Fungal_NH4_5[count] = Fungal_N_Uptake(C_fungal, N_fungal, NC_fungal_opt, mantle_mass, ERM_mass, percentage_C_biomass, Temperature, SoilWater,
                                                                 NH4_medium, conc, Norg, N_limits_R, N_k_R, SWC_k_R, C_value_param, N_value_param)$N_to_fungal
    conc_change_NO3_Uptake_Fungal_NH4_2[count] = Fungal_N_Uptake(C_fungal, N_fungal, NC_fungal_opt, mantle_mass, ERM_mass, percentage_C_biomass, Temperature, SoilWater,
                                                                 NH4_low, conc, Norg, N_limits_R, N_k_R, SWC_k_R, C_value_param, N_value_param)$N_to_fungal
    conc_change_NH4_Uptake_Fungal[count] = Fungal_N_Uptake(C_fungal, N_fungal, NC_fungal_opt, mantle_mass, ERM_mass, percentage_C_biomass, Temperature, SoilWater,
                                                           conc, NO3, Norg, N_limits_R, N_k_R, SWC_k_R, C_value_param, N_value_param)$N_to_fungal
    conc_change_NH4_Uptake_Fungal_NH4_out[count] = Fungal_N_Uptake(C_fungal, N_fungal, NC_fungal_opt, mantle_mass, ERM_mass, percentage_C_biomass, Temperature, SoilWater,
                                                                   conc, NO3, Norg, N_limits_R, N_k_R, SWC_k_R, C_value_param, N_value_param)$NH4_used
    conc_change_NH4_Uptake_Fungal_NO3_out[count] = Fungal_N_Uptake(C_fungal, N_fungal, NC_fungal_opt, mantle_mass, ERM_mass, percentage_C_biomass, Temperature, SoilWater,
                                                                   conc, NO3, Norg, N_limits_R, N_k_R, SWC_k_R, C_value_param, N_value_param)$NO3_used
    conc_change_NH4_Uptake_Fungal_Norg_out[count] = Fungal_N_Uptake(C_fungal, N_fungal, NC_fungal_opt, mantle_mass, ERM_mass, percentage_C_biomass, Temperature, SoilWater,
                                                                    conc, NO3, Norg, N_limits_R, N_k_R, SWC_k_R, C_value_param, N_value_param)$Norg_used
    conc_change_Norg_Uptake_Fungal[count] = Fungal_N_Uptake(C_fungal, N_fungal, NC_fungal_opt, mantle_mass, ERM_mass, percentage_C_biomass, Temperature, SoilWater,
                                                            NH4, NO3, conc, N_limits_R, N_k_R, SWC_k_R, C_value_param, N_value_param)$N_to_fungal
    conc_change_Norg_Uptake_Fungal_NH4_out[count] = Fungal_N_Uptake(C_fungal, N_fungal, NC_fungal_opt, mantle_mass, ERM_mass, percentage_C_biomass, Temperature, SoilWater,
                                                                    NH4, NO3, conc, N_limits_R, N_k_R, SWC_k_R, C_value_param, N_value_param)$NH4_used
    conc_change_Norg_Uptake_Fungal_NO3_out[count] = Fungal_N_Uptake(C_fungal, N_fungal, NC_fungal_opt, mantle_mass, ERM_mass, percentage_C_biomass, Temperature, SoilWater,
                                                                    NH4, NO3, conc, N_limits_R, N_k_R, SWC_k_R, C_value_param, N_value_param)$NO3_used
    conc_change_Norg_Uptake_Fungal_Norg_out[count] = Fungal_N_Uptake(C_fungal, N_fungal, NC_fungal_opt, mantle_mass, ERM_mass, percentage_C_biomass, Temperature, SoilWater,
                                                                     NH4, NO3, conc, N_limits_R, N_k_R, SWC_k_R, C_value_param, N_value_param)$Norg_used
    count = count + 1
  }
  
  SWC_range = seq(0, 1, by = 0.01)
  count = 1
  for (SWC in SWC_range) {
    SWC_change_N_Uptake_Fungal[count] = Fungal_N_Uptake(C_fungal, N_fungal, NC_fungal_opt, mantle_mass, ERM_mass, percentage_C_biomass, Temperature, SWC, 
                                                        NH4, NO3, Norg, N_limits_R, N_k_R, SWC_k_R, C_value_param, N_value_param)$N_to_fungal
    count = count + 1
  }
  
  #####
  ## Plots
  #####
  par(mfrow = c(3, 2))
  plot(concentration_range, conc_change_NH4_Uptake_Fungal, xlab = "NH4", ylab = "total N to Fungal", main = "NH4 response (Fungal) \n with temperature 15, SWC 0.8, other N 10")
  lines(concentration_range, conc_change_NH4_Uptake_Fungal_NH4_out, col = "black", lty = 2, lwd = 2)
  lines(concentration_range, conc_change_NH4_Uptake_Fungal_NO3_out, col = "blue", lty = 2, lwd = 2)
  lines(concentration_range, conc_change_NH4_Uptake_Fungal_Norg_out, col = "green", lty = 2, lwd = 2)
  legend(0, max(unlist(c(conc_change_NH4_Uptake_Fungal, conc_change_NH4_Uptake_Fungal_NH4_out, conc_change_NH4_Uptake_Fungal_NO3_out, conc_change_NH4_Uptake_Fungal_Norg_out))),
         c("NH4", "NO3", "FOM"), col = c("black", "blue", "green"), lty = 2, title = "N Type", bty = "n")
  
  plot(concentration_range, conc_change_Norg_Uptake_Fungal, xlab = "Norg", ylab = "total N to Fungal",
       main = "Norg response (Fungal) \n with temperature 15, SWC 0.8, other N 10",
       ylim = c(0, max(conc_change_Norg_Uptake_Fungal)))
  lines(concentration_range, conc_change_Norg_Uptake_Fungal_NH4_out, col = "black", lty = 2, lwd = 2)
  lines(concentration_range, conc_change_Norg_Uptake_Fungal_NO3_out, col = "blue", lty = 2, lwd = 2)
  lines(concentration_range, conc_change_Norg_Uptake_Fungal_Norg_out, col = "green", lty = 2, lwd = 2)
  
  plot(concentration_range, conc_change_NO3_Uptake_Fungal_NH4_10, xlab = "NO3", ylab = "total N to Fungal", 
       main = "NO3 response (Fungal) \n with temperature 15, SWC 0.8, other 10",
       ylim = c(0,  max(c(conc_change_NO3_Uptake_Fungal_NH4_10))))
  lines(concentration_range, conc_change_NO3_Uptake_Fungal_NH4_10_NH4_out, lty = 2, lwd = 2)
  lines(concentration_range, conc_change_NO3_Uptake_Fungal_NH4_10_NO3_out, lty = 2, lwd = 2, col = "blue")
  lines(concentration_range, conc_change_NO3_Uptake_Fungal_NH4_10_Norg_out, lty = 2, lwd = 2, col = "green")
  
  plot(concentration_range, conc_change_NO3_Uptake_Fungal_NH4_10, xlab = "NO3", ylab = "total N to Fungal",
       main = "NO3 response (Fungal) \n with temperature 15, SWC 0.8, Organic 10, NH4 variable",
       ylim = c(0,  max(c(conc_change_NO3_Uptake_Fungal_NH4_10, conc_change_NO3_Uptake_Fungal_NH4_5, conc_change_NO3_Uptake_Fungal_NH4_2))))
  points(concentration_range, conc_change_NO3_Uptake_Fungal_NH4_5, col = "pink")
  points(concentration_range, conc_change_NO3_Uptake_Fungal_NH4_2, col = "red")
  legend(0, max(unlist(c(conc_change_NO3_Uptake_Fungal_NH4_10, conc_change_NO3_Uptake_Fungal_NH4_5, conc_change_NO3_Uptake_Fungal_NH4_2))),
         c("High", "Medium", "Low"), col = c("black", "pink", "red"), pch = 1, title = "NH4", bty = "n")
  
  plot(temperature_range, temp_change_N_Uptake_Fungal, xlab = "Temperature", ylab = "total N to Fungal", main = "Temperature response (Fungal) \n with concentation 10, SWC 0.8")
  
  plot(SWC_range, SWC_change_N_Uptake, xlab = "SWC", ylab = "total N to Fungal", main = "SWC response (Fungal) \n with concentation 10, temperature 15")
}

nitrogen_graphs <- function(N_type, Temperature, N_limit, k, SoilWater, SoilWaterk) {
  temp_change_N_Uptake <- SWC_change_N_Uptake <- conc_change_N_Uptake <- NULL
  
  temperature_range = -20:20
  count = 1
  for (Temp in temperature_range) {
    temp_change_N_Uptake[count] = uptake_N(N_type, Temp, N_limit, k, SoilWater, SoilWaterk)
    count = count + 1
  }
  
  concentration_range = seq(0, 20, by = 0.01)
  count = 1
  for (conc in concentration_range) {
    conc_change_N_Uptake[count] <- uptake_N(conc, Temperature, N_limit, k, SoilWater, SoilWaterk)
    count = count + 1
  }
  
  SWC_range = seq(0, 1, by = 0.01)
  count = 1
  for (SWC in SWC_range) {
    SWC_change_N_Uptake[count] <- uptake_N(N_type, Temperature, N_limit, k, SWC, SoilWaterk)
    count = count + 1
  }
  
  par(mfrow = c(3, 1))
  plot(concentration_range, conc_change_N_Uptake, xlab = "Concentration", ylab = "Uptake of N", main = "Concentration response \n with temperature 15, SWC 0.8")
  plot(temperature_range, temp_change_N_Uptake, xlab = "Temperature", ylab = "Uptake of N", main = "Temperature response \n with concentation 10, SWC 0.8")
  plot(SWC_range, SWC_change_N_Uptake, xlab = "SWC", ylab = "Uptake of N", main = "SWC response \n with concentation 10, temperature 15")
}

nitrogen_graphs_microbe <- function(C_microbe, N_microbe, NC_microbe_opt, NH4, NO3, Norg, Temperature, SoilWater, N_limits_microbes, N_k_microbes, SWC_k_microbes, SOM_used, Forg_FOM, Rm, Q10) {
  
  NH4_medium = NH4*0.5
  NH4_low = NH4*0.2
  
  #####
  ## Generating the values
  #####
  temp_change_N_Uptake_Microbes_C_out <- SWC_change_N_Uptake_Microbes_C_out <- conc_change_N_Uptake_Microbes_C_out <- conc_change_Norg_Uptake_Microbes_C_out <- conc_change_NO3_Uptake_Microbes_C_out <- conc_change_NH4_Uptake_Microbes_C_out <- NULL
  conc_change_N_Uptake_Microbes_NH4_out <- conc_change_N_Uptake_Microbes_NO3_out <- conc_change_N_Uptake_Microbes_Norg_out <- NULL
  temp_change_N_Uptake_Microbes_NH4_out <- temp_change_N_Uptake_Microbes_NO3_out <- temp_change_N_Uptake_Microbes_Norg_out <- NULL
  conc_change_NO3_Uptake_Microbes_NH4_1000_C_out <- conc_change_NO3_Uptake_Microbes_NH4_1000_NH4_out <- conc_change_NO3_Uptake_Microbes_NH4_1000_NO3_out <- conc_change_NO3_Uptake_Microbes_NH4_1000_Norg_out <- NULL
  conc_change_NO3_Uptake_Microbes_NH4_500_C_out <- conc_change_NO3_Uptake_Microbes_NH4_200_C_out <- NULL
  conc_change_NH4_Uptake_Microbes_NH4_out <- conc_change_NH4_Uptake_Microbes_NO3_out <- conc_change_NH4_Uptake_Microbes_Norg_out <- NULL
  conc_change_Norg_Uptake_Microbes_NH4_out <- conc_change_Norg_Uptake_Microbes_NO3_out <- conc_change_Norg_Uptake_Microbes_Norg_out <- NULL
  conc_change_Norg_FOM_Uptake_Microbes <- NULL
  
  temperature_range = -20:20
  count = 1
  for (Temp in temperature_range) {
    temp_change_N_Uptake_Microbes_C_out[count] = Microbe_Uptake(C_microbe, N_microbe, NC_microbe_opt, NH4, NO3, Norg, Temp, SoilWater, N_limits_microbes, N_k_microbes, SWC_k_microbes, SOM_used, Forg_FOM, c(Rm, Q10))$C_uptaken
    temp_change_N_Uptake_Microbes_NH4_out[count] = Microbe_Uptake(C_microbe, N_microbe, NC_microbe_opt, NH4, NO3, Norg, Temp, SoilWater, N_limits_microbes, N_k_microbes, SWC_k_microbes, SOM_used, Forg_FOM, c(Rm, Q10))$NH4_uptaken
    temp_change_N_Uptake_Microbes_NO3_out[count] = Microbe_Uptake(C_microbe, N_microbe, NC_microbe_opt, NH4, NO3, Norg, Temp, SoilWater, N_limits_microbes, N_k_microbes, SWC_k_microbes, SOM_used, Forg_FOM, c(Rm, Q10))$NO3_uptaken
    temp_change_N_Uptake_Microbes_Norg_out[count] = Microbe_Uptake(C_microbe, N_microbe, NC_microbe_opt, NH4, NO3, Norg, Temp, SoilWater, N_limits_microbes, N_k_microbes, SWC_k_microbes, SOM_used, Forg_FOM, c(Rm, Q10))$Norg_uptaken
    count = count + 1
  }
  concentration_range = seq(0, 10000, by = 1)
  count = 1
  for (conc in concentration_range) {
    conc_change_N_Uptake_Microbes_C_out[count] = Microbe_Uptake(C_microbe, N_microbe, NC_microbe_opt, conc, conc, conc, Temperature, SoilWater, N_limits_microbes, N_k_microbes, SWC_k_microbes, SOM_used, Forg_FOM, c(Rm, Q10))$C_uptaken
    conc_change_N_Uptake_Microbes_NH4_out[count] = Microbe_Uptake(C_microbe, N_microbe, NC_microbe_opt, conc, conc, conc, Temperature, SoilWater, N_limits_microbes, N_k_microbes, SWC_k_microbes, SOM_used, Forg_FOM, c(Rm, Q10))$NH4_uptaken
    conc_change_N_Uptake_Microbes_NO3_out[count] = Microbe_Uptake(C_microbe, N_microbe, NC_microbe_opt, conc, conc, conc, Temperature, SoilWater, N_limits_microbes, N_k_microbes, SWC_k_microbes, SOM_used, Forg_FOM, c(Rm, Q10))$NO3_uptaken
    conc_change_N_Uptake_Microbes_Norg_out[count] = Microbe_Uptake(C_microbe, N_microbe, NC_microbe_opt, conc, conc, conc, Temperature, SoilWater, N_limits_microbes, N_k_microbes, SWC_k_microbes, SOM_used, Forg_FOM, c(Rm, Q10))$Norg_uptaken
    conc_change_Norg_FOM_Uptake_Microbes[count] = Microbe_Uptake(C_microbe, N_microbe, NC_microbe_opt, conc, conc, conc, Temperature, SoilWater, N_limits_microbes, N_k_microbes, SWC_k_microbes, SOM_used, Forg_FOM, c(Rm, Q10))$Norg_uptaken_extra_FOM
    conc_change_NO3_Uptake_Microbes_NH4_1000_C_out[count] = Microbe_Uptake(C_microbe, N_microbe, NC_microbe_opt, NH4, conc, Norg, Temperature, SoilWater, N_limits_microbes, N_k_microbes, SWC_k_microbes, SOM_used, Forg_FOM, c(Rm, Q10))$C_uptaken
    conc_change_NO3_Uptake_Microbes_NH4_1000_NH4_out[count] = Microbe_Uptake(C_microbe, N_microbe, NC_microbe_opt, NH4, conc, Norg, Temperature, SoilWater, N_limits_microbes, N_k_microbes, SWC_k_microbes, SOM_used, Forg_FOM, c(Rm, Q10))$NH4_uptaken
    conc_change_NO3_Uptake_Microbes_NH4_1000_NO3_out[count] = Microbe_Uptake(C_microbe, N_microbe, NC_microbe_opt, NH4, conc, Norg, Temperature, SoilWater, N_limits_microbes, N_k_microbes, SWC_k_microbes, SOM_used, Forg_FOM, c(Rm, Q10))$NO3_uptaken
    conc_change_NO3_Uptake_Microbes_NH4_1000_Norg_out[count] = Microbe_Uptake(C_microbe, N_microbe, NC_microbe_opt, NH4, conc, Norg, Temperature, SoilWater, N_limits_microbes, N_k_microbes, SWC_k_microbes, SOM_used, Forg_FOM, c(Rm, Q10))$Norg_uptaken
    conc_change_NO3_Uptake_Microbes_NH4_500_C_out[count] = Microbe_Uptake(C_microbe, N_microbe, NC_microbe_opt, NH4_medium, conc, Norg, Temperature, SoilWater, N_limits_microbes, N_k_microbes, SWC_k_microbes, SOM_used, Forg_FOM, c(Rm, Q10))$C_uptaken
    conc_change_NO3_Uptake_Microbes_NH4_200_C_out[count] = Microbe_Uptake(C_microbe, N_microbe, NC_microbe_opt, NH4_low, conc, Norg, Temperature, SoilWater, N_limits_microbes, N_k_microbes, SWC_k_microbes, SOM_used, Forg_FOM, c(Rm, Q10))$C_uptaken
    conc_change_NH4_Uptake_Microbes_C_out[count] = Microbe_Uptake(C_microbe, N_microbe, NC_microbe_opt, conc, NO3, Norg, Temperature, SoilWater, N_limits_microbes, N_k_microbes, SWC_k_microbes, SOM_used, Forg_FOM, c(Rm, Q10))$C_uptaken
    conc_change_NH4_Uptake_Microbes_NH4_out[count] = Microbe_Uptake(C_microbe, N_microbe, NC_microbe_opt, conc, NO3, Norg, Temperature, SoilWater, N_limits_microbes, N_k_microbes, SWC_k_microbes, SOM_used, Forg_FOM, c(Rm, Q10))$NH4_uptaken
    conc_change_NH4_Uptake_Microbes_NO3_out[count] = Microbe_Uptake(C_microbe, N_microbe, NC_microbe_opt, conc, NO3, Norg, Temperature, SoilWater, N_limits_microbes, N_k_microbes, SWC_k_microbes, SOM_used, Forg_FOM, c(Rm, Q10))$NO3_uptaken
    conc_change_NH4_Uptake_Microbes_Norg_out[count] = Microbe_Uptake(C_microbe, N_microbe, NC_microbe_opt, conc, NO3, Norg, Temperature, SoilWater, N_limits_microbes, N_k_microbes, SWC_k_microbes, SOM_used, Forg_FOM, c(Rm, Q10))$Norg_uptaken
    conc_change_Norg_Uptake_Microbes_C_out[count] = Microbe_Uptake(C_microbe, N_microbe, NC_microbe_opt, NH4, NO3, conc, Temperature, SoilWater, N_limits_microbes, N_k_microbes, SWC_k_microbes, SOM_used, Forg_FOM, c(Rm, Q10))$C_uptaken
    conc_change_Norg_Uptake_Microbes_NH4_out[count] = Microbe_Uptake(C_microbe, N_microbe, NC_microbe_opt, NH4, NO3, conc, Temperature, SoilWater, N_limits_microbes, N_k_microbes, SWC_k_microbes, SOM_used, Forg_FOM, c(Rm, Q10))$NH4_uptaken
    conc_change_Norg_Uptake_Microbes_NO3_out[count] = Microbe_Uptake(C_microbe, N_microbe, NC_microbe_opt, NH4, NO3, conc, Temperature, SoilWater, N_limits_microbes, N_k_microbes, SWC_k_microbes, SOM_used, Forg_FOM, c(Rm, Q10))$NO3_uptaken
    conc_change_Norg_Uptake_Microbes_Norg_out[count] = Microbe_Uptake(C_microbe, N_microbe, NC_microbe_opt, NH4, NO3, conc, Temperature, SoilWater, N_limits_microbes, N_k_microbes, SWC_k_microbes, SOM_used, Forg_FOM, c(Rm, Q10))$Norg_uptaken
    count = count + 1
  }
  SWC_range = seq(0, 1, by = 0.01)
  count = 1
  for (SWC in SWC_range) {
    SWC_change_N_Uptake_Microbes_C_out[count] = Microbe_Uptake(2000, 1500, NC_microbe_opt, NH4, NO3, Norg, Temperature, SWC, N_limits_microbes, N_k_microbes, SWC_k_microbes, SOM_used, Forg_FOM, c(Rm, Q10))$C_uptaken
    count = count + 1
  }
  
  #####
  ## Plots
  #####
  
  par(mfrow = c(3, 3))
  plot(concentration_range, conc_change_NH4_Uptake_Microbes_C_out, xlab = "NH4", ylab = "total N to Microbes",
       main = "NH4 response (Microbes) \n with temperature 15, SWC 0.8, other N 1000")
  lines(concentration_range, conc_change_NH4_Uptake_Microbes_NH4_out, col = "black", lty = 2, lwd = 2)
  lines(concentration_range, conc_change_NH4_Uptake_Microbes_NO3_out, col = "blue", lty = 2, lwd = 2)
  lines(concentration_range, conc_change_NH4_Uptake_Microbes_Norg_out, col = "green", lty = 2, lwd = 2)
  legend(0, max(unlist(c(conc_change_NH4_Uptake_Microbes_NH4_out, conc_change_NH4_Uptake_Microbes_NH4_out, conc_change_NH4_Uptake_Microbes_NO3_out, conc_change_NH4_Uptake_Microbes_Norg_out))),
         c("NH4", "NO3", "FOM"), col = c("black", "blue", "green"), lty = 2, title = "N Type", bty = "n")
  
  plot(concentration_range, conc_change_Norg_FOM_Uptake_Microbes, xlab = "Norg", ylab = "Emergancy N to microbes",
       main = "Extra Norg from FOM")
  
  plot(concentration_range, conc_change_Norg_Uptake_Microbes_C_out, xlab = "Norg", ylab = "total N to Microbes",
       main = "Norg response (Microbes) \n with temperature 15, SWC 0.8, other N 1000")
  lines(concentration_range, conc_change_Norg_Uptake_Microbes_NH4_out, col = "black", lty = 2, lwd = 2)
  lines(concentration_range, conc_change_Norg_Uptake_Microbes_NO3_out, col = "blue", lty = 2, lwd = 2)
  lines(concentration_range, conc_change_Norg_Uptake_Microbes_Norg_out, col = "green", lty = 2, lwd = 2)
  
  plot(concentration_range, conc_change_NO3_Uptake_Microbes_NH4_1000_C_out, xlab = "NO3", ylab = "total N to Microbes", 
       main = "NO3 response (Microbes) \n with temperature 15, SWC 0.8, other 1000")
  lines(concentration_range, conc_change_NO3_Uptake_Microbes_NH4_1000_NH4_out, lty = 2, lwd = 2)
  lines(concentration_range, conc_change_NO3_Uptake_Microbes_NH4_1000_NO3_out, lty = 2, lwd = 2, col = "blue")
  lines(concentration_range, conc_change_NO3_Uptake_Microbes_NH4_1000_Norg_out, lty = 2, lwd = 2, col = "green")
  
  plot(concentration_range, conc_change_NO3_Uptake_Microbes_NH4_1000_C_out, xlab = "NO3", ylab = "total N to Microbes",
       main = "NO3 response (Microbes) \n with temperature 15, SWC 0.8, Organic 10, NH4 variable")
  points(concentration_range, conc_change_NO3_Uptake_Microbes_NH4_500_C_out, col = "pink")
  points(concentration_range, conc_change_NO3_Uptake_Microbes_NH4_200_C_out, col = "red")
  legend(0, max(unlist(c(conc_change_NO3_Uptake_Microbes_NH4_1000_C_out, conc_change_NO3_Uptake_Microbes_NH4_500_C_out, conc_change_NO3_Uptake_Microbes_NH4_200_C_out))),
         c("High", "Medium", "Low"), col = c("black", "pink", "red"), pch = 1, title = "NH4", bty = "n")
  
  plot(temperature_range, temp_change_N_Uptake_Microbes_C_out, xlab = "Temperature", ylab = "total N to Microbes", main = "Temperature response (Microbes) \n with concentation 10, SWC 0.8")
  
  plot(SWC_range, SWC_change_N_Uptake_Microbes_C_out, xlab = "SWC", ylab = "total N to Microbes", main = "SWC response (Microbes) \n with concentation 10, temperature 15")
}

decision_graphs <- function(C_roots, N_roots, C_fungal, N_fungal, optimum_point_roots, optimum_point_fungal, percentage_C_biomasses, 
                            mantle_mass, ERM_mass, C_value_plant, N_value_plant, C_value_fungal, N_value_fungal) {
  ####
  # Initialisation
  ####
  plant_decision_C <- plant_decision_N <- plant_decision_op <- plant_decision_C_value <- plant_decision_N_value <- NULL
  myco_decision_C <- myco_decision_N <- myco_decision_op <- myco_decision_C_value <- 
    myco_decision_N_value <- myco_decision_mm <- myco_decision_ERM <- myco_decision_pC <- NULL
  
  ####
  # State tests
  ####
  C_range <- seq(100, 300)
  count = 1
  for (C in C_range) {
    plant_decision_C[count] <- plant_decision(C_roots, N_roots, optimum_point_roots, C_value_plant, N_value_plant)$demand
    myco_decision_C[count] <- myco_decision(C_fungal, N_fungal, optimum_point_fungal, mantle_mass, ERM_mass, percentage_C_biomasses, C_value_fungal, N_value_fungal)$demand
    count = count + 1
  }
  N_range <- seq(50, 250)
  count = 1
  for (N in N_range) {
    plant_decision_N[count] <- plant_decision(C_roots, N, optimum_point_roots, C_value_param_plant, N_value_param_plant)$demand
    myco_decision_N[count] <- myco_decision(C_fungal, N, optimum_point_fungal, mantle_mass, ERM_mass, percentage_C_biomasses, C_value_fungal, N_value_fungal)$demand
    count = count + 1
  }
  optimum_point <- seq(0.2, 0.8, by = 0.01)
  count = 1
  for (op in optimum_point) {
    plant_decision_op[count] <- plant_decision(C_roots, N_roots, op, C_value_param_plant, N_value_param_plant)$demand
    myco_decision_op[count] <- myco_decision(C_roots, N_roots, op, mantle_mass, ERM_mass, percentage_C_biomasses, C_value_fungal, N_value_fungal)$demand
    count = count + 1
  }
  mantle_masses <- seq(10, 50, by = 1)
  count = 1
  for (mm in mantle_masses) {
    myco_decision_mm[count] <- myco_decision(C_fungal, N_fungal, optimum_point_fungal, mm, ERM_mass, percentage_C_biomasses, C_value_fungal, N_value_fungal)$demand
    count = count + 1
  }
  ERM_masses <- seq(110, 250, by = 1)
  count = 1
  for (ERM in ERM_masses) {
    myco_decision_ERM[count] <- myco_decision(C_fungal, N_fungal, optimum_point_fungal, mantle_mass, ERM, percentage_C_biomasses, C_value_fungal, N_value_fungal)$demand
    count = count + 1
  }
  percentage_C_biomass <- seq(0.1, 0.8, by = 0.01)
  count = 1
  for (pC in percentage_C_biomass) {
    myco_decision_pC[count] <- myco_decision(C_fungal, N_fungal, optimum_point_fungal, mantle_mass, ERM_mass, pC, C_value_fungal, N_value_fungal)$demand
    count = count + 1
  }
  C_value_range <- seq(-300, 300)
  count = 1
  for (C_value in C_value_range) {
    plant_decision_C_value[count] <- plant_decision(C_roots, N_roots, optimum_point_roots, C_value, N_value_plant)$demand
    myco_decision_C_value[count] <- myco_decision(C_fungal, N_fungal, optimum_point_fungal, mantle_mass, ERM_mass, percentage_C_biomasses, C_value, C_value_fungal)$demand
    count = count + 1
  }
  N_value_range <- seq(-300, 300)
  count = 1
  for (N_value in N_value_range) {
    plant_decision_N_value[count] <- plant_decision(C_roots, N_roots, optimum_point_fungal, C_value_fungal, N_value)$demand
    myco_decision_N_value[count] <- myco_decision(C_fungal, N_fungal, optimum_point_fungal, mantle_mass, ERM_mass, percentage_C_biomasses, C_value_fungal, N_value)$demand
    count = count + 1
  }
  
  
  ####
  # Plots
  ####
  par(mfrow = c(3, 3))
  plot(C_range, plant_decision_C, col = "black",
       ylab = "Decision value", xlab = "C concentration",
       main = "C range test", 
       ylim = c(min(c(myco_decision_C, plant_decision_C)), max(c(myco_decision_C, plant_decision_C))))
  points(C_range, myco_decision_C, col = "red")
  legend(C_range[1], max(myco_decision_C), c("Plant", "Myco"), col = c("black", "red"),
         title = "Organism", bty = "n")
  
  plot(N_range, plant_decision_N, col = "black",
       ylab = "Decision value", xlab = "N concentration",
       main = "N range test", 
       ylim = c(min(c(myco_decision_N, plant_decision_N)), max(c(myco_decision_N, plant_decision_N))))
  points(N_range, myco_decision_N, col = "red")
  legend(N_range[1], max(myco_decision_N), c("Plant", "Myco"), col = c("black", "red"),
         title = "Organism", bty = "n")

  plot(optimum_point, plant_decision_op, col = "black",
       ylab = "Decision value", xlab = "Optimum point",
       main = "Optimum point range test", 
       ylim = c(min(c(plant_decision_op, myco_decision_op)), max(c(plant_decision_op, myco_decision_N))))
  points(optimum_point, myco_decision_op, col = "red")
  legend(optimum_point[1], max(myco_decision_op), c("Plant", "Myco"), col = c("black", "red"),
         title = "Organism", bty = "n")

  plot(mantle_masses, myco_decision_mm, col = "red",
       ylab = "Decision value", xlab = "Mantle Mass",
       main = "Mantle Mass range", 
       ylim = c(min(myco_decision_mm), max(myco_decision_mm)))
  
  plot(ERM_masses, myco_decision_ERM, col = "red",
       ylab = "Decision value", xlab = "ERM range",
       main = "ERM Mass range", 
       ylim = c(min(myco_decision_ERM), max(myco_decision_ERM)))
  
  plot(percentage_C_biomass, myco_decision_pC, col = "red",
       ylab = "Decision value", xlab = "ERM range",
       main = "Percentange of C range", 
       ylim = c(min(myco_decision_pC), max(myco_decision_pC)))
  
  plot(C_value_range, plant_decision_C_value, col = "black",
       ylab = "Decision value", xlab = "C value",
       main = "C value range test", 
       ylim = c(min(c(myco_decision_C_value, plant_decision_C_value)), max(c(myco_decision_C_value, plant_decision_C_value))))
  points(C_value_range, myco_decision_C_value, col = "red")
  legend(C_value_range[1], max(myco_decision_C_value), c("Plant", "Myco"), col = c("black", "red"),
         title = "Organism", bty = "n")
  
  plot(N_value_range, plant_decision_N_value, col = "black",
       ylab = "Decision value", xlab = "N value",
       main = "N range test", 
       ylim = c(min(c(myco_decision_N_value, plant_decision_N_value)), max(c(myco_decision_N_value, plant_decision_N_value))))
  points(N_value_range, myco_decision_N_value, col = "red")
  legend(N_value_range[1], max(myco_decision_N_value), c("Plant", "Myco"), col = c("black", "red"),
         title = "Organism", bty = "n")
}

respiration_graphs <- function(Rm, Q10) {
  temp_respiration <- NULL
  temp = -30:30
  count = 1
  for (tem in temp) {
    temp_respiration[count] <- respiration(tem, Rm, Q10)
    count = count + 1
  }
  
  par(mfrow = c(1, 1))
  plot(temp, temp_respiration, main = "Respiration", 
       xlab = "Temperature range", ylab = "Respiration, kg C")
}

myco_growth_graphs <- function(C_fungal_in, N_fungal_in, gorwth_C, growth_N) {
  myco_growth_C_fungal <- myco_growth_N_fungal <- myco_growth_C_and_N_fungal <- NULL
  range <- 0:150
  count = 1
  for (ele in range) {
    myco_growth_C_and_N_fungal[count] <- myco_growth(ele, ele, gorwth_C, growth_N)
    myco_growth_C_fungal[count] <- myco_growth(ele, N_fungal_in, gorwth_C, growth_N)
    myco_growth_N_fungal[count] <- myco_growth(C_fungal_in, ele, gorwth_C, growth_N)
    count = count + 1
  }
  
  par(mfrow = c(3, 1))
  plot(range, myco_growth_C_fungal, main = "Growth", xlab = "C concentration", ylab = "Growth")
  plot(range, myco_growth_N_fungal, main = "Growth", xlab = "N concentration", ylab = "Growth")
  plot(range, myco_growth_C_and_N_fungal, main = "Growth", xlab = "C and N concentration", ylab = "Growth")
}

mycofon_balence_graphs <- function() {
  ####
  # Initalisation
  ####
  
  mycofon_balence_C_roots_roots <- mycofon_balence_C_roots_N_roots <- mycofon_balence_C_roots_fungal <- mycofon_balence_C_roots_N_fungal <- NULL
  mycofon_balence_C_fungal_roots <- mycofon_balence_C_fungal_N_roots <- mycofon_balence_C_fungal_fungal <- mycofon_balence_C_fungal_N_fungal <- NULL
  mycofon_balence_N_roots_roots <- mycofon_balence_N_roots_N_roots <- mycofon_balence_N_roots_fungal <- mycofon_balence_N_roots_N_fungal <- NULL
  mycofon_balence_N_fungal_roots <- mycofon_balence_N_fungal_N_roots <- mycofon_balence_N_fungal_fungal <- mycofon_balence_N_fungal_N_fungal <- NULL
  mycofon_balence_optimal_root_fungal_biomass_ratio_roots <- mycofon_balence_optimal_root_fungal_biomass_ratio_N_roots <- 
    mycofon_balence_optimal_root_fungal_biomass_ratio_fungal <- mycofon_balence_optimal_root_fungal_biomass_ratio_N_fungal <- NULL
  mycofon_balence_NH4_roots <- mycofon_balence_NH4_N_roots <- mycofon_balence_NH4_fungal <- mycofon_balence_NH4_N_fungal <- NULL
  mycofon_balence_NO3_roots <- mycofon_balence_NO3_N_roots <- mycofon_balence_NO3_fungal <- mycofon_balence_NO3_N_fungal <- NULL
  mycofon_balence_FOM_Norg_roots <- mycofon_balence_FOM_Norg_N_roots <- mycofon_balence_FOM_Norg_fungal <- mycofon_balence_FOM_Norg_N_fungal <- NULL
  mycofon_balence_T_roots <- mycofon_balence_Tsb_roots <- mycofon_balence_T_N_fungal <- mycofon_balence_Tsb_N_fungal <- NULL
  mycofon_balence_SWC_roots <- mycofon_balence_SWC_N_roots <- mycofon_balence_SWC_fungal <- mycofon_balence_SWC_N_fungal <- NULL
  mycofon_balence_T_fungal <- mycofon_balence_Tsb_fungal <- mycofon_balence_T_N_roots <- mycofon_balence_Tsb_N_roots <- NULL
  mycofon_balence_mm_roots <- mycofon_balence_mm_N_roots <- mycofon_balence_mm_fungal <- mycofon_balence_mm_N_fungal <- NULL
  mycofon_balence_ERM_roots <- mycofon_balence_ERM_N_roots <- mycofon_balence_ERM_fungal <- mycofon_balence_ERM_N_fungal <- NULL
  mycofon_balence_NC_in_root_opt_roots <- mycofon_balence_NC_in_root_opt_N_roots <- mycofon_balence_NC_in_root_opt_fungal <- mycofon_balence_NC_in_root_opt_N_fungal <- NULL
  mycofon_balence_NC_in_fungai_opt_roots <- mycofon_balence_NC_in_fungai_opt_N_roots <- mycofon_balence_NC_in_fungai_opt_fungal <- mycofon_balence_NC_in_fungai_opt_N_fungal <- NULL
  mycofon_balence_turnover_roots_roots <- mycofon_balence_turnover_roots_N_roots <- mycofon_balence_turnover_roots_fungal <- mycofon_balence_turnover_roots_N_fungal <- NULL
  mycofon_balence_turnover_mycorrhized_roots <- mycofon_balence_turnover_mycorrhized_N_roots <- mycofon_balence_turnover_mycorrhized_fungal <- mycofon_balence_turnover_mycorrhized_N_fungal <- NULL
  mycofon_balence_turnover_fungal_roots <- mycofon_balence_turnover_fungal_N_roots <- mycofon_balence_turnover_fungal_fungal <- mycofon_balence_turnover_fungal_N_fungal <- NULL
  mycofon_balence_carbon_use <- mycofon_balence_nitrogen_use <- NULL
  mycofon_balence_C_roots_value_roots <- mycofon_balence_C_roots_value_N_roots <- mycofon_balence_C_roots_value_fungal <- mycofon_balence_C_roots_value_N_fungal <- NULL
  mycofon_balence_N_roots_value_roots <- mycofon_balence_N_roots_value_N_roots <- mycofon_balence_N_roots_value_fungal <- mycofon_balence_N_roots_value_N_fungal <- NULL
  mycofon_balence_C_fungal_value_roots <- mycofon_balence_C_fungal_value_N_roots <- mycofon_balence_C_fungal_value_fungal <- mycofon_balence_C_fungal_value_N_fungal <- NULL
  mycofon_balence_N_fungal_value_roots <- mycofon_balence_N_fungal_value_N_roots <- mycofon_balence_N_fungal_value_fungal <- mycofon_balence_N_fungal_value_N_fungal <- NULL
  
  ####
  # vector generation
  ####
  
  temp_range = -20:20
  count = 1
  for (tem in temp_range) {
    mycofon_balence_T_roots[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, tem, 5, 0.8, 
                                                      c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                      c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                      1, 1, 4, 4, 4, 4)$C_roots
    mycofon_balence_Tsb_roots[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, tem, 0.8, 
                                                        c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                        c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                        1, 1, 4, 4, 4, 4)$C_roots
    mycofon_balence_T_N_roots[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, tem, 5, 0.8, 
                                                        c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                        c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                        1, 1, 4, 4, 4, 4)$N_roots
    mycofon_balence_Tsb_N_roots[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, tem, 0.8, 
                                                          c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                          c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                          1, 1, 4, 4, 4, 4)$N_roots
    
    mycofon_balence_T_fungal[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, tem, 5, 0.8, 
                                                      c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                      c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                      1, 1, 4, 4, 4, 4)$C_fungal
    mycofon_balence_Tsb_fungal[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, tem, 0.8, 
                                                        c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                        c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                        1, 1, 4, 4, 4, 4)$C_fungal
    mycofon_balence_T_N_fungal[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, tem, 5, 0.8, 
                                                        c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                        c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                        1, 1, 4, 4, 4, 4)$N_fungal
    mycofon_balence_Tsb_N_fungal[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, tem, 0.8, 
                                                          c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                          c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                          1, 1, 4, 4, 4, 4)$N_fungal
    count = count + 1
  }
  concentration_range = 0:1500
  count = 1
  for (conc in concentration_range) {
    mycofon_balence_C_roots_roots[count] <- mycofon_balence(conc, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                            c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                            c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                            1, 1, 4, 4, 4, 4)$C_roots
    mycofon_balence_C_roots_N_roots[count] <- mycofon_balence(conc, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                              c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                              c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                              1, 1, 4, 4, 4, 4)$N_roots
    mycofon_balence_C_roots_fungal[count] <- mycofon_balence(conc, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                            c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                            c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                            1, 1, 4, 4, 4, 4)$C_fungal
    mycofon_balence_C_roots_N_fungal[count] <- mycofon_balence(conc, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                                c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                                c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                                1, 1, 4, 4, 4, 4)$N_fungal
    
    mycofon_balence_C_fungal_roots[count] <- mycofon_balence(200, 150, 0.4, 0.5, conc, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                             c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                             c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                             1, 1, 4, 4, 4, 4)$C_roots
    mycofon_balence_C_fungal_N_roots[count] <- mycofon_balence(200, 150, 0.4, 0.5, conc, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                               c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                               c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                               1, 1, 4, 4, 4, 4)$N_roots
    mycofon_balence_C_fungal_fungal[count] <- mycofon_balence(200, 150, 0.4, 0.5, conc, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                               c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                               c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                               1, 1, 4, 4, 4, 4)$C_fungal
    mycofon_balence_C_fungal_N_fungal[count] <- mycofon_balence(200, 150, 0.4, 0.5, conc, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                                 c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                                 c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                                 1, 1, 4, 4, 4, 4)$N_fungal
    
    mycofon_balence_N_roots_roots[count] <- mycofon_balence(200, conc, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                           c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                           c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                           1, 1, 4, 4, 4, 4)$C_roots
    mycofon_balence_N_roots_N_roots[count] <- mycofon_balence(200, conc, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                              c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                              c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                              1, 1, 4, 4, 4, 4)$N_roots
    mycofon_balence_N_roots_fungal[count] <- mycofon_balence(200, conc, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                              c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                              c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                              1, 1, 4, 4, 4, 4)$C_fungal
    mycofon_balence_N_roots_N_fungal[count] <- mycofon_balence(200, conc, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                                c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                                c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                                1, 1, 4, 4, 4, 4)$N_fungal
    
    mycofon_balence_N_fungal_roots[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, conc, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                       c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                       c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                       1, 1, 4, 4, 4, 4)$C_roots
    mycofon_balence_N_fungal_N_roots[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, conc, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                             c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                             c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                             1, 1, 4, 4, 4, 4)$N_roots
    mycofon_balence_N_fungal_fungal[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, conc, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                             c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                             c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                             1, 1, 4, 4, 4, 4)$C_fungal
    mycofon_balence_N_fungal_N_fungal[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, conc, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                             c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                             c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                             1, 1, 4, 4, 4, 4)$N_fungal
    
    mycofon_balence_NH4_roots[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), conc, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                        c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                        c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                        1, 1, 4, 4, 4, 4)$C_roots
    mycofon_balence_NH4_N_roots[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), conc, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                                c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                                c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                                1, 1, 4, 4, 4, 4)$N_roots
    mycofon_balence_NH4_fungal[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), conc, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                              c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                              c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                              1, 1, 4, 4, 4, 4)$C_fungal
    mycofon_balence_NH4_N_fungal[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), conc, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                                c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                                c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                                1, 1, 4, 4, 4, 4)$N_fungal
    
    mycofon_balence_NO3_roots[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, conc, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                        c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                        c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                        1, 1, 4, 4, 4, 4)$C_roots
    mycofon_balence_NO3_N_roots[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, conc, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                          c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                          c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                          1, 1, 4, 4, 4, 4)$N_roots
    mycofon_balence_NO3_fungal[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, conc, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                        c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                        c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                        1, 1, 4, 4, 4, 4)$C_fungal
    mycofon_balence_NO3_N_fungal[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, conc, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                          c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                          c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                          1, 1, 4, 4, 4, 4)$N_fungal
    
    mycofon_balence_FOM_Norg_roots[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, conc, 0.5, 0.5, 15, 5, 0.8, 
                                                             c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                             c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                             1, 1, 4, 4, 4, 4)$C_roots
    mycofon_balence_FOM_Norg_N_roots[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, conc, 0.5, 0.5, 15, 5, 0.8, 
                                                               c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                               c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                               1, 1, 4, 4, 4, 4)$N_roots
    mycofon_balence_FOM_Norg_fungal[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, conc, 0.5, 0.5, 15, 5, 0.8, 
                                                             c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                             c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                             1, 1, 4, 4, 4, 4)$C_fungal
    mycofon_balence_FOM_Norg_N_fungal[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, conc, 0.5, 0.5, 15, 5, 0.8, 
                                                               c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                               c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                               1, 1, 4, 4, 4, 4)$N_fungal
    
    mycofon_balence_mm_roots[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                      c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                      c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), conc, 180, c(10, -0.2),
                                                      1, 1, 4, 4, 4, 4)$C_roots
    mycofon_balence_mm_N_roots[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                         c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                         c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), conc, 180, c(10, -0.2),
                                                         1, 1, 4, 4, 4, 4)$N_roots
    mycofon_balence_mm_fungal[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                       c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                       c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), conc, 180, c(10, -0.2),
                                                       1, 1, 4, 4, 4, 4)$C_fungal
    mycofon_balence_mm_N_fungal[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                         c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                         c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), conc, 180, c(10, -0.2),
                                                         1, 1, 4, 4, 4, 4)$N_fungal
    
    mycofon_balence_ERM_roots[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                       c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                       c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, conc, c(10, -0.2),
                                                       1, 1, 4, 4, 4, 4)$C_roots
    mycofon_balence_ERM_N_roots[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                          c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                          c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, conc, c(10, -0.2),
                                                          1, 1, 4, 4, 4, 4)$N_roots
    mycofon_balence_ERM_fungal[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                        c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                        c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, conc, c(10, -0.2),
                                                        1, 1, 4, 4, 4, 4)$C_fungal
    mycofon_balence_ERM_N_fungal[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                          c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                          c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, conc, c(10, -0.2),
                                                          1, 1, 4, 4, 4, 4)$N_fungal
    count = count + 1
  }
  SWC_range = seq(0.0, 1, by = 0.01)
  count = 1
  for (SWC in SWC_range) {
    mycofon_balence_SWC_roots[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, SWC, 
                                                        c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                        c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                        1, 1, 4, 4, 4, 4)$C_roots
    mycofon_balence_SWC_N_roots[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, SWC, 
                                                          c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                          c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                          1, 1, 4, 4, 4, 4)$N_roots
    mycofon_balence_SWC_fungal[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, SWC, 
                                                        c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                        c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                        1, 1, 4, 4, 4, 4)$C_fungal
    mycofon_balence_SWC_N_fungal[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, SWC, 
                                                          c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                          c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                          1, 1, 4, 4, 4, 4)$N_fungal
    
    mycofon_balence_optimal_root_fungal_biomass_ratio_roots[count] <- mycofon_balence(200, 150, 0.4, SWC, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                                                     c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                                                     c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                                                     1, 1, 4, 4, 4, 4)$C_roots
    mycofon_balence_optimal_root_fungal_biomass_ratio_N_roots[count] <- mycofon_balence(200, 150, 0.4, SWC, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                                                        c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                                                        c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                                                        1, 1, 4, 4, 4, 4)$N_roots
    mycofon_balence_optimal_root_fungal_biomass_ratio_fungal[count] <- mycofon_balence(200, 150, 0.4, SWC, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                                                      c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                                                      c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                                                      1, 1, 4, 4, 4, 4)$C_fungal
    mycofon_balence_optimal_root_fungal_biomass_ratio_N_fungal[count] <- mycofon_balence(200, 150, 0.4, SWC, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                                                        c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                                                        c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                                                        1, 1, 4, 4, 4, 4)$N_fungal
    
    mycofon_balence_turnover_roots_roots[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, SWC, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                                  c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                                  c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                                  1, 1, 4, 4, 4, 4)$C_roots
    mycofon_balence_turnover_roots_N_roots[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, SWC, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                                     c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                                     c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                                     1, 1, 4, 4, 4, 4)$N_roots
    mycofon_balence_turnover_roots_fungal[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, SWC, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                                   c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                                   c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                                   1, 1, 4, 4, 4, 4)$C_fungal
    mycofon_balence_turnover_roots_N_fungal[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, SWC, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                                     c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                                     c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                                     1, 1, 4, 4, 4, 4)$N_fungal
    
    mycofon_balence_turnover_mycorrhized_roots[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, SWC, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                                              c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                                              c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                                              1, 1, 4, 4, 4, 4)$C_roots
    mycofon_balence_turnover_mycorrhized_N_roots[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, SWC, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                                                 c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                                                 c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                                                 1, 1, 4, 4, 4, 4)$N_roots
    mycofon_balence_turnover_mycorrhized_fungal[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, SWC, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                                               c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                                               c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                                               1, 1, 4, 4, 4, 4)$C_fungal
    mycofon_balence_turnover_mycorrhized_N_fungal[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, SWC, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                                                 c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                                                 c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                                                 1, 1, 4, 4, 4, 4)$N_fungal
    
    mycofon_balence_turnover_fungal_roots[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, SWC, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                                   c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                                   c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                                   1, 1, 4, 4, 4, 4)$C_roots
    mycofon_balence_turnover_fungal_N_roots[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, SWC, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                                      c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                                      c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                                      1, 1, 4, 4, 4, 4)$N_roots
    mycofon_balence_turnover_fungal_fungal[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, SWC, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                                    c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                                    c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                                    1, 1, 4, 4, 4, 4)$C_fungal
    mycofon_balence_turnover_fungal_N_fungal[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, SWC, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                                        c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                                        c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                                        1, 1, 4, 4, 4, 4)$N_fungal
    
    mycofon_balence_NC_in_root_opt_roots[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, SWC, 0.5, 15, 5, 0.8, 
                                                              c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                              c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                              1, 1, 4, 4, 4, 4)$C_roots
    mycofon_balence_NC_in_root_opt_N_roots[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, SWC, 0.5, 15, 5, 0.8, 
                                                             c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                             c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                             1, 1, 4, 4, 4, 4)$N_roots
    mycofon_balence_NC_in_root_opt_fungal[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, SWC, 0.5, 15, 5, 0.8, 
                                                             c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                             c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                             1, 1, 4, 4, 4, 4)$C_fungal
    mycofon_balence_NC_in_root_opt_N_fungal[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, SWC, 0.5, 15, 5, 0.8, 
                                                             c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                             c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                             1, 1, 4, 4, 4, 4)$N_fungal
    
    mycofon_balence_NC_in_fungai_opt_roots[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, SWC, 15, 5, 0.8, 
                                                              c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                              c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                              1, 1, 4, 4, 4, 4)$C_roots
    mycofon_balence_NC_in_fungai_opt_N_roots[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, SWC, 15, 5, 0.8, 
                                                               c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                               c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                               1, 1, 4, 4, 4, 4)$N_roots
    mycofon_balence_NC_in_fungai_opt_fungal[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, SWC, 15, 5, 0.8, 
                                                               c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                               c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                               1, 1, 4, 4, 4, 4)$C_roots
    mycofon_balence_NC_in_fungai_opt_N_fungal[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, SWC, 15, 5, 0.8, 
                                                               c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                               c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                               1, 1, 4, 4, 4, 4)$N_fungal
    
    
    count = count + 1
  }
  value = 0:10
  count = 1
  for (val in value) {
    mycofon_balence_C_roots_value_roots[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                                  c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                                  c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                                  1, 1, 4, 4, val, 4)$C_roots
    mycofon_balence_C_roots_value_N_roots[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                                    c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                                    c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                                    1, 1, 4, 4, val, 4)$N_roots
    mycofon_balence_C_roots_value_fungal[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                                  c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                                  c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                                  1, 1, 4, 4, val, 4)$C_fungal
    mycofon_balence_C_roots_value_N_fungal[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                                    c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                                    c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                                    1, 1, 4, 4, val, 4)$N_fungal
    
    mycofon_balence_N_roots_value_roots[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                                  c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                                  c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                                  1, 1, 4, 4, 4, val)$C_roots
    mycofon_balence_N_roots_value_N_roots[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                                    c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                                    c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                                    1, 1, 4, 4, 4, val)$N_roots
    mycofon_balence_N_roots_value_fungal[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                                  c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                                  c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                                  1, 1, 4, 4, 4, val)$C_fungal
    mycofon_balence_N_roots_value_N_fungal[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                                    c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                                    c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                                    1, 1, 4, 4, 4, val)$N_fungal
    
    mycofon_balence_C_fungal_value_roots[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                                  c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                                  c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                                  1, 1, val, 4, 4, 4)$C_roots
    mycofon_balence_C_fungal_value_N_roots[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                                     c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                                     c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                                     1, 1, val, 4, 4, 4)$N_roots
    mycofon_balence_C_fungal_value_fungal[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                                   c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                                   c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                                   1, 1, val, 4, 4, 4)$C_fungal
    mycofon_balence_C_fungal_value_N_fungal[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                                     c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                                     c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                                     1, 1, val, 4, 4, 4)$N_fungal
    
    mycofon_balence_N_fungal_value_roots[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                                  c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                                  c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                                  1, 1, 4, val, 4, 4)$C_roots
    mycofon_balence_N_fungal_value_N_roots[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                                     c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                                     c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                                     1, 1, 4, val, 4, 4)$N_roots
    mycofon_balence_N_fungal_value_fungal[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                                   c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                                   c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                                   1, 1, 4, val, 4, 4)$C_fungal
    mycofon_balence_N_fungal_value_N_fungal[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                                     c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                                     c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                                     1, 1, 4, val, 4, 4)$N_fungal
    
    mycofon_balence_carbon_use[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                         c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                         c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                         val, 1, 4, 4, 4, 4)$C_roots
    mycofon_balence_nitrogen_use[count] <- mycofon_balence(200, 150, 0.4, 0.5, 200, 150, 0.1, 0.1, 0.1, c(0.2, 0.2), 15, 15, 15, 0.5, 0.5, 15, 5, 0.8, 
                                                           c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 
                                                           c(10, 10, 10), c(0.2, 0.2, 0.2), c(0.5, 0.5, 0.5), 20, 180, c(10, -0.2),
                                                           1, val, 4, 4, 4, 4)$C_roots
    count = count + 1
  }
  
  ####
  # Plots!
  ####
  
  par(mfrow = c(3, 3))
  axis_1 = c(min(mycofon_balence_T_roots, mycofon_balence_T_N_roots, mycofon_balence_T_fungal, mycofon_balence_T_N_fungal),
             max(mycofon_balence_T_roots, mycofon_balence_T_N_roots, mycofon_balence_T_fungal, mycofon_balence_T_N_fungal))
  plot(temp_range, mycofon_balence_T_roots, main = "kg C", xlab = "Temperature Range: 'C", ylab = "C kg eq", ylim = axis_1)
  points(temp_range, mycofon_balence_T_N_roots, col = "blue")
  points(temp_range, mycofon_balence_T_fungal, pch = "+")
  points(temp_range, mycofon_balence_T_N_fungal, pch = "+", col = "blue")
  legend(temp_range[1], axis_1[2], c("Roots C", "Fungal C", "Roots N", "Fungal N"), col = c("black", "black", "blue", "blue"),  pch = c(1, "x", 1, "x"), bty = "n")
  
  axis_2 = c(min(mycofon_balence_Tsb_roots, mycofon_balence_Tsb_N_roots, mycofon_balence_Tsb_fungal, mycofon_balence_Tsb_N_fungal),
             max(mycofon_balence_Tsb_roots, mycofon_balence_Tsb_N_roots, mycofon_balence_Tsb_fungal, mycofon_balence_Tsb_N_fungal))
  plot(temp_range, mycofon_balence_Tsb_roots, main = "kg C eq", xlab = "Temperature Range: 'C", ylab = "C kg eq", ylim = axis_2)
  points(temp_range, mycofon_balence_Tsb_N_roots, col = "blue")
  points(temp_range, mycofon_balence_Tsb_fungal, pch = "+")
  points(temp_range, mycofon_balence_Tsb_N_fungal, pch = "+", col = "blue")
  
  axis_3 = c(min(mycofon_balence_SWC_roots, mycofon_balence_SWC_N_roots, mycofon_balence_SWC_fungal, mycofon_balence_SWC_N_fungal),
             max(mycofon_balence_SWC_roots, mycofon_balence_SWC_N_roots, mycofon_balence_SWC_fungal, mycofon_balence_SWC_N_fungal))
  plot(SWC_range, mycofon_balence_SWC_roots, main = "kg C eq", xlab = "Soil water concentration range: %", ylab = "C kg", ylim = axis_3)
  points(SWC_range, mycofon_balence_SWC_N_roots, col = "blue")
  points(SWC_range, mycofon_balence_SWC_fungal, pch = "+")
  points(SWC_range, mycofon_balence_SWC_N_fungal, pch = "+", col = "blue")
  
  axis_4 = c(min(mycofon_balence_NH4_roots, mycofon_balence_NH4_N_roots, mycofon_balence_NH4_fungal, mycofon_balence_NH4_N_fungal),
             max(mycofon_balence_NH4_roots, mycofon_balence_NH4_N_roots, mycofon_balence_NH4_fungal, mycofon_balence_NH4_N_fungal))
  plot(concentration_range, mycofon_balence_NH4_roots, main = "kg C", xlab = "Concentration range: NH4", ylab = "C kg", ylim = axis_4)
  points(concentration_range, mycofon_balence_NH4_N_roots, col = "blue")
  points(concentration_range, mycofon_balence_NH4_fungal, pch = "+")
  points(concentration_range, mycofon_balence_NH4_N_fungal, pch = "+", col = "blue")
  
  axis_5 = c(min(mycofon_balence_NO3_roots, mycofon_balence_NO3_N_roots, mycofon_balence_NO3_fungal, mycofon_balence_NO3_N_fungal),
             max(mycofon_balence_NO3_roots, mycofon_balence_NO3_N_roots, mycofon_balence_NO3_fungal, mycofon_balence_NO3_N_fungal))
  plot(concentration_range, mycofon_balence_NO3_roots, main = "kg C", xlab = "Concentration range: NO3", ylab = "C kg", ylim = axis_5)
  points(concentration_range, mycofon_balence_NO3_N_roots, col = "blue")
  points(concentration_range, mycofon_balence_NO3_fungal, pch = "+")
  points(concentration_range, mycofon_balence_NO3_N_fungal, pch = "+", col = "blue")
  
  axis_6 = c(min(mycofon_balence_FOM_Norg_roots, mycofon_balence_FOM_Norg_N_roots, mycofon_balence_FOM_Norg_fungal, mycofon_balence_FOM_Norg_N_fungal),
             max(mycofon_balence_FOM_Norg_roots, mycofon_balence_FOM_Norg_N_roots, mycofon_balence_FOM_Norg_fungal, mycofon_balence_FOM_Norg_N_fungal))
  plot(concentration_range, mycofon_balence_FOM_Norg, main = "kg C", xlab = "Concentration range: FOM_Norg", ylab = "C kg", ylim = axis_6)
  points(concentration_range, mycofon_balence_FOM_Norg_N_roots, col = "blue")
  points(concentration_range, mycofon_balence_FOM_Norg_fungal, pch = "+")
  points(concentration_range, mycofon_balence_FOM_Norg_N_fungal, pch = "+", col = "blue")
  
  axis_7 = c(min(mycofon_balence_turnover_roots_roots, mycofon_balence_turnover_roots_N_roots, mycofon_balence_turnover_roots_fungal, mycofon_balence_turnover_roots_N_fungal),
             max(mycofon_balence_turnover_roots_roots, mycofon_balence_turnover_roots_N_roots, mycofon_balence_turnover_roots_fungal, mycofon_balence_turnover_roots_N_fungal))
  plot(SWC_range, mycofon_balence_turnover_roots, main = "kg C", xlab = "Turnover rate: %", ylab = "C kg", ylim = axis_7)
  points(SWC_range, mycofon_balence_turnover_roots_N_roots, col = "blue")
  points(SWC_range, mycofon_balence_turnover_roots_fungal, pch = "+")
  points(SWC_range, mycofon_balence_turnover_roots_N_fungal, pch = "+", col = "blue")
  
  axis_8 = c(min(mycofon_balence_turnover_mycorrhized_roots, mycofon_balence_turnover_mycorrhized_N_roots, mycofon_balence_turnover_mycorrhized_fungal, mycofon_balence_turnover_mycorrhized_N_fungal),
             max(mycofon_balence_turnover_mycorrhized_roots, mycofon_balence_turnover_mycorrhized_N_roots, mycofon_balence_turnover_mycorrhized_fungal, mycofon_balence_turnover_mycorrhized_N_fungal))
  plot(SWC_range, mycofon_balence_turnover_mycorrhized_roots, main = "kg C", xlab = "root turnover rate: %", ylab = "C kg", ylim = axis_8)
  points(SWC_range, mycofon_balence_turnover_mycorrhized_N_roots, col = "blue")
  points(SWC_range, mycofon_balence_turnover_mycorrhized_fungal, pch = "+")
  points(SWC_range, mycofon_balence_turnover_mycorrhized_N_fungal, pch = "+", col = "blue")
  
  axis_9 = c(min(mycofon_balence_turnover_fungal_roots, mycofon_balence_turnover_fungal_N_roots, mycofon_balence_turnover_fungal_fungal, mycofon_balence_turnover_fungal_N_fungal),
             max(mycofon_balence_turnover_fungal_roots, mycofon_balence_turnover_fungal_N_roots, mycofon_balence_turnover_fungal_fungal, mycofon_balence_turnover_fungal_N_fungal))
  plot(SWC_range, mycofon_balence_turnover_fungal_roots, main = "kg C", xlab = "root turnover rate: %", ylab = "C kg", ylim = axis_9)
  points(SWC_range, mycofon_balence_turnover_fungal_N_roots, col = "blue")
  points(SWC_range, mycofon_balence_turnover_fungal_fungal, pch = "+")
  points(SWC_range, mycofon_balence_turnover_fungal_N_fungal, pch = "+", col = "blue")
  
  par(mfrow = c(4, 2))
  axis_10 = c(min(mycofon_balence_C_roots_roots, mycofon_balence_C_roots_N_roots, mycofon_balence_C_roots_fungal, mycofon_balence_C_roots_N_fungal, na.rm = T),
              max(mycofon_balence_C_roots_roots, mycofon_balence_C_roots_N_roots, mycofon_balence_C_roots_fungal, mycofon_balence_C_roots_N_fungal, na.rm = T))
  plot(concentration_range, mycofon_balence_C_roots_roots, main = "kg C", xlab = "Concentration range: C", ylab = "C kg", ylim = axis_10)
  points(concentration_range, mycofon_balence_C_roots_N_roots, col = "blue")
  points(concentration_range, mycofon_balence_C_roots_fungal, pch = "+")
  points(concentration_range, mycofon_balence_C_roots_N_fungal, pch = "+", col = "blue")
  legend(temp_range[1], axis_10[2], c("Roots C", "Fungal C", "Roots N", "Fungal N"), col = c("black", "black", "blue", "blue"),  pch = c(1, "x", 1, "x"), bty = "n")
  
  axis_11 = c(min(mycofon_balence_N_roots_roots, mycofon_balence_N_roots_N_roots, mycofon_balence_N_roots_fungal, mycofon_balence_N_roots_N_fungal, na.rm = T),
              max(mycofon_balence_N_roots_roots, mycofon_balence_N_roots_N_roots, mycofon_balence_N_roots_fungal, mycofon_balence_N_roots_N_fungal, na.rm = T))
  plot(concentration_range, mycofon_balence_N_roots_roots, main = "kg C", xlab = "Concentration range: N", ylab = "C kg", ylim = axis_11)
  points(concentration_range, mycofon_balence_N_roots_N_roots, col = "blue")
  points(concentration_range, mycofon_balence_N_roots_fungal, pch = "+")
  points(concentration_range, mycofon_balence_N_roots_N_fungal, pch = "+", col = "blue")
  
  axis_12 = c(min(mycofon_balence_C_fungal_roots, mycofon_balence_C_fungal_N_roots, mycofon_balence_C_fungal_fungal, mycofon_balence_C_fungal_N_fungal, na.rm = T),
              max(mycofon_balence_C_fungal_roots, mycofon_balence_C_fungal_N_roots, mycofon_balence_C_fungal_fungal, mycofon_balence_C_fungal_N_fungal, na.rm = T))
  plot(concentration_range, mycofon_balence_C_fungal_roots, main = "kg C", xlab = "Concentration range: C", ylab = "C kg", ylim = axis_12)
  points(concentration_range, mycofon_balence_C_fungal_N_roots, col = "blue")
  points(concentration_range, mycofon_balence_C_fungal_fungal, pch = "+")
  points(concentration_range, mycofon_balence_C_fungal_N_fungal, pch = "+", col = "blue")
  
  axis_13 = c(min(mycofon_balence_N_fungal_roots, mycofon_balence_N_fungal_N_roots, mycofon_balence_N_fungal_fungal, mycofon_balence_N_fungal_N_fungal, na.rm = T),
              max(mycofon_balence_N_fungal_roots, mycofon_balence_N_fungal_N_roots, mycofon_balence_N_fungal_fungal, mycofon_balence_N_fungal_N_fungal, na.rm = T))
  plot(concentration_range, mycofon_balence_N_fungal_roots, main = "kg C", xlab = "Concentration range: N", ylab = "C kg", ylim = axis_13)
  points(concentration_range, mycofon_balence_N_fungal_N_roots, col = "blue")
  points(concentration_range, mycofon_balence_N_fungal_fungal, pch = "+")
  points(concentration_range, mycofon_balence_N_fungal_N_fungal, pch = "+", col = "blue")
  
  axis_14 = c(min(mycofon_balence_C_roots_value_roots, mycofon_balence_C_roots_value_N_roots, mycofon_balence_C_roots_value_fungal, mycofon_balence_C_roots_value_N_fungal),
              max(mycofon_balence_C_roots_value_roots, mycofon_balence_C_roots_value_N_roots, mycofon_balence_C_roots_value_fungal, mycofon_balence_C_roots_value_N_fungal))
  plot(value, mycofon_balence_C_roots_value_roots, main = "kg C eq", xlab = "Carbon value range", ylab = "C kg eq", ylim = axis_14)
  points(value, mycofon_balence_C_roots_value_N_roots, col = "blue")
  points(value, mycofon_balence_C_roots_value_fungal, pch = "+")
  points(value, mycofon_balence_C_roots_value_N_fungal, pch = "+", col = "blue")
  
  axis_15 = c(min(mycofon_balence_N_roots_value_roots, mycofon_balence_N_roots_value_N_roots, mycofon_balence_N_roots_value_fungal, mycofon_balence_N_roots_value_N_fungal),
              max(mycofon_balence_N_roots_value_roots, mycofon_balence_N_roots_value_N_roots, mycofon_balence_N_roots_value_fungal, mycofon_balence_N_roots_value_N_fungal))
  plot(value, mycofon_balence_N_roots_value, main = "kg C", xlab = "Nitrogen value range", ylab = "C kg", ylim = axis_15)
  points(value, mycofon_balence_N_roots_value_N_roots, col = "blue")
  points(value, mycofon_balence_N_roots_value_fungal, pch = "+")
  points(value, mycofon_balence_N_roots_value_N_fungal, pch = "+", col = "blue")
  
  axis_16 = c(min(mycofon_balence_C_fungal_value_roots, mycofon_balence_C_fungal_value_N_roots, mycofon_balence_C_fungal_value_fungal, mycofon_balence_C_fungal_value_N_fungal),
              max(mycofon_balence_C_fungal_value_roots, mycofon_balence_C_fungal_value_N_roots, mycofon_balence_C_fungal_value_fungal, mycofon_balence_C_fungal_value_N_fungal))
  plot(value, mycofon_balence_C_fungal_value, main = "kg C", xlab = "Carbon value range", ylab = "C kg", ylim = axis_16)
  points(value, mycofon_balence_C_fungal_value_N_roots, col = "blue")
  points(value, mycofon_balence_C_fungal_value_fungal, pch = "+")
  points(value, mycofon_balence_C_fungal_value_N_fungal, pch = "+", col = "blue")
  
  axis_17 = c(min(mycofon_balence_N_fungal_value_roots, mycofon_balence_N_fungal_value_N_roots, mycofon_balence_N_fungal_value_fungal, mycofon_balence_N_fungal_value_N_fungal),
              max(mycofon_balence_N_fungal_value_roots, mycofon_balence_N_fungal_value_N_roots, mycofon_balence_N_fungal_value_fungal, mycofon_balence_N_fungal_value_N_fungal))
  plot(value, mycofon_balence_N_fungal_value, main = "kg C", xlab = "Nitrogen value range", ylab = "C kg", ylim = axis_17)
  points(value, mycofon_balence_N_fungal_value_N_roots, col = "blue")
  points(value, mycofon_balence_N_fungal_value_fungal, pch = "+")
  points(value, mycofon_balence_N_fungal_value_N_fungal, pch = "+", col = "blue")
  
  par(mfrow = c(4, 2))
  axis_18 = c(min(mycofon_balence_NC_in_root_opt_roots, mycofon_balence_NC_in_root_opt_N_roots, mycofon_balence_NC_in_root_opt_fungal, mycofon_balence_NC_in_root_opt_N_fungal, na.rm = T),
              max(mycofon_balence_NC_in_root_opt_roots, mycofon_balence_NC_in_root_opt_N_roots, mycofon_balence_NC_in_root_opt_fungal, mycofon_balence_NC_in_root_opt_N_fungal, na.rm = T))
  plot(SWC_range, mycofon_balence_NC_in_root_opt_roots, main = "kg C", xlab = "Optimum NC ratio in roots: %", ylab = "C kg", ylim = axis_18)
  points(SWC_range, mycofon_balence_NC_in_root_opt_N_roots, col = "blue")
  points(SWC_range, mycofon_balence_NC_in_root_opt_fungal, pch = "+")
  points(SWC_range, mycofon_balence_NC_in_root_opt_N_fungal, pch = "+", col = "blue")
  legend(SWC_range[1], axis_18[2], c("Roots C", "Fungal C", "Roots N", "Fungal N"), col = c("black", "black", "blue", "blue"),  pch = c(1, "x", 1, "x"), bty = "n")
  
  axis_19 = c(min(mycofon_balence_NC_in_root_opt_roots, mycofon_balence_NC_in_root_opt_N_roots, mycofon_balence_NC_in_root_opt_fungal, mycofon_balence_NC_in_root_opt_N_fungal, na.rm = T),
              max(mycofon_balence_NC_in_root_opt_roots, mycofon_balence_NC_in_root_opt_N_roots, mycofon_balence_NC_in_root_opt_fungal, mycofon_balence_NC_in_root_opt_N_fungal, na.rm = T))
  plot(SWC_range, mycofon_balence_NC_in_fungai_opt, main = "kg C", xlab = "Optimum NC ratio in fungal: %", ylab = "C kg", ylim = axis_19)
  points(SWC_range, mycofon_balence_NC_in_fungai_opt_N_roots, col = "blue")
  points(SWC_range, mycofon_balence_NC_in_fungai_opt_fungal, pch = "+")
  points(SWC_range, mycofon_balence_NC_in_fungai_opt_N_fungal, pch = "+", col = "blue")
  
  axis_20 = c(min(mycofon_balence_mm_roots, mycofon_balence_mm_N_roots, mycofon_balence_mm_fungal, mycofon_balence_mm_N_fungal, na.rm = T),
              max(mycofon_balence_mm_roots, mycofon_balence_mm_N_roots, mycofon_balence_mm_fungal, mycofon_balence_mm_N_fungal, na.rm = T))
  plot(concentration_range, mycofon_balence_mm_roots, main = "kg C", xlab = "Mantle Mass: kg C", ylab = "C kg", ylim = axis_20)
  points(concentration_range, mycofon_balence_mm_N_roots, col = "blue")
  points(concentration_range, mycofon_balence_mm_fungal, pch = "+")
  points(concentration_range, mycofon_balence_mm_N_fungal, pch = "+", col = "blue")
  
  axis_21 = c(min(mycofon_balence_ERM_roots, mycofon_balence_ERM_N_roots, mycofon_balence_ERM_fungal, mycofon_balence_ERM_N_fungal, na.rm = T),
              max(mycofon_balence_ERM_roots, mycofon_balence_ERM_N_roots, mycofon_balence_ERM_fungal, mycofon_balence_ERM_N_fungal, na.rm = T))
  plot(concentration_range, mycofon_balence_ERM_roots, main = "kg C", xlab = "ERM Mass: kg C", ylab = "C kg", ylim = axis_21)
  points(concentration_range, mycofon_balence_ERM_N_roots, col = "blue")
  points(concentration_range, mycofon_balence_ERM_fungal, pch = "+")
  points(concentration_range, mycofon_balence_ERM_N_fungal, pch = "+", col = "blue")
  
  plot(value, mycofon_balence_carbon_use, main = "kg C", xlab = "Carbon Use", ylab = "C kg")
  plot(value, mycofon_balence_nitrogen_use, main = "kg C", xlan = "Nitrogen Use", ylab = "C kg")
  
  axis_24 = c(min(mycofon_balence_optimal_root_fungal_biomass_ratio_roots, mycofon_balence_optimal_root_fungal_biomass_ratio_N_roots, mycofon_balence_optimal_root_fungal_biomass_ratio_fungal, mycofon_balence_optimal_root_fungal_biomass_ratio_N_fungal, na.rm = T),
              max(mycofon_balence_optimal_root_fungal_biomass_ratio_roots, mycofon_balence_optimal_root_fungal_biomass_ratio_N_roots, mycofon_balence_optimal_root_fungal_biomass_ratio_fungal, mycofon_balence_optimal_root_fungal_biomass_ratio_N_fungal, na.rm = T))
  plot(SWC_range, mycofon_balence_optimal_root_fungal_biomass_ratio_roots, main = "kg C", xlan = "Nitrogen Use", ylab = "C kg", ylim = axis_24)
  points(SWC_range, mycofon_balence_optimal_root_fungal_biomass_ratio_N_roots, col = "blue")
  points(SWC_range, mycofon_balence_optimal_root_fungal_biomass_ratio_fungal, pch = "+")
  points(SWC_range, mycofon_balence_optimal_root_fungal_biomass_ratio_N_fungal, pch = "+", col = "blue")

}

# TODO: need graphs for
# symphony_multiple_FOM_daily
# Toy_Model

# TODO: tomorrow, try to fill the data table and also update the documentation
