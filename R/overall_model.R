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

nitrogen_graphs <- function() {
  
  # Indervidual functions should follow this pattern
  temp_change_organic <- temp_change_NO3 <- conc_change_organic <- conc_change_NO3 <- SWC_change_organic <- SWC_change_NO3 <- NULL
  temp_change_N_Uptake <- SWC_change_N_Uptake <- conc_change_N_Uptake <- conc_change_Norg_Uptake <- conc_change_NH4_Uptake <- conc_change_NO3_Uptake <- NULL
  temp_change_N_Uptake_Fungal <- SWC_change_N_Uptake_Fungal <- conc_change_N_Uptake_Fungal <- conc_change_Norg_Uptake_Fungal <- conc_change_NO3_Uptake_Fungal <- conc_change_NH4_Uptake_Fungal <- NULL
  temperature_range = -20:20
  count = 1
  for (Temp in temperature_range) {
    temp_change_organic[count] = uptake_organic_N(10, Temp, 10, 0.2, 0.8, 0.5)
    temp_change_NO3[count] = uptake_NO3(10, Temp, 10, -0.2, 0.8, 0.5)
    temp_change_N_Uptake[count] = Plant_N_Uptake(c(0.5, 0.5, 0.5), c(10, 10, 10), 0.2, 15, Temp, 0.8, c(10, 10, 10), c(0.2, -0.2, 0.2), c(0.5, 0.5, 0.5))
    temp_change_N_Uptake_Fungal[count] = Fungal_N_Uptake(200, 150, 200, 500, 3000, 3000, 3000, 10, Temp, 0.8,c(10, 10, 10), c(10, 10, 10), c(0.2, -0.2, 0.2), c(0.5, 0.5, 0.5))
    count = count + 1
  }
  concentration_range = seq(0, 20, by = 0.01)
  count = 1
  for (conc in concentration_range) {
    conc_change_organic[count] <- uptake_organic_N(conc, 15, 10, 0.2, 0.8, 0.5)
    conc_change_NO3[count] <- uptake_NO3(conc, 15, 10, -0.2, 0.8, 0.5)
    conc_change_N_Uptake[count] = Plant_N_Uptake(c(0.5, 0.5, 0.5), c(conc, conc, conc), 0.2, 15, 15, 0.8, c(10, 10, 10), c(0.2, -0.2, 0.2), c(0.5, 0.5, 0.5))
    conc_change_Norg_Uptake[count] = Plant_N_Uptake(c(0.5, 0.5, 0.5), c(conc, 10, 10), 0.2, 15, 15, 0.8, c(10, 10, 10), c(0.2, -0.2, 0.2), c(0.5, 0.5, 0.5))
    conc_change_NO3_Uptake[count] = Plant_N_Uptake(c(0.5, 0.5, 0.5), c(10, conc, 10), 0.2, 15, 15, 0.8, c(10, 10, 10), c(0.2, -0.2, 0.2), c(0.5, 0.5, 0.5))
    conc_change_NH4_Uptake[count] = Plant_N_Uptake(c(0.5, 0.5, 0.5), c(10, 10, conc), 0.2, 15, 15, 0.8, c(10, 10, 10), c(0.2, -0.2, 0.2), c(0.5, 0.5, 0.5))
    conc_change_N_Uptake_Fungal[count] = Fungal_N_Uptake(200, 150, 200, 500, 3000, 3000, 3000, 10, 15, 0.8, c(conc, conc, conc), c(10, 10, 10), c(0.2, -0.2, 0.2), c(0.5, 0.5, 0.5))
    conc_change_NO3_Uptake_Fungal[count] = Fungal_N_Uptake(200, 150, 200, 500, 3000, 3000, 3000, 10, 15, 0.8, c(10, conc, 10), c(10, 10, 10), c(0.2, -0.2, 0.2), c(0.5, 0.5, 0.5))
    conc_change_NH4_Uptake_Fungal[count] = Fungal_N_Uptake(200, 150, 200, 500, 3000, 3000, 3000, 10, 15, 0.8, c(conc, 10, 10), c(10, 10, 10), c(0.2, -0.2, 0.2), c(0.5, 0.5, 0.5))
    conc_change_Norg_Uptake_Fungal[count] = Fungal_N_Uptake(200, 150, 200, 500, 3000, 3000, 3000, 10, 15, 0.8, c(10, 10, conc), c(10, 10, 10), c(0.2, -0.2, 0.2), c(0.5, 0.5, 0.5))
    count = count + 1
  }
  SWC_range = seq(0, 1, by = 0.01)
  count = 1
  for (SWC in SWC_range) {
    SWC_change_organic[count] <- uptake_organic_N(10, 15, 10, 0.2, SWC, 0.5)
    SWC_change_NO3[count] <- uptake_NO3(10, 15, 10, -0.2, SWC, 0.5)
    SWC_change_N_Uptake[count] = Plant_N_Uptake(c(0.5, 0.5, 0.5), c(10, 10, 10), 0.2, 15, 15, SWC, c(10, 10, 10), c(0.2, -0.2, 0.2), c(0.5, 0.5, 0.5))
    SWC_change_N_Uptake_Fungal[count] = Fungal_N_Uptake(200, 150, 200, 500, 3000, 3000, 3000, 10, 15, SWC, c(10, 10, 10), c(10, 10, 10), c(0.2, -0.2, 0.2), c(0.5, 0.5, 0.5))
    count = count + 1
  }
  
  par(mfrow = c(3, 2))
  plot(concentration_range, conc_change_organic, xlab = "Concentration", ylab = "Uptake of Organic / NH4", main = "Concentration response \n with temperature 15, SWC 0.8")
  plot(concentration_range, conc_change_NO3, xlab = "Concentration", ylab = "Uptake of NO3", main = "Concentration response \n with temperature 15, SWC 0.8")
  plot(temperature_range, temp_change_organic, xlab = "Temperature", ylab = "Uptake of Organic / NH4", main = "Temperature response \n with concentation 10, SWC 0.8")
  plot(temperature_range, temp_change_NO3, xlab = "Temperature", ylab = "Uptake of NO3", main = "Temperature response \n with concentation 10, SWC 0.8")
  plot(SWC_range, SWC_change_organic, xlab = "SWC", ylab = "Uptake of Organic / NH4", main = "SWC response \n with concentation 10, temperature 15")
  plot(SWC_range, SWC_change_NO3, xlab = "SWC", ylab = "Uptake of NO3", main = "SWC response \n with concentation 5, temperature 15")
  
  par(mfrow = c(3, 2))
  plot(concentration_range, conc_change_N_Uptake, xlab = "Concentration", ylab = "total N to plant", main = "Concentration response \n with temperature 15, SWC 0.8")
  plot(concentration_range, conc_change_NH4_Uptake, xlab = "NH4", ylab = "total N to plant", main = "NH4 response \n with temperature 15, SWC 0.8, other N 10")
  plot(temperature_range, temp_change_N_Uptake, xlab = "Temperature", ylab = "total N to plant", main = "Temperature response \n with concentation 10, SWC 0.8")
  plot(concentration_range, conc_change_NO3_Uptake, xlab = "NO3", ylab = "total N to plant", main = "NO3 response \n with temperature 15, SWC 0.8, other N 10")
  plot(SWC_range, SWC_change_N_Uptake, xlab = "SWC", ylab = "total N to plant", main = "SWC response \n with concentation 10, temperature 15")
  plot(concentration_range, conc_change_Norg_Uptake, xlab = "Norg", ylab = "total N to plant", main = "Norg response \n with temperature 15, SWC 0.8, other N 10")
  
  par(mfrow = c(3, 2))
  plot(concentration_range, conc_change_N_Uptake_Fungal, xlab = "Concentration", ylab = "total N to plant", main = "Concentration response \n with temperature 15, SWC 0.8")
  plot(concentration_range, conc_change_NH4_Uptake_Fungal, xlab = "NH4", ylab = "total N to plant", main = "NH4 response \n with temperature 15, SWC 0.8, other N 10")
  plot(temperature_range, temp_change_N_Uptake_Fungal, xlab = "Temperature", ylab = "total N to plant", main = "Temperature response \n with concentation 10, SWC 0.8")
  plot(concentration_range, conc_change_NO3_Uptake_Fungal, xlab = "NO3", ylab = "total N to plant", main = "NO3 response \n with temperature 15, SWC 0.8, other N 10")
  plot(SWC_range, SWC_change_N_Uptake_Fungal, xlab = "SWC", ylab = "total N to plant", main = "SWC response \n with concentation 10, temperature 15")
  plot(concentration_range, conc_change_Norg_Uptake_Fungal, xlab = "Norg", ylab = "total N to plant", main = "Norg response \n with temperature 15, SWC 0.8, other N 10")

}
