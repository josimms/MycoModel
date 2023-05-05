test_function <- function() {
  
}

library(devtools)
install_github("josimms/CASSIA")

library(CASSIA)

CASSIA(Hyde_weather, site = "Hyde", sperling_model = T, PRELES_GPP = T)


