\name{myconfon_updated}
\alias{myconfon}
\alias{myconfon}
\title{
  Mycofon model (todo: reference), with updates to include the uptake properties for three types of nitrogen as well as the decision functionality.
}
\description{
  This function brings together the C or N balance as in the original MYCOFON model, the decision functions for resource transfer, fungal growth and multiple N uptake functions. The equations in this function are below.
  
  m = \frac{N^r}{B^{rf}_{opt}C^r}
  
  Where N is nitrogen, C is carbon, r roots, f fungai, B biomass, opt is optimum.
  
  C^r = C^r + C^{CASSIA}_{Allocated} - (1 - m)*t^r*C^r - m*t^m - r(Tmb)*C^r - d^r(C^{CASSIA})
  
  Where, C^{CASSIA}_{Allocated} is the carbon allocated from the CASSIA model, t is turnover, which is different for micorhized (m) and non-mycorrhized roots (r), Tmb is temperature of the soil, d() is decomposition and r() is respiration.
  
  C^f = C^f + d(C^{CASSIA}) - t^f * N^{r} * p^{C, biomass} - r(Tmb) * N^{r} * p^{C, biomass}
  
  Where p is percentage.
  
  N^r = N^r + d^f(N^{f}) + u^r(T, SWC, NH4, NO3, FOM_Norg, C^r, N^r)*C_roots - (1 - m)*t^r*C^r*fungal_NC_ratio - m*t^m*fungal_NC_ratio - a(N^f)
  
  a allocation from roots to plant, fungal_NC_ratio
  
  N^f = N^f + u^f(T, SWC, NH4, NO3, FOM_Norg, C^r, N^r)*C^f*fungal_NC_ratio - t^f*C^f*fungal_NC_ratio - d^f(N^f)
}
\usage{

mycofon_balence(Allocation_C_CASSIA,
                N_to_CASSIA,
                C_allocation,
                N_allocation,
                C_roots,
                N_roots,
                percentage_C_biomass,
                optimal_root_fungal_biomass_ratio,
                C_roots_biomass,
                C_fungal,
                N_fungal,
                turnover_roots,
                turnover_fungal,
                respiration_parameters_R,
                NH4,
                NO3,
                FOM_Norg,
                NC_in_root_opt,
                NC_in_fungai_opt,
                T,
                Tsb,
                SWC,
                N_limits_Plant,
                N_k_Plant,
                SWC_k_Plant,
                N_limits_Fungal,
                N_k_Fungal,
                SWC_k_Fungal,
                NC_fungal_opt,
                mantle_mass,
                ERM_mass,
                parameters_NH4_on_NO3)

}

\author{
  Joanna Simms, joanna.x.simms@ad.helsinki.fi
}
\references{
  TODO: add a reference here
}
\keyword{mycofon, decision, balance, }
\seealso{
  Optional links to other man pages // TODO: look at the functions
}
\examples{
  \dontrun{
     ## Optional simple examples of the most important functions
     ## These can be in \dontrun{} and \donttest{} blocks.   
  }
}
