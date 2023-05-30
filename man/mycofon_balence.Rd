\name{Mycofon Balence}
\alias{mycofon_balence}
\alias{mycofon_balence}
\title{
  Mycofon model (todo: reference), with updates to include the uptake properties for three types of nitrogen as well as the decision functionality.
}
\description{
  This function brings together the C or N balance as in the original MYCOFON model, the decision functions for resource transfer, fungal growth and multiple N uptake functions. The equations in this function are below.
  
  \eqn{m = \frac{N^r}{B^{rf}_{opt}C^r}}
  
  Where N is nitrogen, C is carbon, r roots, f fungai, B biomass, opt is optimum.
  
  \eqn{C^r = C^r + C^{CASSIA}_{Allocated} - (1 - m)t^rC^r - mt^m - r(Tmb)C^r - d^r(C^{CASSIA})}
  
  Where, C^{CASSIA}_{Allocated} is the carbon allocated from the CASSIA model, t is turnover, which is different for micorhized (m) and non-mycorrhized roots (r), Tmb is temperature of the soil, d() is decomposition and r() is respiration.
  
  \eqn{C^f = C^f + d(C^{CASSIA}) - t^f  N^{r}  p^{C, biomass} - r(Tmb)  N^{r}  p^{C, biomass}}
  
  Where p is percentage.
  
  \eqn{N^r = N^r + d^f(N^{f}) + u^r(T, SWC, NH4, NO3, FOM_Norg, C^r, N^r)C_roots - (1 - m)t^rC^rfungal_NC_ratio - mt^mfungal_NC_ratio - a(N^f)}
  
  a allocation from roots to plant, fungal_NC_ratio
  
  \eqn{N^f = N^f + u^f(T, SWC, NH4, NO3, FOM_Norg, C^r, N^r)C^ffungal_NC_ratio - t^fC^f fungal_NC_ratio - d^f(N^f)}
}
\usage{

mycofon_balence(C_roots,
                N_roots,
                percentage_C_biomass,
                optimal_root_fungal_biomass_ratio,
                C_fungal,
                N_fungal,
                turnover_roots,
                turnover_roots_mycorrhized,
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

\arguments{
  \item{C_roots}{
    Carbon in roots.
  }
  \item{N_roots}{
    Nitrogen in roots.
  }
  \item{percentage_C_biomass}{
    The percentage of C that is in biomass.
  }
  \item{optimal_root_fungal_biomass_ratio}{
    The ratio of root and fungal biomass, as in mochrizzal rate.
  }
  \item{C_fungal}{
    Carbon in mycorrhzia.
  }
  \item{N_fungal}{
    Nitrogen in mycorrhiza.
  }
  \item{turnover_roots}{
    Turnover of the roots that ARE NOT mycorrhized
  }
  \item{turnover_mycorrhized}{
    Turnover of the roots that ARE mycorrhized
  }
  \item{turnover_fungal}{
    Turnover of the mycorrhized roots
  }
  \item{respiration_parameters_R}{
    Respiration parameters
  }
  \item{NH4}{
    Amount of NH4 in the soil
  }
  \item{NO3}{
    Amount of NO3 in the soil
  }
  \item{FOM_Norg}{
    The nitrogen in FOM
  }
  \item{NC_in_root_opt}{
    The ideal NC ratio in the roots
  }
  \item{NC_in_fungai_opt}{
    The idea NC ratio in the fungus
  }
  \item{T}{
    Temperature of the air, 'C
  }
  \item{Tsb}{
    Temperature of the B horizon of the soil, 'C
  }
  \item{SWC}{
    Soil water content, \%
  }
  \item{N_limits_Plant}{
    Parameters to control the N uptake
  }
  \item{N_k_Plant}{
    Parameters to control the N uptake
  }
  \item{SWC_k_Plant}{
    Parameters to control the N uptake
  }
  \item{N_limits_Fungal}{
    Parameters to control the N uptake
  }
  \item{N_k_Fungal}{
    Parameters to control the N uptake
  }
  \item{SWX_k_Fungal}{
    Parameters to control the N uptake
  }
  \item{mantle_mass}{
    The mass of the mantle section of the fungal mass
  }
  \item{ERM_mass}{
    The mass of the ERM section of the fungal mass 
  }
  \item{parameters_NH4_on_NH3}{
    The additional parameters to control the uptake of the NO3 from NH4 in the plant uptake.
  }
}

\value{
  \item{output}{
    List of the outputs
    
    1. C_roots (kg C) \cr
    2. C_fungal (kg C) \cr
    3. N_roots (kg C eq) \cr
    4. N_fungal (kg C eq) \cr
    5. N uptake to the plant from the soil (kg C eq) \cr
    6. N uptake to the fungus from the soil (kg C eq)
  }
}

\author{
  Joanna Simms, joanna.x.simms@ad.helsinki.fi
}
\references{
  TODO: add a reference here
}

\examples{
  \dontrun{
     ## Optional simple examples of the most important functions
     ## These can be in \dontrun{} and \donttest{} blocks.   
  }
}
