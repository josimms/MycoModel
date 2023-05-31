\name{Plant_N_Uptake}
\alias{Plant_N_Uptake}
\alias{plant_n_uptake}
\title{
  Fungal Nitrogen Uptake
}
\description{
  This function uses the nitrogen uptake functions for each of the indervidual types of N compiled for the fungae.
  
  The demand is calculated by the decision function.
  
  \eqn{demand = \frac{C^f B^{mantle mass}}{B^f B^{ERM}}(1 - \frac{B^{rf}}{B^{rf}_{opt}})}
}

\usage{

Fungal_N_Uptake(C_fungal,
                N_fungal,
                NC_fungal_opt,
                mantle_mass,
                ERM_mass,
                percentage_C_biomass,
                T,
                SWC,
                NH4,
                NO3,
                FOM_Norg,
                N_limits_R,
                N_k_R,
                SWC_k_R)

}

\arguments{
  \item{C_fungal}{
    C in the fungal mass
  }
  \item{N_fungal}{
    N in the fungal mass
  }
  \item{NC_fungal_opt}{
    The optimal ratio between C and N 
  }
  \item{mantle_mass}{
    The fungal mass that is mantle
  }
  \item{ERM_mass}{
    The fungal mass that is ERM
  }
  \item{percentage_C_biomass}{
    The percentage of carbon in biomass
  }
  \item{T}{
    The temperature at the soil horizon B
  }
  \item{SWC}{
    The soil water content
  }
  \item{NH4}{
    NH4 in the soil
  }
  \item{NO3}{
    NO3 in the soil
  }
  \item{FOM_Norg}{
    The nitrogen in the FOM
  }
  \item{N_limits_R}{
    Parameters to control the N uptake
  }
  \item{N_k_R}{
    Parameters to control the N uptake
  }
  \item{SWC_k_R}{
    Parameters to control the N uptake
  }
}

\value{
  \item{output}{
    List of the outputs
    
    1. N_to_fungal (kg C eq) \cr
    2. NH4_used (kg C eq) \cr
    3. NO3_used (kg C eq) \cr
    4. Norg_used (kg C eq)
  }
}

\author{
  Joanna Simms, joanna.x.simms@ad.helsinki.fi
}
\references{
  TODO: add a reference here
}
