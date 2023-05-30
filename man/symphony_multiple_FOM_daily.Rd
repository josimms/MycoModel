\name{Symphony_Multiple_FOM_Daily}
\alias{Symphony_Multiple_FOM_Daily}
\title{
  Symphony Multiple FOM Daily
}
\description{
  This model updates the soil states from CASSIA output (eventually).
  \eqn{C_{FOM} = C_{FOM}^{needles} + C_{FOM}^{woody} + C_{FOM}^{roots} + C_{FOM}^{mycelium}}
  
  Then the plant and microbe usages from MYCOFON are calculated.
  \eqn{NH4 = NH4 - NH4_{used}^{p} - NH4_{used}^{f}}
  \eqn{NO3 = NO3 - NO3_{used}^{p} - NO3_{used}^{f}}
  \eqn{C^{FOM} = C^{FOM} - C_{used}^{FOM,p} - C_{used}^{FOM,f}}
  \eqn{C^{SOM} = C^{SOM}}
  
  Next the microbe uptake is considered, and this is taken away from the N type states and C.
  
  \eqn{NH4 = NH4 - u_{NH4}C^{FOM} - u_{NH4}C^{SOM}}
  \eqn{NO3 = NO3 - u_{NO3}C^{FOM} - u_{NO3}C^{SOM}}
  
  \eqn{decomposition_{total} = C_{SOM} + C_{FOM}}
  
  \eqn{C_{FOM}^{needles} = C_{FOM}^{needles} + litter^{needles} - decomposition_{total} \frac{C_{FOM}^{needles}}{C_{FOM}}}
  \eqn{C_{FOM}^{woody} = C_{FOM}^{woody} + litter^{woody} - decomposition_{total} \frac{C_{FOM}^{woody}}{C_{FOM}}}
  \eqn{C_{FOM}^{roots} = C_{FOM}^{roots} + litter^{roots} - decomposition_{total} \frac{C_{FOM}^{roots}}{C_{FOM}}}
  \eqn{C_{FOM}^{mycelium} = C_{FOM}^{mycelium} + litter^{mycelium} - decomposition_{total} \frac{C_{FOM}^{mycelium}}{C_{FOM}}}
  
  \eqn{C_{SOM} = C_{SOM} - C^{microbe, SOM} + turnover*(C^{microbe, SOM} + C^{microbe, FOM})}
  
  \eqn{C^{microbe, FOM} = (1 + u_{SOM}C^{microbe, FOM} - r(Tmb) - turnover)C^{microbe, FOM}}
  \eqn{C^{microbe, SOM} = (1 + u_{SOM}C^{microbe, SOM} - r(Tmb) - turnover)C^{microbe, SOM}}
}

\usage{

symphony_multiple_FOM_daily(Tmb,
                            SWC,
                            C_FOM_needles,
                            C_FOM_woody,
                            C_FOM_roots,
                            C_FOM_mycelium,
                            C_SOM,
                            C_decompose_FOM,
                            C_decompose_SOM,
                            N_decompose_FOM,
                            N_decompose_SOM,
                            Litter_needles,
                            Litter_woody,
                            Litter_roots,
                            Litter_mycelium,
                            NH4,
                            NO3,
                            NH4_used_Plant,
                            NH4_used_Fungal,
                            NO3_used_Plant,
                            NO3_used_Fungal,
                            FOM_Norg_used_Plant,
                            FOM_Norg_used_Fungal,
                            SOM_Norg_used,
                            respiration_microbes_params,
                            N_limits_R,
                            N_k_R,
                            SWC_k_R,
                            NC_microbe_opt,
                            microbe_turnover)

}

\arguments{
  \item{Tmb}{
    Temperature at soil depth B
  }
  \item{SWC}{
    Soil water content
  }
  \item{C_FOM_needles}{
    FOM from needles
  }
  \item{C_FOM_woody}{
    FOM from woody
  }
  \item{C_FOM_roots}{
    FOM from roots
  }
  \item{C_FOM_mycelium}{
    FOM from mycelium
  }
  \item{C_SOM}{
    SOM
  }
  \item{C_decompose_FOM}{
    Microbes that decompose the FOM
  }
  \item{C_decompose_SOM}{
    Microbes that decompose the SOM
  }
  \item{N_decompose_FOM}{
    The N component of the microbes that decompose FOM
  }
  \item{N_decompose_SOM}{
    The N component of the microbes that decompose SOM
  }
  \item{Litter_needles}{
    The input of needle litter from CASSIA
  }
  \item{Litter_woody}{
    The input of woody litter from CASSIA
  }
  \item{Litter_roots}{
    The input of root litter from CASSIA
  }
  \item{Litter_mycelium}{
    The input of mycelium litter from CASSIA
  }
  \item{NH4}{
    NH4 from the soil
  }
  \item{NO3}{
    NO3 from the soil
  }
  \item{NH4_used_Plant}{
    NH4 used by / allocated by the plant
  }
  \item{NH4_used_Fungal}{
    NH4 used by / allocated by the fungus
  }
  \item{NO3_used_Plant}{
    NO3 used by / allocated by the plant
  }
  \item{NO3_used_Fungal}{
    NO3 used by / allocated by the fungus
  }
  \item{FOM_Norg_used_Plant}{
    NO3 used by / allocated by the plant
  }
  \item{FOM_Norg_used_Fungal}{
    NO3 used by / allocated by the fungus
  }
  \item{SOM_Norg_used}{
    SOM used by / allocated by the plant
  }
  \item{respiration_microbes_params}{
    Parameters that control the microbial respiration
  }
  \item{N_limits_R}{
    The parameters that change the uptake parameters for microbes
  }
  \item{N_k_R}{
    The parameters that change the uptake parameters for microbes
  }
  \item{SWC_k_R}{
    The parameters that change the uptake parameters for microbes
  }
  \item{NC_microbe_opt}{
    Optimal N:C ratio in the microbes
  }
  \item{microbe_turnover}{
    The turnover of microbes
  }
}

\value{
  \item{output}{
    List of the outputs
    
    1. C_decompose_FOM (kg C) \cr
    2. C_decompose_SOM (kg C) \cr
    3. C_FOM_needles (kg C) \cr
    4. C_FOM_woody (kg C) \cr
    5. C_FOM_roots (kg C) \cr
    6. C_FOM_mycelium (kg C) \cr
    7. C_SOM (kg C) \cr
    8. NH4 (kg C) \cr
    8. NO3 (kg C) \cr
    8. Respiration (kg C)
  }
}

\author{
  Joanna Simms, joanna.x.simms@ad.helsinki.fi
}
\references{
  TODO: add a reference here
}
