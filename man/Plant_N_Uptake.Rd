\name{Plant N Uptake}
\alias{Plant_N_Uptake}
\alias{plant_n_uptake}
\title{
  Plant Nitrogen Uptake
}
\description{
  This function uses the nitrogen uptake functions for each of the indervidual types of N compiled for the plant.
  
  The demand is calculated by the decision function. Then TODO: I should link this somehow.
  
  \eqn{N_to_root = u_{organic}(FOM, T, SWC) + u_{NH4}(NH4, T, SWC) + f(NH4)u_{NO3}(NO3, T, SWC)}
  
  The nitrogen to the root is a combination of all of the uptake functions, with the NO3 nitrogen uptake updated with a NH4 modifier. NH4, NO3 and FOM are the amounts in the soil. T is temperature in the soil B horizon and then SWC is the soil water content.
  
  \eqn{f(NH4) = \frac{a NH4^8}{NH4^8 + b^8}}
  
  Where a and b are fitted parameters and the function gives a value between 0 and 1.
}
\usage{

Plant_N_Uptake(NC_in_root_opt, 
               T,
               SWC,
               m,
               NH4_in,
               NO3_in,
               FOM_in,
               N_limits_R,
               N_k_R,
               SWC_k_R,
               C_roots,
               N_roots,
               percentage_C_biomass,
               parameters)

}

\arguments{
  \item{NC_in_root_opt}{
    The optimal NC ratio in the roots
  }
  \item{T}{
    Temperature
  }
  \item{SWC}{
    Soil water content
  }
  \item{m}{
    Mycorrhized potential
  }
  \item{NH4_in}{
    The NH4 in the soil
  }
  \item{NO3_in}{
    The NO3 in the soil
  }
  \item{FOM_in}{
    The nitrogen in the FOM
  }
  \item{N_limits_R}{
    Turnover of the roots that ARE mycorrhized
  }
  \item{N_k_R}{
    Parameters to control the N uptake
  }
  \item{SWC_k_R}{
    Parameters to control the N uptake
  }
  \item{C_roots}{
    Carbon in the roots
  }
  \item{N_roots}{
    Nitrogen in the roots
  }
  \item{percentage_C_biomass}{
    The percentage of C in the biomass
  }
  \item{parameters}{
    The parameters that control the NH4 effect on NO3 uptake
  }
}

\value{
  \item{output}{
    List of the outputs
    
    1. N_to_Plant (kg C eq) \cr
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

\examples{
  \dontrun{
     ## Optional simple examples of the most important functions
     ## These can be in \dontrun{} and \donttest{} blocks.   
  }
}
