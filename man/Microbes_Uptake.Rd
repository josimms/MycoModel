\name{Microbe_Uptake}
\alias{Microbe_Uptake}
\alias{micorbe_uptake}
\docType{package}
\title{
  Micorbe Uptake
}
\description{
  This function uses the nitrogen uptake functions for each of the indervidual types of N compiled for the microbes.
  
  Organic SOM N uptake is considered first. The N demand is calculated first.

  \eqn{demand = 1 - \frac{B^{CN}}{B^{CN}_{opt}}}

  Then the C uptake is calculated with the same switch functions as the N uptake functions. As the C uptake per kg C is assumed to only be limited by uptake there are no modifications here.
  
  The C and N limitations are then considered against each other.
  
  \eqn{min{C limitation, N limitation}}
  
  Leading to the ultermate organic uptake being considered to be as much as the limiting elemental uptake.
  
  The inorganic uptake is then considered to top up the missing N.
  
  And then FOM uptake is then considered last if the microbes are SOM.
  
  \eqn{Organic Uptaake = demand*u(N_{type}, T, SWC)}
  
  B^{CN}, C, N ratio of the microbes.
}
\usage{

Microbe_Uptake(C_microbe,
               N_micorbe,
               NC_microbe_opt,
               NH4_avaliable,
               NO3_avaliable,
               Norg_avaliable,
               T,
               SWC,
               N_limits_R,
               N_k_R,
               SWC_k_R,
               SOM_decomposers,
               Norg_avaliable_FOM)

}

\arguments{
  \item{C_microbe}{
    The C in a microbe
  }
  \item{N_microbe}{
    The N in a microbe
  }
  \item{NC_microbe_opt}{
    The optimal microbe N:C ratio
  }
  \item{NH4_avaliable}{
    The NH4 in the soil
  }
  \item{NO3_avalibale}{
    The NO3 in the soil
  }
  \item{T}{
    Tempature at the soil horizon
  }
  \item{SWC}{
    Soil water content
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
  \item{SOM_decomposers}{
    Boolian values if it is a SOM decomposer that should be calculated
  }
  \item{Norg_avaliable_FOM}{
    Norg avaliable for FOM
  }
  \item{respiration_microbes_params}{
    Respiration parameters
  }
}

\value{
  \item{output}{
    List of the outputs
  
    1. NH4 Uptaken (kg C eq) \cr
    2. NO3 Uptaken (kg C eq) \cr
    3. Norg Uptaken (kg C eq) \cr
    4. C_uptaken (kg C) \cr
    5- Norg uptake extra FOM
  }
}

\author{
  Joanna Simms, joanna.x.simms@ad.helsinki.fi
}
\references{
  TODO: add a reference here
}

