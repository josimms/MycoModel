\name{respiration}
\alias{Respiration}
\docType{package}
\title{
  Respiration
}
\description{
  The respiration is the same for all of the organisms, but the parameters are different.
  
  \eqn{a exp(b T)}
}

\usage{
respiration(T,
            a,
            b)
}

\arguments{
  \item{T}{
    Temperature of the air or the soil depending on the microbes.
  }
  \item{a}{
    Scaling factor.
  }
  \item{b}{
    Exponential factor.
  }
}

\value{
  \item{output}{Number of the respiration.}
}

\author{
  Joanna Simms, joanna.x.simms@ad.helsinki.fi
}
\references{
  TODO: add a reference here
}
