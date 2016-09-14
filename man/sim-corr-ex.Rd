\name{Simulation under exchangeable correlation}
\alias{Fig5}
\alias{Sim5}
\title{Simulation under exchangeable correlation}
\usage{
  Fig5(res)
}
\description{
  You must run the simulation in \code{Sim4()} before creating the figure with
  \code{Fig5()}; see example for details.
}
\arguments{
  \item{res}{Output from \code{Sim4()}; see example.}
}
\examples{
\dontrun{
results <- Sim4(corr="exch", include.perm=TRUE)
Fig5(results)}
  \dontshow{
    results <- Sim4(N=5, corr="exch", include.perm=TRUE)
    Fig5(results)
  }
}
