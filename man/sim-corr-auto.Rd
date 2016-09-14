\name{Simulation under autoregressive correlation}
\alias{Fig4}
\alias{Sim4}
\title{Simulation under autoregressive correlation}
\usage{
  Sim4(N=ifelse(include.perm, 100, 500), corr=c("auto", "exch"),
       include.perm=FALSE)
  Fig4(res)
}
\description{
  You must run the simulation in \code{Sim4()} before creating the figure with
  \code{Fig4()}; see example for details.
}
\arguments{
  \item{N}{Number of simulated realizations}
  \item{corr}{Correlation structure.  "auto" for autoregressive, "exch" for
  exchangeable.}
  \item{include.perm}{Carry out permutation approach to quantifying FIR in
  addition to analytic approach?}
  \item{res}{Output from \code{Sim4()}; see example.}
}
\examples{
\dontrun{
results <- Sim4()
Fig4(results)}
  \dontshow{
    results <- Sim4(N=5)
    Fig4(results)
  }
}
