\name{Simulation under independence}
\alias{Fig2}
\alias{Sim2}
\title{Simulation under independence}
\usage{
  Sim2(N = 100)
  Fig2(results)
}
\description{
  You must run the simulation in \code{Sim2()} before creating the figure with
  \code{Fig2()}; see example for details.
}
\arguments{
  \item{N}{Number of simulated realizations}
  \item{results}{Output from \code{Sim2()}; see example.}
}
\examples{
\dontrun{
  results <- Sim2()
  Fig2(results)}
  \dontshow{
  results <- Sim2(N=5)
  Fig2(results)
  }
}
