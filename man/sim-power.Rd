\name{Comparing power}
\alias{Fig3}
\alias{SimCT}
\alias{SimSS}
\title{Comparing power}
\usage{
  SimCT(N = 100)
  SimSS(N = 100)
  Fig3(resultsSim2, resultsCT, resultsSS)
}
\description{
  You must run the simulations in \code{\link{Sim2}()}, \code{SimCT()}, and
  \code{SimSS()} before creating the figure with \code{Fig3()}; see example
  for details.  CT = CovTest, SS = Sample splitting.
}
\arguments{
  \item{N}{Number of simulated realizations}
  \item{resultsSim2}{Output from \code{\link{Sim2}()}; see example.}
  \item{resultsCT}{Output from \code{SimCT()} (CovTest); see example.}
  \item{resultsSS}{Output from \code{SimSS()} (Sample Splitting); see
  example.}
}
\examples{
\dontrun{
  res1 <- Sim2()
  res2 <- SimCT()
  res3 <- SimSS()
  Fig3(res1, res2, res3)
  }
\dontshow{
  res1 <- Sim2(5)
  res2 <- SimCT(5)
  res3 <- SimSS(5)
  Fig3(res1, res2, res3)
}
}
