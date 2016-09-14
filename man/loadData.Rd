\name{loadData}
\alias{loadData}
\title{Load data from an external source}
\usage{
  loadData(name, envir=parent.frame())
}
\description{
  Loads data from a website; requires an internet connecton.
}
\arguments{
  \item{name}{Name of data set.}
  \item{envir}{Environment in which to load data set.}
}
\examples{
  # Loads objects X and y into the current environment
  loadData("bc-tcga")
  dim(X)
  hist(y, col="gray", border="white")
}
