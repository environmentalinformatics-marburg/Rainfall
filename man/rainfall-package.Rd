\docType{package}
\name{Rainfall-package}
\alias{Rainfall-package}
\alias{rainfallpackage}
\title{Functions for training and prediction of an MSG based rainfall retrieval}
\description{
Rainfall
}
\author{
Hanna Meyer\cr
\emph{Maintainer:} Environmental Informatics \email{admin@environmentalinformatics-marburg.de}
}
\keyword{package}
\details{

Main functions include
\itemize{
  \item{Handling of MSG images}{
    \itemize{
      \item{
       \code{\link{getChannels}}
      }
      \item{
       \code{\link{getSunzenith}}
      }
      \item{
       \code{\link{getDate}}
      }
      \item{
       \code{\link{getDaytime}}
      }
    }
  }

  \item{Calculation of predictor variables from MSG images}{
    \itemize{
    \item{
      Derivations of spectral data \code{\link{spectralDerivate}}
      }
      \item{
      Texture variables \code{\link{textureVariables}} and \code{\link{glcmPerPatch}}
      }
      \item{
      Shape of cloud variables \code{\link{geometryVariables}}
      }
      \item{
      Zonal statistics based on cloud entities \code{\link{ppStat}}
      }
    }
  }


\item{Feature selection}{}

\item{Model training}{}

\item{Model prediction}{}

\item{Model validation}{}

\item{Visualisations}{}
}
