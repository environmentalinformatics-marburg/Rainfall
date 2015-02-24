# Rainfall
Functions for training, prediction and validation of an MSG based optical rainfall retrieval


Main functions include
\itemize{
  \item{Handling of MSG images}{
    \itemize{
      \item{
      e.g \code{\link{getChannels}}
      }
    }
  }

  \item{Calculation of predictor variables from MSG images}{
    \itemize{
      \item{
      Texture variables \code{\link{textureVariables}} and \code{\link{glcmPerPatch}}
      }
      \item{
      Shape of cloud variables \code{\link{geometryVariables}}
      }
    }
  }


\item{Feature selection}{}

\item{Model training}{}

\item{Model prediction}{}

\item{Model validation}{}

\item{Visualisations}{}



Install the package with 
install_github("environmentalinformatics-marburg/Rainfall")
