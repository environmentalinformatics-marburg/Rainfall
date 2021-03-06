% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geometryVariables.R
\name{geometryVariables}
\alias{geometryVariables}
\title{Calculate selected geometry variables for clouds}
\usage{
geometryVariables(sceneraster, var = c("Ar"))
}
\arguments{
\item{sceneraster}{A rasterLayer containing NA for non clouds and any value for 
clouded areas}

\item{var}{A list of character values indicating the variables to be used.
Possible values
are "Ar",SI","CA","Ur","CAI","PAR","distEdges","Re","Ru","OIC",
CI1","CO1","CI2","CO2","CCI1","CCI2","CO","SHD","C1","E",
"TR","CR","C2","FR","EI","SF1","GSI","SF2","C3","SF3"}
}
\value{
A list of RasterStacks containing the texture parameters for each 
combination of channel and filter
}
\description{
Calculate selected geometry variables for clouds
}
\examples{

msg_example <-getChannels(inpath=system.file("extdata/msg",package="Rainfall"),
channels="VIS0.8")

#calculate geometry Variables
geometry <- geometryVariables(msg_example,var=c("Ar","Ur"))
plot(geometry)
}
\author{
Hanna Meyer
}
\seealso{
\code{\link{SDMTools}}, \code{\link{clump}},
 \code{\link{borgIndices}}
}

