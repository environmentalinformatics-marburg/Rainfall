
#' Calculate shape indices listed in Borg 1998 
#' @param Ar area of the entities
#' @param Ur Perimeter of the cloud entities
#' @param De Diameter of the inner circle
#' @param Du Diameter of the outer circle
#' @return A RasterStack
#' @details This function is mainly used as internal function of 
#' \code{\link{geometryVariables}}
#' @author Hanna Meyer
#' @references Borg, E. & Fichtelmann, B. (1998):
#' Vergleichende Analyse von Formindizes zur Charaktersierung von 
#' Landschaftsobjekten unter ökologischen Aspekten.
#'  Zeitschrift für Photogrammetrie und Vermessung, 66, 108-112.
#'  @examples
#'  msg_example <-  raster(system.file("extdata/msg",
#'  "201007121650_mt09s_ct01dk009_m1hct_1000_rg01de_003000.rst",
#'  package="Rainfall"))
#'  # set non clouded areas to NA:
#'  msg_example=reclassify(msg_example, cbind(-99,NA))
#'  #calculate geometry Variables
#'  geometry <- geometryVariables(msg_example,var=c("Ar","Ur","Re","Ru"))
#'  borg<-borgIndices(geometry$Ar,geometry$Ur,geometry$Re*2, geometry$Ru*2)
#'  plot(borg)

borgIndices <- function(Ar,Ur,De,Du){
  Dk<-sqrt((Ar/pi))*2 # Diameter of the circle of area Ar
  Uk<-Dk*pi # perimeter of the circle of area Ar
  Ue<-De*pi #perimeter of the inner circle
  Uu=Du*pi #perimeter of the outer circle
  Au<-pi*((Du/2)^2) #area of outer circle
  Ae<-pi*((De/2)^2) #area of the inner circle
  CI1<-Ue/Uk
  CO1<-Uk/Uu
  CI2<-(Uk-Ue)/Uk
  CO2<-(Uu-Uk)/Uu
  CCI1<-(CI1+CO1)/2
  CCI2<-1-sqrt(CI2*CO2)
  ### orientation at re circle of same area
  CO<-(2*sqrt(pi*Ar))/Ur
  SHD<-Ur/(2*sqrt(pi*Ar))
  C1<-(4*Ar)/(Ur^2)
  E<-Ur/(sqrt(Ar))
  TR<-(4*pi*Ar)/(Ur^2)
  ###orientation at the smallest outer circle
  CR<-Ar/Au
  C2<-sqrt(Ar/Au)
  FR<-Ar/Du^2
  EI<-(pi*Du*(0.5*Du))/Ar
  SF1<-Uu/Ur
  GSI<-Ur/Du
  ###orientation at the largest inner circle
  SF2<-Ae/Ar
  ###orientation on inner and outer circle
  C3<-sqrt(De/Du)
  SF3<-(Au-Ae)/Ar
  result<-stack(CI1,CO1,CI2,CO2,CCI1,CCI2,CO,SHD,C1,E,TR,CR,
               C2,FR,EI,SF1,GSI,SF2,C3,SF3)
  names(result)<-c("CI1","CO1","CI2","CO2","CCI1","CCI2","CO","SHD","C1","E",
                  "TR","CR","C2","FR","EI","SF1","GSI","SF2","C3","SF3")
  return(result)
}