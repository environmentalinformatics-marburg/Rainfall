#' Calculate selected geometry variables for clouds
#' 
#' @param sceneraster A rasterLayer containing NA for non clouds and any value for 
#' clouded areas
#' @param var A list of character values indicating the variables to be used.
#' Possible values
#' are "Ar",SI","CA","Ur","CAI","PAR","distEdges","Re","Ru","OIC",
#' CI1","CO1","CI2","CO2","CCI1","CCI2","CO","SHD","C1","E",
#' "TR","CR","C2","FR","EI","SF1","GSI","SF2","C3","SF3"
#' @return A list of RasterStacks containing the texture parameters for each 
#' combination of channel and filter  
#' @author Hanna Meyer
#' @export geometryVariables
#' @seealso \code{\link{SDMTools}}, \code{\link{clump}},
#'  \code{\link{borgIndices}}
#' @examples
#' 
#' msg_example <-getChannels(inpath=system.file("extdata/msg",package="Rainfall"),
#' channels="VIS0.8")
#' 
#' #calculate geometry Variables
#' geometry <- geometryVariables(msg_example,var=c("Ar","Ur"))
#' plot(geometry)


geometryVariables <- function(sceneraster, var=c("Ar")){
  if (class(sceneraster)=="RasterStack"||class(sceneraster)=="RasterBrick"){
    sceneraster <- sceneraster[[1]]
    print ("warning: only first element of the raster stack is used...")
  }

  require(SDMTools)
  namesvar<-c()
  cloudPatches<-clump(sceneraster)
  names(cloudPatches)<-"cloudPatches"
  cloudStats<-PatchStat(cloudPatches)
  # patch area:
  if ("Ar"%in% var||any(c("CI1","CO1","CI2","CO2","CCI1","CCI2","CO",
                          "SHD","C1","E", "TR","CR","C2","FR","EI","SF1",
                          "GSI","SF2","C3","SF3")%in% var)){
    Ar<-reclassify(cloudPatches, 
                   cbind(cloudStats$patchID,cloudStats$area))
  }
  #shape index:
  if ("SI"%in% var){
    SI<-reclassify(cloudPatches, 
                   cbind(cloudStats$patchID,cloudStats$shape.index))
  }
  #core area:
  if ("CA"%in% var){
    CA<-reclassify(cloudPatches, 
                         cbind(cloudStats$patchID,cloudStats$core.area.index))
  }
  #perimeter:
  if ("Ur"%in% var||any(c("CI1","CO1","CI2","CO2","CCI1","CCI2","CO",
                          "SHD","C1","E","TR","CR","C2","FR","EI","SF1",
                          "GSI","SF2","C3","SF3")%in% var)){
    Ur<-reclassify(cloudPatches, 
                   cbind(cloudStats$patchID,cloudStats$perimeter))
  }
  #core.area.index
  if ("CAI"%in% var){
    CAI<-reclassify(cloudPatches, cbind(cloudStats$patchID,
                                        cloudStats$core.area.index))
  }
  #the ratio of the patch perimeter (m) to area (m2)
  if ("PAR"%in% var){
    PAR<-reclassify(cloudPatches, 
                    cbind(cloudStats$patchID,cloudStats$perim.area.ratio))
  }
  #distance to edge:
  if ("distEdges"%in% var||"Re"%in% var||"OIC"%in% var||
        any(c("CI1","CO1","CI2","CO2","CCI1","CCI2","CO","SHD","C1","E",
              "TR","CR","C2","FR","EI","SF1","GSI","SF2","C3","SF3")%in% var)){
    edges<-boundaries(cloudPatches, type='inner')
    distEdges<-gridDistance(edges,origin=1) 
    values(distEdges)[is.na(values(cloudPatches))]<-NA
  }
  #innerCircle (largest circle)= maximum distance from edge
  if ("Re"%in% var||"OIC"%in% var||any(c("CI1","CO1","CI2","CO2","CCI1","CCI2",
                                         "CO","SHD","C1","E","TR","CR","C2",
                                         "FR","EI","SF1","GSI","SF2","C3",
                                         "SF3")%in% var)){
    tmp<-zonal(distEdges,cloudPatches,fun="max")
    Re<-cloudPatches
    Re<-reclassify(Re,tmp) #inner circle
  }
  ##### outer circle
  if ("Ru"%in% var||"OIC"%in% var||any(c("CI1","CO1","CI2","CO2","CCI1","CCI2",
                                         "CO","SHD","C1","E", "TR","CR","C2",
                                         "FR","EI","SF1","GSI","SF2","C3",
                                         "SF3")%in% var)){
    oci<-c()
    
    for (i in 1:max(values(cloudPatches),na.rm=TRUE)){
      cp<-cloudPatches
      cp[cp!=i]<-NA
      cpp<-rasterToPolygons(cp,dissolve=TRUE)
      centroid<-gCentroid(cpp, byid=TRUE,id=attributes(cpp)$plotOrder)
      
      dist<-distanceFromPoints(cloudPatches, centroid)
      dist[is.na(cp)]<-NA
      oci[i]<-max(values(dist),na.rm=TRUE)
    }
    Ru<-reclassify(cloudPatches, cbind(cloudStats$patchID,oci)) #outer Circle
  }
  if ("OIC"%in% var){ #outerInnerCircle
    OIC<-Ru-Re
  }
  ### Indices listed and/or developed by Borg 98
  if (any(c("CI1","CO1","CI2","CO2","CCI1","CCI2","CO","SHD","C1","E",
            "TR","CR","C2","FR","EI","SF1","GSI","SF2","C3","SF3")%in% var)){
    borg<-borgIndices(Ar=Ar,Ur=Ur,De=Re*2,Du=Ru*2)
  }
  ##############################################################################  
  #subset of borg
  if(exists("borg")){
  borg <- borg[[which(c("CI1","CO1","CI2","CO2","CCI1","CCI2","CO","SHD",
                        "C1","E","TR","CR","C2","FR","EI","SF1","GSI","SF2",
                        "C3","SF3")%in% var)]]
  }
  splitstring <- var[as.logical(paste0(!var %in%c("CI1","CO1","CI2","CO2",
                                                  "CCI1","CCI2","CO","SHD","C1",
                                                  "E","TR","CR","C2","FR","EI",
                                                  "SF1","GSI","SF2","C3",
                                                  "SF3")))]
  
  result<-eval(parse(text=paste0("stack(",paste0(paste0(
    splitstring,collapse=","),collapse=""),")")))
  names(result) <-var[as.logical(paste0(!var %in%c("CI1","CO1","CI2","CO2",
                                                   "CCI1","CCI2","CO","SHD",
                                                   "C1","E","TR","CR","C2","FR",
                                                   "EI","SF1","GSI","SF2",
                                                   "C3","SF3")))]
  if(exists("borg")){
    result <- stack (result,borg)
  }
  return(result)  
}