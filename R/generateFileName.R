generateFileName <- function (x,date){
  
  lut<-cbind(c("ca02p0001","ca02p0002","ca02p0003","ct01dk004","ct01dk005",
               "ct01dk006","ct01dk007","ct01dk008","ct01dk009","ct01dk010",
               "ct01dk011"),c(
                 "VIS0.6","VIS0.8","NIR1.6","IR3.9","WV6.2","WV7.3","IR8.7",
                 "IR9.7","IR10.8","IR12.0","IR13.4"))
  
  chstring <- lut[,1][lut[,2]%in%names(x)]
  
  return(unlist(strsplit(paste0(date,"_mt09s_",
                                chstring,"_m1hct_1000_rg01de_003000.tif")," ")))
  
}