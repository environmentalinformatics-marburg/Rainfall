#' Randomly Select a percentage of scenes
#' @param sampsize proportion of scenes to select
#' @param seed A number used as seed
#' @param years Start to end year
#' @param month start to end month
#' @param days start to end days
#' @param hours start to end hours
#' @return character vector containing teh dates of the selected scenes 
#' @author Hanna Meyer
#' @export randomScenes
#' @examples
#' randomScenes(sampsize=0.01)
#' 
randomScenes <- function (sampsize=0.25,seed=25, years=c(2010:2010),months=c(1:12),
                          days=1:30,hours=0:23){

##alle kombination aus k,i,l. 
years=formatC(years,flag=0,width=2)
months=formatC(months,flag=0,width=2)
days=formatC(days,flag=0,width=2)
hours=formatC(hours,flag=0,width=2)

allscenes<-expand.grid(years,months,days,hours)
set.seed(seed)
trainingscenes<-allscenes[sample(nrow(allscenes),sampsize*nrow(allscenes)),]
trainingscenes=apply( trainingscenes , 1 , paste , collapse = "" )
return(trainingscenes)
}