
#' Whether Made Landfall
#'
#' This function takes in a character string indicating the storm ID and returns
#' whether the storm made landfall in the continental United States.
#'
#' @param storm_id a character string of storm ID.
#'
#' @return Integer value:
#' 0: The storm did not make landfall.
#' 1: The storm made landfall.
#'
#' @examples
#' is.landfall("AL061851")
#' is.landfall("AL071851")
#'
#' @export
is.landfall<-function(storm_id){
  map.dat<-map_data("usa")
  hurdat_position<-filter(hurdat,hurdat$ID == storm_id)
  decision<-point.in.polygon(-hurdat_position$Longitude,hurdat_position$Latitude,
                             map.dat$long,map.dat$lat)
  decision<-ifelse(1%in%decision,1,0)
  return(decision)
}
