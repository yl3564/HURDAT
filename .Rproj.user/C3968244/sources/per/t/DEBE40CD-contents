
#' Accumulated Cyclone Energy
#'
#' This function takes in a character string indicating the storm ID and returns
#' the accumulated cyclone energy of the selected storm.
#'
#' @param storm_id a character string of storm ID.
#'
#' @examples
#' # The accumulated cyclone energy of storm AL061866
#' cyclone_energy("AL061866")
#'
#' @export
cyclone_energy<-function(storm_id){
  hurdat_id<-filter(hurdat,hurdat$ID == storm_id)
  hurdat_id<-filter(hurdat_id,hurdat_id$Size >= 35)
  return(sum(hurdat_id$Size^2)/10000)
}


