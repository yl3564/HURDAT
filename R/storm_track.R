
#' Storm Position Track
#'
#' This function takes in a character string indicating the storm ID and returns
#' a list of the position of the storm every 30 minutes based on linear
#' interpolation.
#' @param storm_id a character string of storm ID.
#'
#' @examples
#' # Get the list of position for storm AL031869
#' storm_track("AL031869")
#'
#' @export
storm_track<-function(storm_id){
  select_storm<-hurdat[which(hurdat$ID==storm_id),]
  if(dim(select_storm)[1]==1){
    cat(paste(paste("Storm track for",storm_id),":\n",
              select_storm$Time,
              paste0(select_storm$Latitude,"N"),paste0(select_storm$Longitude,"W")))
  }else{
    r<-dim(select_storm)[1]
    Time<-c()
    for (i in 1:(r-1)) {
      time<-select_storm$Time[i]
      hour<-substr(time,1,2)
      minute<-substr(time,3,4)
      time<-paste(hour,minute,"00",sep=":") %>%
        paste("2023-05-20",.,sep=" ")
      time_seq<-seq(from=as.POSIXct(time),by="30 mins",length.out=12) %>%
        format("%H%M")
      Time<-c(Time,time_seq)
    }
    Time<-c(Time,select_storm$Time[r])

    la<-select_storm$Latitude
    lo<-select_storm$Longitude
    Latitude<-c()
    Longitude<-c()
    for (j in 1:(r-1)) {
        Latitude<-c(Latitude,la[j]+(la[j+1]-la[j])/12*(0:11))
        Longitude<-c(Longitude,lo[j]+(lo[j+1]-lo[j])/12*(0:11))
    }
    Latitude<-round(c(Latitude,select_storm$Latitude[r]),1)
    Longitude<-round(c(Longitude,select_storm$Longitude[r]),1)

    track.list<-data.frame("Name"=rep(select_storm$Name[1],length(Latitude)),
                           "ID"=rep(storm_id,length(Latitude)),
                           "Time"=Time,
                           "Latitude"=Latitude,
                           "Longitude"=Longitude)
    return(track.list)
  }
}
