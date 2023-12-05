
#' Plot Map of Storm Movement Path and Size
#'
#' This function takes in a character string indicating the storm ID and returns
#' a map recording the movement path of the storm with different sizes and colors
#' indicating the size of the selected storm. The
#'
#' @param storm_id a character string of storm ID.
#'
#' @examples
#' storm_map_size("AL182012")
#'
#' @export
storm_map_size<-function(storm_id){
  hurdat_map<-filter(hurdat,hurdat$ID == storm_id)
  map.dat<-map_data("state")
  x_min<-min(c(-hurdat_map$Longitude,map.dat$long))
  x_max<-max(c(-hurdat_map$Longitude,map.dat$long))
  y_min<-min(c(hurdat_map$Latitude,map.dat$lat))
  y_max<-max(c(hurdat_map$Latitude,map.dat$lat))
  map("world", xlim = c(x_min, x_max), ylim = c(y_min, y_max))
  map("state",add=T)
  r<-dim(hurdat_map)[1]
  for (i in 1:r) {
    NE<-max(c(0,hurdat_map$NE_34[i],hurdat_map$NE_50[i],hurdat_map$NE_64[i]),na.rm=T)/400
    SE<-max(c(0,hurdat_map$SE_34[i],hurdat_map$SE_50[i],hurdat_map$SE_64[i]),na.rm=T)/400
    SW<-max(c(0,hurdat_map$SW_34[i],hurdat_map$SW_50[i],hurdat_map$SW_64[i]),na.rm=T)/400
    NW<-max(c(0,hurdat_map$NW_34[i],hurdat_map$NW_50[i],hurdat_map$NW_64[i]),na.rm=T)/400
    x1<-seq(from=(-hurdat_map$Longitude[i]-NW),
            to=-hurdat_map$Longitude[i],
            length.out=100)
    y1<-c(hurdat_map$Latitude[i]+NW*sin(seq(from=0,to=pi/2,length.out=99)),
          hurdat_map$Latitude[i])
    x2<-seq(from=-hurdat_map$Longitude[i],
            to=(-hurdat_map$Longitude[i]+NE),
            length.out=100)
    y2<-c(hurdat_map$Latitude[i],
          hurdat_map$Latitude[i]+NE*sin(seq(from=pi/2,to=0,length.out=99)))
    x3<-seq(from=(-hurdat_map$Longitude[i]-SW),
            to=-hurdat_map$Longitude[i],length.out=100)
    y3<-c(hurdat_map$Latitude[i]-SW*sin(seq(from=0,to=pi/2,length.out=99)),
          hurdat_map$Latitude[i])
    x4<-seq(from=-hurdat_map$Longitude[i],
            to=(-hurdat_map$Longitude[i]+SE),
            length.out=100)
    y4<-c(hurdat_map$Latitude[i],
          hurdat_map$Latitude[i]-SE*sin(seq(from=pi/2,to=0,length.out=99)))
    polygon(x1,y1,col="red",border = "red")
    polygon(x2,y2,col="blue",border = "blue")
    polygon(x3,y3,col="green",border = "green")
    polygon(x4,y4,col="yellow",border = "yellow")
    points(x=-hurdat_map$Longitude[i],y=hurdat_map$Latitude[i],col="black",pch = 16,cex=.2)
  }
}
