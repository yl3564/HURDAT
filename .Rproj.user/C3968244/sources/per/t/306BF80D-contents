
#' Plot Map of Storms Movement Paths
#'
#' This function takes in a character vector indicating a selection of storms ID
#' and returns a map recording their paths in different colors.
#'
#' @param id a character vector of storms ID.
#'
#' @examples
#' selected.storm<-c("AL011851","AL041851","AL061851","AL031924")
#' storm_map(selected.storm)
#'
#' @export
storm_map<-function(id){
  index = 0
  hurdat_map<-filter(hurdat,hurdat$ID %in% id)
  map.dat<-map_data("state")
  x_min<-min(c(-hurdat_map$Longitude,map.dat$long))
  x_max<-max(c(-hurdat_map$Longitude,map.dat$long))
  y_min<-min(c(hurdat_map$Latitude,map.dat$lat))
  y_max<-max(c(hurdat_map$Latitude,map.dat$lat))
  map("world", xlim = c(x_min, x_max), ylim = c(y_min, y_max))
  map("state",add=T)
  col_sample<-sample(1:657,length(id))
  for (i in id){
    hurdat_map<-filter(hurdat,hurdat$ID == i)
    index = index + 1
    points(x = -hurdat_map$Longitude,y = hurdat_map$Latitude,col = col_sample[index]
           ,pch = 16,cex = 0.5)
  }
  legend(x=-120,y=45,legend = id, col = col_sample, pch = 16, cex = 0.2)
}

