mymap <- get_map(location = "40.71117399459069,-73.98639034958494", 
                 zoom = 12, maptype = "toner-lines")
# mymap <- get_stamenmap(bbox = c(left = min(distancePairs.num$start.station.longitude),
#                                 bottom = min(distancePairs.num$start.station.latitude),
#                                 right = max(distancePairs.num$start.station.longitude) + 0.05,
#                                 top = max(distancePairs.num$start.station.latitude)),
#                        maptype = "toner-lines")
ggmap(mymap) + geom_point(aes(x = as.numeric(start.station.longitude), 
                              y = as.numeric(start.station.latitude)), 
                          data = distancePairs, color = "blue") +
  theme_nothing()