library(lubridate)
library(plyr)
library(dplyr)
library(data.table)
library(png)
library(ggmap)
library(gganimate)
library(tweenr)

start <- ymd("2013-07-01"); end <- ymd("2015-10-01")
months <- as.character(seq(start, end, by = "1 month"))
sums.mat <- data.frame(station.id = character(), n = integer(), mo = character(), start = logical())
for (m in 1:length(months)){  
  t <- processMonthTrip(months[m], distancePairs)
  start.station <- t %>% group_by(station.id = start.station.id) %>% 
    summarize(n = n(), mo = months[m], start = TRUE)
  sums.mat <- rbind(sums.mat, start.station)
}

distance <- unique(distancePairs[, c("start.station.id", "start.station.latitude", 
                                     "start.station.longitude"), with = FALSE])
sums <- sums.mat %>% left_join(y = distance, by = c("station.id" = "start.station.id")) %>% 
          select(station.id, n, mo, start, station.latitude = start.station.latitude, 
                 station.longitude = start.station.longitude)
sums$mo <- as.factor(sums$mo)

mymap13 <- get_map(location = "40.72417399459069,-73.98639034958494", zoom = 13, 
                   maptype = "toner-lines")
inv <- readPNG("invert.png")
g <- ggmap(mymap13, extent = "device") + 
       inset_raster(inv, xmin = -74.048, xmax = -73.928, ymin = 40.68, ymax = 40.766) +
       geom_point(aes(x = as.numeric(station.longitude), y = as.numeric(station.latitude), size = n,
                      frame = mo), data = sums, alpha = 0.3, color = "blue") +
       scale_size(range = c(3, 8)) +
       theme(
            legend.position = "none",
            legend.key = element_blank(), 
            legend.text = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(), 
            axis.ticks = element_blank(),
            panel.background = element_rect(color = "black", fill = "white"),
            plot.background = element_rect(fill = "white", color = "white"),
            plot.title = element_text(size = 18),
            panel.border = element_rect(fill = NA, color = "white"),
            plot.margin = unit(c(0, 0, 0, 0), "lines")
            ) 

gg_animate(g)
