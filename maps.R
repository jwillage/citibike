library(lubridate)
library(plyr)
library(dplyr)
library(data.table)
library(ggmap)
library(gganimate)
library(tweenr)

start <- ymd("2013-07-01"); end <- ymd("2015-10-01")
months <- as.character(seq(start, end, by = "1 month"))
sums.mat <- data.frame(station.id = character(), n = integer(), mo = character(), start = logical())
l <- list()
for (m in 1:length(months)){  
  t <- processMonthTrip(months[m], distancePairs)
  start.station <- t %>% group_by(station.id = start.station.id) %>% 
    summarize(n = n(), mo = months[m], start = TRUE)
  #end.station <- t %>% group_by(station.id = end.station.id) %>% 
  #  summarize(n = n(), mo = months[m], start = FALSE)
  #sums.mat <- rbind(sums.mat, start.station, end.station)
  l[[length(l) + 1]] <- start.station
}

l <- lapply(l, function(x) x[complete.cases(x), ])

distance <- unique(distancePairs[, c("start.station.id", "start.station.latitude", 
                                     "start.station.longitude"), with = FALSE])
sums <- sums.mat %>% left_join(y = distance, by = c("station.id" = "start.station.id")) %>% 
          select(station.id, n, mo, start, station.latitude = start.station.latitude, 
                 station.longitude = start.station.longitude)
sums.start <- subset(sums, start = TRUE)
sums.start$mo <- as.numeric(
  paste(substr(sums.start$mo, 1, 4), substr(sums.start$mo, 6, 7 ), sep = "."))

sumList <- lapply(l, function(x) x %>% right_join(y = distance, 
                                                  by = c("station.id" = "start.station.id")) %>% 
                     select(station.id, n, mo, start, station.latitude = start.station.latitude,
                            station.longitude = start.station.longitude))
sumList <- lapply(sumList, function(x) {
  x[is.na(x$n), "n"] <- 0; 
  x[is.na(x$mo), "mo"] <- unique(x$mo)[1]
  x[is.na(x$start), "start"] <- TRUE
  x
  })
sl <- rbind.fill(sumList)
sl$station.id <- as.numeric(sl$station.id)
sl$ease = "linear"
#sl$mo <- as.numeric(paste(substr(sl$mo, 1, 4), substr(sl$mo, 6, 7 ), sep = "."))
sl$mo <- as.numeric(substr(sl$mo, 1, 4)) * 100 + as.numeric(substr(sl$mo, 6, 7 ), sep = ".")
s <- sl[, c("station.id", "n", "ease", "mo")]
colClasses <- col_classes(data) #maybe n nd mo dont have to be numeric
dt <- tween_elements(s, 'mo', 'station.id', 'ease', nframes = 500)

ds <- tween_states(sumList, 3, 2, ease="linear" , nframes=30)

mymap13 <- get_map(location = "40.72417399459069,-73.98639034958494", zoom = 13, 
                   maptype = "toner-lines")
inv <- readPNG("invert.png")
g <- ggmap(mymap13, extent = "device") + 
   inset_raster(inv, xmin = -74.048, xmax = -73.928, ymin = 40.68, ymax = 40.766) +
   geom_point(aes(x = as.numeric(station.longitude), y = as.numeric(station.latitude), size = n,
                 frame = mo),
             data = sums.start, alpha = 0.3, color = "blue") +
  #theme_nothing() + 
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
        plot.title = element_text(size = 18))

