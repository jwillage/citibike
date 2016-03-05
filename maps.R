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
distance <- unique(distancePairs[, c("start.station.id", "start.station.latitude", 
                                     "start.station.longitude"), with = FALSE])
mymap13 <- get_map(location = "40.72417399459069,-73.98639034958494", zoom = 13, 
                   maptype = "toner-lines")
inv <- readPNG("invert.png")

### ANIMATED STATION POPULARITY
sums.mat <- data.frame(station.id = character(), n = integer(), mo = character(), start = logical())
for (m in 1:length(months)){  
  t <- processMonthTrip(months[m], distancePairs)
  start.station <- t %>% group_by(station.id = start.station.id) %>% 
    summarize(n = n(), mo = months[m], start = TRUE)
  sums.mat <- rbind(sums.mat, start.station)
}

sums <- sums.mat %>% left_join(y = distance, by = c("station.id" = "start.station.id")) %>% 
          select(station.id, n, mo, start, station.latitude = start.station.latitude, 
                 station.longitude = start.station.longitude)
sums$mo <- as.factor(sums$mo)

g <- ggmap(mymap13, extent = "device") + 
       inset_raster(inv, xmin = -74.048, xmax = -73.928, ymin = 40.68, ymax = 40.766) +
       geom_point(aes(x = as.numeric(station.longitude), y = as.numeric(station.latitude), size = n,
                      frame = mo), data = sums, alpha = 0.3, color = "blue") +
       scale_size(range = c(3, 8)) +
       theme_nothing() +
       annotate("text", label = "frame", x = -74.026, y = 40.69, size = 10) #gg_animate2 specific

gg_animate2(g, ani.width = 640, ani.height = 640)


### USER TYPE COMPARISON
sums <- data.frame(station.id = character(), usertype = factor(), n = integer(), mo = character())
for (m in 1:length(months)){  
  t <- processMonthTrip(months[m], distancePairs)
  station <- t %>% group_by(station.id = start.station.id, usertype) %>% 
    summarize(n = n(), mo = months[m])
  u <- table(t$usertype)
  ratio <-u['Subscriber']/sum(u)
  cust <- station$usertype == 'Customer'
  # normalize number of subscribers vs customers
  station[cust, ]$n <- station[cust, ]$n * (u['Subscriber'] / u['Customer'])
  sums <- rbind(sums, station)
}

station <- as.data.table(as.data.frame(sums))
top <- station %>% 
  group_by(station.id, usertype) %>% summarize(total = sum(n)) %>%
  group_by(station.id) %>% summarize(usertype, total, all = sum(total))
top$pct <- top$total/top$all
top <- as.data.table(as.data.frame(top))
top <- top[top[, .I[order(total, decreasing = TRUE)[1]], by = station.id]$V1, 
           c("station.id", "usertype", "total", "pct"), with = FALSE]
top <- as.data.frame(top)
top <- top %>% left_join(y = distance, by = c("station.id" = "start.station.id")) %>% 
  select(station.id, usertype, total, pct, station.latitude = start.station.latitude, 
         station.longitude = start.station.longitude)

g <- ggmap(mymap13, extent = "device") + 
  inset_raster(inv, xmin = -74.048, xmax = -73.928, ymin = 40.68, ymax = 40.766) +
  geom_point(aes(x = as.numeric(station.longitude), y = as.numeric(station.latitude), 
                 size = total,  color = usertype), data = top, alpha = 0.6 # use top$pct
  ) +
  scale_size(range = c(3, 8)) +
  scale_colour_manual(values=c("blue", "red")) +
  guides(size = FALSE, color = guide_legend(override.aes = list(size = 3))) +
  theme(
    legend.position = c(0.95, 0.5), 
    legend.title = element_blank(), 
    legend.key = element_rect(fill = "white"), 
    legend.text = element_text(size = 16)
  ) +
  labs(title = "Majority User Type by Station")

g
