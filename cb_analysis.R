library(RCurl)
library(lubridate)
library(data.table)
library(jsonlite)
library(XML)
library(tidyr)
library(dplyr)

myGetURL <- function(...) {
  rcurlEnv <- getNamespace("RCurl")
  mapUnicodeEscapes <- get("mapUnicodeEscapes", rcurlEnv)
  unlockBinding("mapUnicodeEscapes", rcurlEnv)
  assign("mapUnicodeEscapes", function(str) str, rcurlEnv)
  on.exit({
    assign("mapUnicodeEscapes", mapUnicodeEscapes, rcurlEnv)
    lockBinding("mapUnicodeEscapes", rcurlEnv)
  }, add = TRUE)
  return(getURL(...))
}

stationDistance <- function(startLat, startLon, endLat, endLon){
  #cannot be vectorized, error during call of more than 1 row
  url <- "https://maps.googleapis.com/maps/api/directions/json?mode=bicycling"
  url <- paste0(url, "&origin=", startLat, ",", startLon, 
                "&destination=", endLat, ",", endLon)
  doc <- fromJSON(url)
  
  dist <- doc$routes$legs[[1]]$distance$text
  duration <- doc$routes$legs[[1]]$duration$text 
  c(dist, duration)
}

stationDistanceXml <- function(startLat, startLon, endLat, endLon){
  url <- "https://maps.googleapis.com/maps/api/directions/xml?mode=bicycling"
  url <- paste0(url, "&origin=", startLat, ",", startLon,   
                " &destination=", endLat, ",", endLon)
  
  xData <- myGetURL(URLencode(url))
  
  doc <- xmlTreeParse(xData)
  rootNode <- xmlRoot(doc)
 
  c(xmlValue(rootNode[[2]][["leg"]][["distance"]][["text"]][["text"]]), 
  xmlValue(rootNode[[2]][["leg"]][["duration"]][["text"]][["text"]]) )
}


stationDistanceMatrix <- function(startLat, startLon, endLat, endLon){
  #restricted to using google maps

  url <- "https://maps.googleapis.com/maps/api/distancematrix/xml?mode=bicycling&units=imperial"
  
  #key is optional for this api call
  #url <- paste0(url, "&key=", readLines("privatekey.txt"))
  url <- paste0(url, "&origins=", startLat, ",", startLon, 
                " &destinations=", endLat, ",", endLon)

  xData <- myGetURL(URLencode(url))
  
  doc <- xmlTreeParse(xData)
  rootNode <- xmlRoot(doc)
  
    c(xmlValue(rootNode[[4]][[1]][[2]][[2]]), 
      xmlValue(rootNode[[4]][[1]][[3]][[2]]))
}


getMonthData <- function(monthFile){
  dt <- paste0(year(monthFile), sprintf("%02d", month(monthFile)))
  #For files after Aug '15 this url is become dynamic
  u <- paste0("https://s3.amazonaws.com/tripdata/", dt, 
              "-citibike-tripdata.zip")
  
  if (monthFile < "2014-08-02"){ #old format
    fil <- paste0("data/", year(monthFile), "-", 
                  sprintf("%02d", month(monthFile)), 
                  " - Citi Bike trip data.csv")
    
    #read from zip
    if (!file.exists(fil)){
      download.file(u, paste0("data/", dt, ".zip"))
      unzip(paste0("data/", dt, ".zip"))   
      file.remove(paste0("data/", dt, ".zip"))
    }
    
    tmp <- fread(fil, na.strings = "\\N")
  }
  
  if (monthFile >= "2014-08-02"){ #second through current format
    fil <- paste0("data/", year(monthFile), sprintf("%02d", month(monthFile)), 
                  "-citibike-tripdata.csv")
    
    if (!file.exists(fil)){
      download.file(u, paste0("data/", dt, ".zip"))
      unzip(paste0("data/", dt, ".zip"), exdir = "data")   
      file.remove(paste0("data/", dt, ".zip"))
    }
    
    tmp <- fread(fil, na.strings = "\\N")
  }
  
  #danger
  #file.remove(...)
  
  tmp
}

processMonthTrip <- function(monthFile, distancePairs){
  tmp <- getMonthData(monthFile)
  
  #Split into trip history 
  tmp.trip <- tmp[, c(1:4, 8, 12:15), with = FALSE]
  
  if (monthFile < "2014-08-02"){ #old format
    tmp.trip$starttime <- ymd_hms(tmp$starttime)
    tmp.trip$stoptime <- ymd_hms(tmp$stoptime)
  }
  
  #todo look into 'truncated' argument
  if (monthFile >= "2014-08-02" & monthFile < "2015-05-31"){ #second format
    #impute months which have missing seconds (oct, dec '14, jan - march '15)
    suppressWarnings(if (is.na(mdy_hms(tmp.trip$starttime))){
      tmp.trip$starttime <- paste0(tmp.trip$starttime, ":00")
    })
    suppressWarnings(if (is.na(mdy_hms(tmp.trip$stoptime))){
      tmp.trip$stoptime <- paste0(tmp.trip$stoptime, ":00")
    })
    
    tmp.trip$starttime <- mdy_hms(tmp.trip$starttime)
    tmp.trip$stoptime <- mdy_hms(tmp.trip$stoptime)
  }
  
  #june file does not have seconds, remove 's' from function and run
  if (monthFile > "2015-05-31"){ #current format
    tmp.trip$starttime <- mdy_hms(tmp$starttime)
    tmp.trip$stoptime <- mdy_hms(tmp$stoptime)
  }

  setnames(tmp.trip, make.names(names(tmp.trip)))
  
  #perform any data filtering, ie searching by neighborhood, etc
  #remove trips where start station = end station if comparing to estimates
  #remove trips that were longer than 2 hours
  
  #join with estimates and process at the trip level, then aggregate
  tmp.trip$start.station.id <- as.character(tmp.trip$start.station.id)
  tmp.trip$end.station.id <- as.character(tmp.trip$end.station.id)
  trip.month <- tmp.trip %>% 
    left_join(distancePairs, by = c("start.station.id", "end.station.id")) %>%
    select(start.station.id : gender, est.time, est.distance)
  
  #data tables too difficult to develop with
  trip.month <- as.data.frame(trip.month)
  
  trip.month$birth.year <- as.numeric(trip.month$birth.year)
  trip.month$tripduration <- as.numeric(trip.month$tripduration)
  trip.month$usertype <- as.factor(tmp.trip$usertype)
  trip.month$gender <- as.numeric(trip.month$gender)
  
  trip.month
}

processMonthStation <- function(monthFile){
  tmp <- getMonthData(monthFile)
  
  #Split into stations
  tmp.station <- tmp[, c(4, 5, 6, 7), with = FALSE]

  #calculate all combinations of stations and the distance between them
  comb <- createCombs(as.data.frame(tmp.station), as.data.frame(tmp.station))
  
  comb <- separate(comb, V1, c(names(tmp.station), 
                               sub('start', 'end', names(tmp.station))), 
                   sep = ";")
  setnames(comb, make.names(names(comb)))
  
  #vectorize whichever function is being used
  stationDistanceMatrix <- Vectorize(stationDistanceMatrix) 
  
  df<-NULL

  numStations <- sqrt(dim(comb)[1])
  #can only make 2500 google calls a day, need to break up in smaller chunks
  #ie 7 stations at a time (7 * 330 stations = 2310, < 2500)
    for (i in 1:numStations){
      estimates <- t(with(comb[((i - 1) * numStations + 1): (i * numStations), ], 
                                 stationDistanceMatrix(start.station.latitude,
                                               start.station.longitude,
                                               end.station.latitude, 
                                               end.station.longitude)))
      df <- rbind(df, estimates)
  } 

  distancePairs <- cbind(comb, df)
  names(distancePairs)[9:10] <- c("est.time", "est.distance")
  
  distancePairs$est.time <- 60 * 
    as.numeric(sub(" min[s]*", "", distancePairs$est.time))
  distancePairs$est.distance <- sub(" mi", "", distancePairs$est.distance)
  
  #convert feet to mi
  rows.ft <- grep("ft", distancePairs$est.distance)
  distancePairs$est.distance[rows.ft] <- round(
    as.numeric(sub(" ft", "", distancePairs[rows.ft]$est.distance)) * 0.000189, 
    2)
  distancePairs$est.distance <- as.numeric(distancePairs$est.distance)
  
  saveRDS(distancePairs, file = "data/distancePairs.rds")
  
  distancePairs
}

createCombs <- function(x, y){
  as.data.table(levels(interaction(
                                paste(x[,1], x[,2], x[,3], x[,4], sep = ";"),
                                paste(y[,1], y[,2], y[,3], y[,4], sep = ";"),
                                sep = ";") ) )
}

addStations <- function(unknown, distancePairs){
  known <- as.data.frame(unique(distancePairs[, 1:4, with = FALSE]))
  startCombs <- createCombs(unknown, known)
  endCombs <- createCombs(known, unknown)
  newCombs <- createCombs(unknown, unknown)
  
  combs <- rbind(startCombs, endCombs, newCombs)
  combs <- separate(combs, V1, names(distancePairs)[1:8], sep = ";")
  setnames(combs, make.names(names(combs)))
  
  stationDistanceMatrix <- Vectorize(stationDistanceMatrix) 
  estimates <- t(with(combs, stationDistanceMatrix(start.station.latitude,
                               start.station.longitude,
                               end.station.latitude, 
                               end.station.longitude)))
 
  newDp <- cbind(combs, estimates)
  names(newDp)[9:10] <- c("est.time", "est.distance")
  newDp$est.time <- 60 * as.numeric(sub(" min[s]*", "", newDp$est.time))
  newDp$est.distance <- sub(" mi", "", newDp$est.distance)
  
  #convert feet to mi
  rows.ft <- grep("ft", newDp$est.distance)
  newDp$est.distance[rows.ft] <- round(
    as.numeric(sub(" ft", "", newDp[rows.ft]$est.distance)) * 0.000189, 
    2)
  newDp$est.distance <- as.numeric(newDp$est.distance)
  
  distancePairs <- rbind(distancePairs, newDp)
  
  distancePairs
}

findUnknownStations <- function(monthFile, monthTrip, distancePairs){
  all.stations <- unique(c(monthTrip$start.station.id, monthTrip$end.station.id))
  unknown.indices <- which(!all.stations %in% distancePairs$start.station.id)
  unknown <- all.stations[unknown.indices]
  
  tmp <- getMonthData(monthFile)
  start <- tmp[, 4:7, with = FALSE]
  end <- tmp[, 8:11, with = FALSE]; names(end) <- names(start)
  tmp.station <-  rbind(start, end)
  unknown.full <- unique(tmp.station[tmp.station$`start station id` %in% unknown, ])
  
  data.frame(unknown.full)
}

#Download trip data from Citi Bikes website. Datasets are available per month,
#begining July 2013, when the service launched.
startMonth <- "07"
startYear <- "2013"
#need to tack a day onto the date in order to use lubridate/strptime
start <- ymd(paste(startYear, startMonth, "01"))
#endMonth <- paste0(year(today()), sprintf("%02d", month(today())))
endMonth <- "09"
endYear <- "2013"
end <- ymd(paste(endYear, endMonth, "01"))

months <- seq(start, end, by = "1 month")

stationCombs <- data.table()

#process the most recent file to get the up-to-date station list
 #distancePairs <- processMonthStation(tail(months, 1))
 #setkeyv(distancePairs, c("start.station.id", "end.station.id"))

#get trip history for all months
 #for (m in 1 : (length(months))){
 #  dat <- rbind(dat, processMonthTrip(months[m]), distancePairs)
 #}

 #setnames(dat, make.names(names(dat)))

#average duration by customer type. Divide by 60 to convert to minutes
 #dat[, mean(tripduration/60, na.rm = T), by = usertype]

short <- NULL
for (i in 1 : nrow(distancePairs)) {
  d <- distm(c(as.numeric(distancePairs[i, start.station.latitude]), 
               as.numeric(distancePairs[i, start.station.longitude])),
             c(as.numeric(distancePairs[i, end.station.latitude]), 
               as.numeric(distancePairs[i, end.station.longitude])))
  short <- rbind(short, d)
}

s <- distancePairs[short[, 1] < 100, ]
s[s$est.time > 1,]
summary(s)
s319.s152 <- (distancePairs$start.station.id == 319 & distancePairs$end.station.id == 152) |
              (distancePairs$start.station.id == 152 & distancePairs$end.station.id == 319)
distancePairs[s319.s152, ]$est.time <- 3
distancePairs[s319.s152, ]$est.distance <- 0.3

s223.s345 <- (distancePairs$start.station.id == 223 & distancePairs$end.station.id == 345) |
  (distancePairs$start.station.id == 345 & distancePairs$end.station.id == 223)
distancePairs[s223.s345, ]$est.time <- 1

same <- distancePairs$start.station.id == distancePairs$end.station.id
err <- distancePairs[same, ]$est.time > 1
distancePairs[same, ][err, ]$est.time <- 1
distancePairs[same, ][err, ]$est.distance <- 0
s146.s152 <- (distancePairs$start.station.id == 146 & distancePairs$end.station.id == 152) |
              (distancePairs$start.station.id == 152 & distancePairs$end.station.id == 146)
distancePairs[s146.s152, ]$est.time <- 1
distancePairs[s146.s152, ]$est.distance <- 0.1