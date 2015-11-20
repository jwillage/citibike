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
  url <- paste0(url, "&key=", readLines("privatekey.txt"))
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
  u <- paste0("https://s3.amazonaws.com/tripdata/", dt, 
              "-citibike-tripdata.zip")
  
  if (monthFile < "2014-08-02"){ #old format
    fil <- paste0("data/", year(monthFile), "-", 
                  sprintf("%02d", month(monthFile)), 
                  " - Citi Bike trip data.csv")
    
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
      unzip(paste0("data/", dt, ".zip"))   
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
  
  if (monthFile >= "2014-08-02" & monthFile < "2015-05-31"){ #second format
    tmp.trip$starttime <- mdy_hms(tmp$starttime)
    tmp.trip$stoptime <- mdy_hms(tmp$stoptime)
  }
  
  if (monthFile > "2015-05-31"){ #current format
    tmp.trip$starttime <- mdy_hm(tmp$starttime)
    tmp.trip$stoptime <- mdy_hm(tmp$stoptime)
  }
  

  setnames(tmp.trip, make.names(names(tmp.trip)))
  tmp.trip$usertype <- as.factor(tmp.trip$usertype)
  
  #setkeyv is messing up start/end stations in dat
#  setkeyv(tmp.trip, c("start.station.id", "end.station.id"))
  
  #only bring necessary columns from stationCombs file
  #check tmp.trip - looks like sorted by start and end stations?
#  fullTrip <- merge(tmp.trip, stationCombs)
  #  fullTrip <- merge(tmp.trip, stationCombs[, 
  #            c("start.station.id", "end.station.id", "start.station.name"), 
  #            with = FALSE])

  #perform any data filtering, ie searching by neighborhood, etc
  #remove trips where start station = end station if comparing to estimates
  #remove trips that were longer than 2 hours
  tmp.trip$tripduration <- as.numeric(tmp.trip$tripduration)
  #saveRDS(tmp.trip, "data/Sep13tmptrip.rds")
  tmp.trip.filt <- tmp.trip[tmp.trip$tripduration < 7200, ]
  
  #join with estimates and process at the trip level, then aggregate
  trip.month <- tmp.trip %>% 
    left_join(distancePairs, by = c("start.station.id", "end.station.id")) %>%
    select(start.station.id : gender, est.time, est.distance)

  trip.month$birth.year <- as.numeric(trip.month$birth.year)
  trip.month$tripduration <- as.numeric(trip.month$tripduration)
  
  #data frames too difficult to develop with
  trip.month <- as.data.frame(trip.month)
  
  trip.month
}

processMonthStation <- function(monthFile){
  tmp <- getMonthData(monthFile)
  
  #Split into stations
  tmp.station <- tmp[, c(4, 5, 6, 7), with = FALSE]

  #calculate all combinations of stations and the distance between them,
  #in order to map them back to each rider
  comb <- as.data.table(levels(
             interaction(paste(tmp.station$'start station id', 
                               tmp.station$'start station name',
                               tmp.station$'start station latitude', 
                               tmp.station$'start station longitude',
                               sep = ";"), 
                         paste(tmp.station$'start station id', 
                               tmp.station$'start station name',
                               tmp.station$'start station latitude', 
                               tmp.station$'start station longitude',
                               sep = ";")
                         , sep = ";")))
  
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

dat <- data.table()
stationCombs <- data.table()

#process the most recent file to get the up-to-date station list
distancePairs <- processMonthStation(tail(months, 1))
setkeyv(distancePairs, c("start.station.id", "end.station.id"))

#get trip history for all months
for (m in 1 : (length(months))){
  dat <- rbind(dat, processMonthTrip(months[m]), distancePairs)
}

setnames(dat, make.names(names(dat)))

dat$birth.year <- as.numeric(dat$birth.year)
dat$tripduration <- as.numeric(dat$tripduration)

#average duration by customer type. Divide by 60 to convert to minutes
dat[, mean(tripduration/60, na.rm = T), by = usertype]

