library(lubridate)
library(data.table)
library(jsonlite)

stationDistance <- function(startLat, startLon, endLat, endLon){
  url <- "https://maps.googleapis.com/maps/api/directions/json?mode=bicycling"
  url <- paste0(url, "&origin=", startLat, ",", startLon, 
                "&destination=", endLat, ",", endLon)
  doc <- fromJSON(url)

  dist <- doc$routes$legs[[1]]$distance$text
  duration <- doc$routes$legs[[1]]$duration$text 
  
  c(dist, duration)
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
stations <- data.table()

for (m in 1:length(months)){
  dt <- paste0(year(months[m]), sprintf("%02d", month(months[m])))
  u <- paste0("https://s3.amazonaws.com/tripdata/", dt, 
              "-citibike-tripdata.zip")

  if (months[m] < "2014-08-02"){ #old format
    fil <- paste0("data/", year(months[m]), "-", sprintf("%02d", 
                  month(months[m])), " - Citi Bike trip data.csv")
    
    if (!file.exists(fil)){
        download.file(u, paste0("data/", dt, ".zip"))
        unzip(paste0("data/", dt, ".zip"))   
        file.remove(paste0("data/", dt, ".zip"))
    }
    
    tmp <- fread(fil, na.strings = "\\N")
    
    #Split into trip history and station lookup data
    tmp.trip <- tmp[, c(1:4, 8, 12:15), with = FALSE]
    tmp.station <- tmp[, c(4, 5, 6, 7), with = FALSE]
                
    tmp.trip$starttime <- ymd_hms(tmp$starttime)
    tmp.trip$stoptime <- ymd_hms(tmp$stoptime)
  }
  
  if (months[m] >= "2014-08-02" & months[m] < "2015-05-31"){ #second format
    fil <- paste0("data/", year(months[m]), sprintf("%02d", month(months[m])), 
                  "-citibike-tripdata.csv")
    
    if (!file.exists(fil)){
      download.file(u, paste0("data/", dt, ".zip"))
      unzip(paste0("data/", dt, ".zip"))   
      file.remove(paste0("data/", dt, ".zip"))
    }
    
    tmp <- fread(fil, na.strings = "\\N")
    tmp.trip <- tmp[, c(1:4, 8, 12:15), with = FALSE]
    tmp.station <- tmp[, c(4, 5, 6, 7), with = FALSE]
    
    tmp.trip$starttime <- mdy_hms(tmp$starttime)
    tmp.trip$stoptime <- mdy_hms(tmp$stoptime)
  }
  
  if (months[m] > "2015-05-31"){ #current format
    fil <- paste0("data/", year(months[m]), sprintf("%02d", month(months[m])), 
                  "-citibike-tripdata.csv")
    if (!file.exists(fil)){
      download.file(u, paste0("data/", dt, ".zip"))
      unzip(paste0("data/", dt, ".zip"))   
      file.remove(paste0("data/", dt, ".zip"))
    }
    
    tmp <- fread(fil, na.strings = "\\N")
    tmp.trip <- tmp[, c(1:4, 8, 12:15), with = FALSE]
    tmp.station <- tmp[, c(4, 5, 6, 7), with = FALSE]
    
    tmp.trip$starttime <- mdy_hm(tmp$starttime)
    tmp.trip$stoptime <- mdy_hm(tmp$stoptime)
  }
  
  dat <- rbind(dat, tmp.trip)
  stations <- unique(rbind(stations, tmp.station))
  
    #danger
  #file.remove(list.files())

}

rm(list = c("tmp", "tmp.trip", "tmp.station"))

setnames(dat, make.names(names(dat)))


dat$birth.year <- as.numeric(dat$birth.year)
dat$tripduration <- as.numeric(dat$tripduration)

#average duration by customer type. Divide by 60 to convert from sec to min
dat[, mean(tripduration/60, na.rm = T), by = usertype]

tripDistance()