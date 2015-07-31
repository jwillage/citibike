library(lubridate)
library(data.table)

#Download trip data from Citi Bikes website. Datasets are available per month,
#begining July 2013, when the service launched.

startMonth <- "07"
startYear <- "2013"
#need to tack a day onto the date in order to use lubridate/strptime
start <- ymd(paste(startYear, startMonth, "01"))
#endMonth <- paste0(year(today()), sprintf("%02d", month(today())))
endMonth <- "06"
endYear <- "2015"
end <- ymd(paste(endYear, endMonth, "01"))

months <- seq(start, end, by = "1 month")

#setwd("data/")

dat <- data.table()
stations <- data.table()

#download each file, unzip and rbind it to dat, then remove file and zip
#format of filename and dates changed starting 9/2014, so handle separately

#need to aggregate each file as it's read in - not enough memory to read
#full dataset
for (m in 1:length(months)){
  dt <- paste0(year(months[m]), sprintf("%02d", month(months[m])))
  u <- paste0("https://s3.amazonaws.com/tripdata/", dt, 
              "-citibike-tripdata.zip")

  if (months[m] < "2014-08-02"){ #old format
    fil <- paste(year(months[m]), sprintf("%02d ", month(months[m])), 
                " Citi Bike trip data.csv", sep = "-")
    if (!file.exists(fil)){
        download.file(u, paste0(dt, ".zip"))
        unzip(paste0(dt, ".zip"))   
        file.remove(paste0(dt, ".zip"))
    }
    
    #Skip over the station name and geo coordinates for now to save space
    tmp <- fread(paste(year(months[m]), sprintf("%02d ", month(months[m])), 
                       " Citi Bike trip data.csv", sep = "-"), 
                 select = c(1:4, 8, 12:15), na.strings = "\\N")
                
    tmp$starttime <- ymd_hms(tmp$starttime)
    tmp$stoptime <- ymd_hms(tmp$stoptime)
    
    #build station lookup table
    tmp.st <- fread(paste(year(months[m]), sprintf("%02d ", month(months[m])), 
                          " Citi Bike trip data.csv", sep = "-"), 
                    select = c(4, 5, 6, 7))
  }
  
  if (months[m] >= "2014-08-02"){ #new format
    fil <- paste0(year(months[m]), sprintf("%02d", month(months[m])), 
                  "-citibike-tripdata.csv")
    if (!file.exists(fil)){
      download.file(u, paste0(dt, ".zip"))
      unzip(paste0(dt, ".zip"))   
      file.remove(paste0(dt, ".zip"))
    }
    
    tmp <- fread(paste0(year(months[m]), sprintf("%02d", month(months[m])), 
                        "-citibike-tripdata.csv"), 
                 select = c(1:4, 8, 12:15), na.strings = "\\N")
    tmp$starttime <- mdy_hms(tmp$starttime)
    tmp$stoptime <- mdy_hms(tmp$stoptime)
    tmp.st <- fread(paste0(year(months[m]), sprintf("%02d", month(months[m])), 
                           "-citibike-tripdata.csv"), 
                    select = c(4, 5, 6, 7))
  }
  
  dat <- rbind(dat, tmp)
  stations <- unique(rbind(stations, tmp.st))
  
    #danger
  #file.remove(list.files())

}

rm(list = c("tmp", "tmp.st"))
setwd("..")

setnames(dat, make.names(names(dat)))


dat$birth.year <- as.numeric(dat$birth.year)
dat$tripduration <- as.numeric(dat$tripduration)

#average duration by customer type. Divide by 60 to convert from sec to min
dat[, mean(tripduration/60, na.rm = T), by = usertype]