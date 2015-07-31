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

setwd("data/")

dat <- data.table()
stations <- data.table()

#download each file, unzip and rbind it to dat, then remove file and zip
#format of filename and dates changed starting 9/2014, so handle separately
for (m in 1:length(months)){
  dt <- paste0(year(months[m]), sprintf("%02d", month(months[m])))
  u <- paste0("https://s3.amazonaws.com/tripdata/", dt, 
              "-citibike-tripdata.zip")
#  download.file(u, paste0(dt, ".zip"))
#  unzip(paste0(dt, ".zip"))
  
  if (months[m] < "2014-08-02"){ #old format
    tmp <- fread(paste(year(months[m]), sprintf("%02d ", month(months[m])), 
                       " Citi Bike trip data.csv", sep = "-"), 
                 select = c(1, 2, 3, 4, 8))
    tmp$starttime <- ymd_hms(tmp$starttime)
    tmp$stoptime <- ymd_hms(tmp$stoptime)
    
    tmp.st <- fread(paste(year(months[m]), sprintf("%02d ", month(months[m])), 
                          " Citi Bike trip data.csv", sep = "-"), 
                    select = c(4, 5, 6, 7))
  }
  
  if (months[m] >= "2014-08-02"){ #new format
    tmp <- fread(paste0(year(months[m]), sprintf("%02d", month(months[m])), 
                        "-citibike-tripdata.csv"), 
                 select = c(1, 2, 3, 4, 8))
    tmp$starttime <- mdy_hms(tmp$starttime)
    tmp$stoptime <- mdy_hms(tmp$stoptime)
    tmp.st <- fread(paste0(year(months[m]), sprintf("%02d", month(months[m])), 
                           "-citibike-tripdata.csv"), 
                    select = c(4, 5, 6, 7))
  }
  
  #dat <- rbind(dat, read.csv(paste0(list.files(pattern = "csv"))))
  #TODO change to be less complcated
  
  #Skip over the station name and geo coordinates for now to save space
#  dat <- rbind(dat, 
#        fread(grep(paste0(".*", year(months[m]), ".*", month(months[m]), ".*"), 
#             list.files(pattern="csv"), value =T), select=c(1, 2, 3, 4, 8)))

  dat <- rbind(dat, tmp)
  stations <- rbind(stations, tmp.st)
    #danger
  #file.remove(list.files())
  
  #build station lookup table
#  stations <- rbind(stations, 
#               fread(grep(paste0(".*", year(months[m]), ".*", month(months[m]), ".*"), 
#               list.files(pattern="csv"), value =T), select=c(4, 5, 6, 7)))
#  stations <- unique(stations)
}

setwd("..")

setnames(dat, make.names(names(dat)))

#start/stop times are in different formats
dat$starttime <- sub(".*-.*", paste0(
                    month(ymd_hms(dat$starttime)), "/", 
                    day(ymd_hms(dat$starttime)), "/", 
                    year(ymd_hms(dat$starttime)), " ", 
                    hour(ymd_hms(dat$starttime)), ":", 
                    minute(ymd_hms(dat$starttime))), dat$starttime)

dat$stoptime <- sub(".*-.*", paste0(
                    month(ymd_hms(dat$stoptime)), "/", 
                    day(ymd_hms(dat$stoptime)), "/", 
                    year(ymd_hms(dat$stoptime)), " ",
                    hour(ymd_hms(dat$stoptime)), ":", 
                    minute(ymd_hms(dat$stoptime))), dat$stoptime)

dat$birth.year <- sub("\\N", NA, dat$birth.year)
dat$birth.year <- as.numeric(dat$birth.year)
dat$tripduration <- as.numeric(dat$tripduration)

#average duration by customer type. Divide by 60 to convert from sec to min
dat[, mean(tripduration/60, na.rm = T), by = usertype]