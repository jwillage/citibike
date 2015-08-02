#prepare temp files separately but only call fread once, not in both kinds,
#read file once and split out 2 temp files

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

dat <- data.table()
stations <- data.table()

#download each file, unzip and rbind it to dat, then remove file and zip
#format of filename and dates changed starting 9/2014, so handle separately.
#format changed again in 6/2015 when seconds stopped being tracked for start/end
#times

#need to aggregate each file as it's read in - not enough memory to read
#full dataset
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
    
    #Skip over the station name and geo coordinates for now to save space
    tmp <- fread(fil, select = c(1:4, 8, 12:15), na.strings = "\\N")
                
    tmp$starttime <- ymd_hms(tmp$starttime)
    tmp$stoptime <- ymd_hms(tmp$stoptime)
    
    #build station lookup table
    tmp.st <- fread(fil, select = c(4, 5, 6, 7))
  }
  
  if (months[m] >= "2014-08-02" & months[m] < "2015-05-31"){ #second format
    fil <- paste0("data/", year(months[m]), sprintf("%02d", month(months[m])), 
                  "-citibike-tripdata.csv")
    if (!file.exists(fil)){
      download.file(u, paste0("data/", dt, ".zip"))
      unzip(paste0("data/", dt, ".zip"))   
      file.remove(paste0("data/", dt, ".zip"))
    }
    
    tmp <- fread(fil, select = c(1:4, 8, 12:15), na.strings = "\\N")
    tmp$starttime <- mdy_hms(tmp$starttime)
    tmp$stoptime <- mdy_hms(tmp$stoptime)
    tmp.st <- fread(fil, select = c(4, 5, 6, 7))
  }
  
  if (months[m] > "2015-05-31"){ #current format
    fil <- paste0("data/", year(months[m]), sprintf("%02d", month(months[m])), 
                  "-citibike-tripdata.csv")
    if (!file.exists(fil)){
      download.file(u, paste0("data/", dt, ".zip"))
      unzip(paste0("data/", dt, ".zip"))   
      file.remove(paste0("data/", dt, ".zip"))
    }
    
    tmp <- fread(fil, select = c(1:4, 8, 12:15), na.strings = "\\N")
    tmp$starttime <- mdy_hm(tmp$starttime)
    tmp$stoptime <- mdy_hm(tmp$stoptime)
    tmp.st <- fread(fil, select = c(4, 5, 6, 7))
  }
  
  dat <- rbind(dat, tmp)
  stations <- unique(rbind(stations, tmp.st))
  
    #danger
  #file.remove(list.files())

}

rm(list = c("tmp", "tmp.st"))

setnames(dat, make.names(names(dat)))


dat$birth.year <- as.numeric(dat$birth.year)
dat$tripduration <- as.numeric(dat$tripduration)

#average duration by customer type. Divide by 60 to convert from sec to min
dat[, mean(tripduration/60, na.rm = T), by = usertype]