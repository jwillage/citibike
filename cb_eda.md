# cb_eda
Joe Willage  
November 18, 2015  



Given a trip month file, explore some of the properties

```r
source("cb_analysis.R") 
```

```
## Loading required package: bitops
## 
## Attaching package: 'data.table'
## 
## The following objects are masked from 'package:lubridate':
## 
##     hour, mday, month, quarter, wday, week, yday, year
## 
## The following objects are masked from 'package:dplyr':
## 
##     between, last
## 
## 
## Attaching package: 'jsonlite'
## 
## The following object is masked from 'package:utils':
## 
##     View
## 
## 
## Attaching package: 'tidyr'
## 
## The following object is masked from 'package:RCurl':
## 
##     complete
```

```r
distancePairs <- readRDS("data/distancePairs.rds")
trip.month <- processMonthTrip("2013-10-01", distancePairs)
```

```
## 
Read 31.8% of 1037712 rows
Read 53.0% of 1037712 rows
Read 69.4% of 1037712 rows
Read 85.8% of 1037712 rows
Read 1037712 rows and 15 (of 15) columns from 0.189 GB file in 00:00:06
```

```r
long <- which(trip.month$tripduration > 7200)
table(trip.month[long, ]$usertype) 
```

```
## 
##   Customer Subscriber 
##        216       2108
```

Surprisingly, we see that subscribers had bikes out for lengthy periods almost 
as much as customers. Perhaps this is due to the new-ness of the company, we
need to explore this for a later month file. 

Let's check out the remaining rows.


```r
trip.month <- trip.month[trip.month$tripduration < 7200, ]
g <- ggplot(trip.month, aes(x = tripduration/60))
g + geom_histogram(binwidth = 2, aes(fill = usertype)) + 
  facet_grid(. ~  usertype) + guides(fill = F)
```

![](figure/unnamed-chunk-2-1.png) 

Two things immediately jump out. First of all, there are a lot more subscriber 
trips than customers, in only the third month of the program. Unfortunately we 
don't have the data to compare unique riders. We can make a general assumption, 
like subscribers probably bike to work five days a week. Customers, on the other 
hand, we'll assume only make one round trip (two trips)  in a given month file. There are 
31 days in this file, 20 of them are working 
days (21 weekdays - 1 Labor Day holiday). So all things being even, we would 
expect 20 subscriber rides for every 2 customer rides, or 10 : 1. 


```r
users <- table(trip.month$usertype)
users["Customer"]
```

```
## Customer 
##    97230
```

```r
users["Subscriber"]/10
```

```
## Subscriber 
##    93815.7
```

Although it appears from the histogram that there are many more subscribers than 
customers, under these assumptions, we see that
50.89%
of this month's unique riders are customers. Since the types of users are not 
too different, we'll scale each plot's y-axis accordingly. We will split the 
plots on two rows, instead of the same row, to further emphasize the difference 
in scale. 

The second thing that jumps out in the histogram is the x-axis and all the 
seemingly blank space on the right hand side. We filtered on trips less than 120 
minutes, but there are still trips that take much longer than the median. 


```r
g <- ggplot(subset(trip.month, tripduration > 1800),
            aes(x = tripduration/60))
g + geom_histogram(binwidth = 2, aes(fill = usertype)) + 
  facet_wrap(~  usertype, scales = "free_y", nrow = 2) + guides(fill = F)
```

![](figure/unnamed-chunk-4-1.png) 

There are still thousands of trips in this tail that we don't want to ignore. 
Let's see how the log-transformed histogram of the original filtered data looks.


```r
g <- ggplot(trip.month, aes(x = log(tripduration/60)))
g + geom_histogram(aes(fill = usertype)) + 
  facet_wrap(~  usertype, scales = "free_y", nrow = 2) + guides(fill = F)
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![](figure/unnamed-chunk-5-1.png) 

Pretty normal. We'll keep this in our back pocket in case it's needed later.


```r
subs <- trip.month$usertype == "Subscriber"
upper.bound.subs <- quantile(trip.month[subs, "tripduration"], .95)
upper.bound.cust <- quantile(trip.month[-subs, "tripduration"], .95)
```

In addition to taking 95%, we'll go back to plotting on the seconds scale rather
than minutes. Binwidths are 30 second intervals, giving us a pretty smooth 
curve. 


```r
trip.month <- trip.month %>% 
  mutate(inbounds = ifelse(usertype == "Customer", 
                           tripduration < upper.bound.cust , 
                           tripduration < upper.bound.subs)) %>%
  filter(inbounds == TRUE)

subs <- trip.month$usertype == "Subscriber"

g <- ggplot(trip.month, aes(x =tripduration))
g + geom_histogram(binwidth = 30, aes(fill = usertype)) + 
  facet_wrap(~  usertype, scales = "free_y", nrow = 2) + guides(fill = F)
```

![](figure/unnamed-chunk-7-1.png) 

Now that we've got a data set that makes sense, let's dig in further. 


```r
g <- ggplot(trip.month, aes(y =tripduration/60, x = usertype))
g + geom_boxplot(aes(fill = usertype)) + guides(fill = F)
```

![](figure/unnamed-chunk-8-1.png) 

Almost identical boxplots for both user types. 


```r
rbind(summary(trip.month[subs, "tripduration"]), 
      summary(trip.month[-subs, "tripduration"]))
```

```
##      Min. 1st Qu. Median  Mean 3rd Qu. Max.
## [1,]   60     383    595 691.9     923 1850
## [2,]   60     383    594 691.5     923 1850
```

Nary a difference. Let's finally look at the trip distance estimates.


```r
g <- ggplot(trip.month, aes(y = est.distance, x = usertype))
g + geom_boxplot(aes(fill = usertype)) + guides(fill = F)
```

![](figure/unnamed-chunk-10-1.png) 

Here again we can see there's barely a difference between user types. 

Let's shift focus and explore a more recent month. 
(In March 2014, new stations were added: 491, 530). 


```r
monthFile <- "2014-03-01" 
rec.trip.month <- processMonthTrip(monthFile, distancePairs)
summary(rec.trip.month)
```

```
##  start.station.id   end.station.id      tripduration    
##  Length:439117      Length:439117      Min.   :   60.0  
##  Class :character   Class :character   1st Qu.:  349.0  
##  Mode  :character   Mode  :character   Median :  536.0  
##                                        Mean   :  715.6  
##                                        3rd Qu.:  852.0  
##                                        Max.   :21454.0  
##                                                         
##    starttime                      stoptime                  
##  Min.   :2014-03-01 00:00:16   Min.   :2014-03-01 00:03:08  
##  1st Qu.:2014-03-10 12:33:37   1st Qu.:2014-03-10 12:43:49  
##  Median :2014-03-17 11:03:56   Median :2014-03-17 11:16:50  
##  Mean   :2014-03-17 03:16:52   Mean   :2014-03-17 03:28:48  
##  3rd Qu.:2014-03-23 19:53:39   3rd Qu.:2014-03-23 20:03:45  
##  Max.   :2014-03-31 23:59:54   Max.   :2014-04-01 00:16:56  
##                                                             
##     bikeid                usertype        birth.year       gender         
##  Length:439117      Customer  : 22882   Min.   :1899    Length:439117     
##  Class :character   Subscriber:416235   1st Qu.:1967    Class :character  
##  Mode  :character                       Median :1977    Mode  :character  
##                                         Mean   :1975                      
##                                         3rd Qu.:1984                      
##                                         Max.   :1998                      
##                                         NA's   :22882                     
##     est.time         est.distance  
##  Min.   :      60   Min.   :0.000  
##  1st Qu.: 1080000   1st Qu.:0.700  
##  Median : 1728000   Median :1.100  
##  Mean   : 1927182   Mean   :1.377  
##  3rd Qu.: 2376000   3rd Qu.:1.700  
##  Max.   :11664000   Max.   :9.600  
## 
```

```r
all.stations <- unique(c(rec.trip.month$start.station.id, rec.trip.month$end.station.id))
unknown.indices <- which(!all.stations %in% distancePairs$start.station.id)
print(unknown <- all.stations[unknown.indices])
```

```
## character(0)
```

Note the NA's. We're missing some stations in our distancePairs list. 

```r
#calculate distance matrix for these missing values and append to distance.pairs

#need to put this in it's own function for anytime we want to add more station combinations
tmp <- getMonthData(monthFile)
start <- tmp[, 4:7, with = FALSE]
end <- tmp[, 8:11, with = FALSE]; names(end) <- names(start)
tmp.station <-  rbind(start, end)
unknown.full <- unique(tmp.station[tmp.station$`start station id` %in% unknown, ])
existing.full <- unique(distancePairs[, 1:4, with = FALSE])

startCombs <- as.data.table(levels(interaction(paste(unknown.full$'start station id', 
                               unknown.full$'start station name',
                               unknown.full$'start station latitude', 
                               unknown.full$'start station longitude',
                               sep = ";"),
            paste(existing.full$start.station.id,
                  existing.full$start.station.name,
                  existing.full$start.station.latitude,
                  existing.full$start.station.longitude,
                  sep = ";")
            , sep = ";"
            )))
endCombs <- as.data.table(levels(interaction(            
                paste(existing.full$start.station.id,
                  existing.full$start.station.name,
                  existing.full$start.station.latitude,
                  existing.full$start.station.longitude,
                  sep = ";"),
                  paste(unknown.full$'start station id', 
                               unknown.full$'start station name',
                               unknown.full$'start station latitude', 
                               unknown.full$'start station longitude',
                               sep = ";"),
                sep = ";"
            )))
# interaction between new station and new station
newCombs <- as.data.table(levels(interaction(paste(unknown.full$'start station id', 
                               unknown.full$'start station name',
                               unknown.full$'start station latitude', 
                               unknown.full$'start station longitude',
                               sep = ";"),
                               paste(unknown.full$'start station id', 
                               unknown.full$'start station name',
                               unknown.full$'start station latitude', 
                               unknown.full$'start station longitude',
                               sep = ";"), sep = ";")))

combs <- rbind(startCombs, endCombs, newCombs)
combs <- separate(combs, V1, c(names(tmp.station), 
                               sub('start', 'end', names(tmp.station))), 
                   sep = ";")
setnames(combs, make.names(names(combs)))

#now make the call to google api to get estimates
stationDistanceMatrix <- Vectorize(stationDistanceMatrix)
estimates <- t(with(combs, stationDistanceMatrix(start.station.latitude,
                                               start.station.longitude,
                                               end.station.latitude, 
                                               end.station.longitude)))
newDp <- cbind(combs, estimates)
names(newDp)[9:10] <- c("est.time", "est.distance")
distancePairs <- rbind(distancePairs, newDp)
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
```


```r
monthFile <- "2015-05-01"
rec.trip.month <- processMonthTrip(monthFile, distancePairs)
summary (rec.trip.month) 

unknown <- findUnknownStations(monthFile, rec.trip.month, distancePairs)
newDP <- addStations(unknown, distancePairs)
```


Let's remove the person who had their bike out for over two weeks, and any other 
suspiciously long trips. 


```r
tail(rec.trip.month[order(rec.trip.month$tripduration),]$tripduration, 100)
```

```
##   [1] 17142 17157 17235 17242 17272 17315 17336 17347 17386 17393 17494
##  [12] 17602 17622 17622 17628 17648 17653 17738 17766 17774 17778 17782
##  [23] 17790 17858 17962 17963 17993 18004 18140 18160 18239 18399 18473
##  [34] 18500 18535 18575 18786 18797 18822 18829 18830 18848 18870 18955
##  [45] 19009 19081 19163 19179 19196 19272 19289 19319 19346 19398 19433
##  [56] 19453 19469 19572 19606 19630 19640 19655 19759 19788 19874 19890
##  [67] 19892 19935 19966 20089 20128 20218 20244 20283 20388 20427 20521
##  [78] 20530 20546 20596 20649 20855 20879 20886 21147 21168 21178 21180
##  [89] 21194 21227 21254 21270 21320 21330 21332 21359 21387 21433 21437
## [100] 21454
```

The 100th longest trip isn't nearly as bad as the longest, about 16 hours vs 2 
weeks. Instead of using the 95th percentile as a cutoff point, let's see what we 
get at the 99th. It's 49.2666667 
minutes. I like that better than 95%; Not only does it give us more data, it 
brings us out of 33 minute upper bound and into the 50's, which isn't 
unreasonable since subscribers are alloted 45 minutes. 
